# Atomic Orchestration — Design Doc (north star)

> **Status:** Draft / north star. No contiene prompts ejecutables, ni código de orquestador. Define la taxonomía de tareas, la atomicidad de subtareas, los context bundles (con su contenido sustantivo embebido), los verificadores y sus esquemas, los loops de diseño y review, los ejes universales de correctitud, y las reglas de escalado. El objetivo es que una pipeline NeoHaskell pueda ejecutarse mayoritariamente con modelos baratos (Haiku 4.5, Gemini 2.5/3 Flash) reservando los modelos caros (Sonnet 4.6, Gemini 3 Pro, Opus) para revisión cruzada y escalado.
>
> **No-objetivos:** redactar prompts, definir el runtime/CLI de orquestación, ni cómo se enrutan las llamadas a cada API. Eso es trabajo futuro (§12).
>
> **Audiencia primaria:** otro agente o ingeniero que va a portar este diseño a otro repo y construir la implementación. Las skills originales que sirven de fuente quedan referenciadas pero su sustancia operativa va embebida en §3 y §6.

---

## 0. Cómo leer este documento

| Si eres... | Lee primero |
|------------|-------------|
| Un agente sin contexto previo de NeoHaskell | §1 entero (§1.1–§1.12 te dan el dominio, vocabulario y motivación antes de entrar al diseño) |
| El humano que diseña la implementación | §1 → §2 → §3.1 → §5 → §6 |
| El agente que va a construir el orquestador | §5 → §6 → §7 → §8 → §9 |
| El agente que va a escribir prompt templates | §3.4 (contenido de bundles) → §5.3 (ejes) → §6 (árboles canónicos) → §7.2 (esquemas) |
| Quien va a auditar coste/eficiencia | §1.8 (motivación) → §1.11 (principios) → §2 → §8.1 → §10 |

---

## 1. Introducción y contexto

> Esta sección establece el contexto necesario para que un agente o ingeniero **sin conocimiento previo de NeoHaskell** pueda entender qué orquesta este diseño y por qué. Si ya conoces el proyecto, puedes saltar a §1.8.

### 1.1 ¿Qué es NeoHaskell?

NeoHaskell es un dialecto de Haskell pensado para ser **newcomer-friendly**: presentar la potencia del tipado estricto y la composición funcional con sintaxis y vocabulario que un desarrollador con background TypeScript/Java/C# pueda absorber sin pasar antes por la curva tradicional de Haskell.

Es un **monorepo** con varios sub-proyectos:

- **`nhcore`** — la librería core. Re-exporta primitives renombradas (`Text`, `Array`, `Result`, `Task`, `Maybe`) sobre paquetes existentes del ecosistema Haskell, oculta el Prelude estándar, y ofrece infraestructura de event-sourcing/CQRS (`EventStore`, `Command`, `Query`, `Integration`).
- **`testbed`** — aplicación de ejemplo + tests de aceptación (Hspec + Hurl).
- **`transpiler`** — diseño en curso de un transpiler `.nh → .hs`.
- **`cli`** — diseño en curso de un CLI `neo`.
- **`installer`** — installer en Rust para curl-pipe.
- **`website`** — sitio en Astro/Starlight con i18n y auto-translation.

Diferencias clave respecto a Haskell estándar (que aparecerán a lo largo de este documento):

| Concepto | NeoHaskell | Haskell estándar |
|----------|------------|------------------|
| String | `Text` | `String` / `Data.Text` |
| Lista/array | `Array` | `[]` / `Data.Vector` |
| Resultado con error | `Result error value` | `Either e a` |
| Acción con efectos | `Task err val` | `IO a` |
| `pure`/`return` | `Task.yield` | `pure` / `return` |
| Composición | `x \|> foo \|> bar` | `bar (foo x)` o `bar . foo $ x` |
| Bindings | `do let y = expr` | `let..in` o `where` |
| Pattern matching | `case x of { ... }` | en función o `case` |
| Interpolación | `[fmt\|Hello #{name}\|]` | concatenación `<>` / `++` |
| Strict por defecto | `Strict` extension global | lazy por defecto |
| Type params | `forall element result.` | `forall a b.` |

Filosofía explícita del proyecto:

1. **Least Astonishment**: las APIs deben comportarse como un dev TS/Java espera.
2. **Developer Happiness**: usar la API debe empoderar, no agobiar.
3. **Least Effort**: las operaciones comunes deben requerir código mínimo.

Performance target del runtime: **50.000 requests/segundo** sostenidos en la app de event-sourcing típica. Esto define qué partes del código son "hot path" y deben ir bajo escrutinio (`<perf_baseline>`).

### 1.2 La persona "Jess"

A lo largo de este documento aparece **"Jess"** como referencia. Jess no es un usuario real ni un colaborador — es una **persona ficticia** que sirve de norte de calidad para todas las decisiones de API y documentación:

- Junior dev en TypeScript/Java de día.
- Side projects en evenings/weekends, 15-30 minutos por sesión.
- No leerá la documentación más allá del primer scroll.
- No configurará seguridad, performance ni nada opcional.
- Tomará siempre el path of least resistance.
- Si se atasca >15 minutos, abandona.

**Toda decisión de API se evalúa contra Jess**: si el path inseguro o lento es el camino fácil, la API está mal diseñada. La **regla 15 minutos**: si Jess se atasca >15 min, es bug del API/docs, no de Jess.

Cuando este documento dice cosas como "el security review aplica el Jess test" o "el DevEx review evalúa contra Jess", se refiere a: la persona target define el techo de fricción aceptable. Si el camino seguro requiere que Jess sepa algo que no sabe → la API está rota.

### 1.3 ¿Qué es una "feature pipeline"?

En este documento, **"pipeline" no se refiere a CI/CD**. Se refiere al **workflow completo desde "tengo una idea o un bug" hasta "el cambio está mergeado en `main`"**.

Una pipeline NeoHaskell típica recorre los siguientes hitos (con variantes por categoría de tarea, ver §4 y §6):

```
                          [trigger: issue / bug / idea]
                                       |
                                       v
                          [ADR (Architecture Decision Record)]   ← decisión documentada en docs/decisions/
                                       |
                                       v
       [security review of ADR]   [perf review of ADR]   [DevEx review of ADR]
                                \      |      /
                                 [merge findings]
                                       |
                                       v
                          [revise ADR if findings]                ← LOOP hasta convergencia
                                       |
                                       v
                          [architecture design]                   ← module map, signatures
                                       |
                                       v
                          [test specification]                    ← outside-in TDD: tests primero
                                       |
                                       v
                          [tests-first scaffold]                  ← tests escritos, deben fallar
                                       |
                                       v
                          [implementation]                        ← código real
                                       |
                                       v
                          [build & test loop]                     ← iterar hasta verde
                                       |
                                       v
       [security review of impl]   [perf review of impl]
                                \      |
                                 [fix findings]                   ← LOOP hasta convergencia
                                       |
                                       v
                          [final verification 100%]               ← todos los ejes pasan
                                       |
                                       v
                          [PR creation]
                                       |
                                       v
                          [CI + bot review]                       ← GitHub Actions + CodeRabbit
                                       |
                                       v
                          [fix bot comments]                      ← LOOP
                                       |
                                       v
                          [human merge]                           ← único PAUSE gate aceptable
```

**Lo que define una pipeline en este contexto**:
- Es una secuencia de fases con dependencias y **loops de retroalimentación** (no lineal).
- Cada fase produce un **artifact concreto** (ADR markdown, architecture YAML, test spec, source code, findings YAML, PR body).
- Hay **revisiones cruzadas** (security/perf/devex) que pueden devolver findings al diseño.
- Termina con un **PR mergeado** en `main`.

Distintas categorías de tarea recorren subconjuntos distintos de este flujo (ej. un `chore(deps)` salta ADR + reviews; un `fix` trivial salta ADR pero mantiene reviews). La taxonomía está en §4.

### 1.4 ¿Qué es un ADR?

ADR = **Architecture Decision Record**. Un fichero markdown en `docs/decisions/NNNN-slug.md` que documenta una decisión arquitectónica respondiendo cuatro preguntas:

1. **¿Qué problema forzó la decisión?** (Context)
2. **¿Qué opciones había?** (Alternatives, comparación tabular)
3. **¿Qué se eligió y por qué?** (Decision, con rationale para rechazar cada alternativa)
4. **¿Qué cuesta haber elegido eso?** (Consequences — positivas, negativas, riesgos, mitigaciones)

Numeración secuencial 4-dígitos (`0001`, `0002`, ...). Status: `Proposed` (pendiente revisar) → `Accepted` (aprobado) → `Deprecated` o `Superseded by ADR-XXXX`. Solo el maintainer humano cambia status.

Un ADR **no** es:
- Un PRD (no contiene user flows).
- Un tutorial (no contiene "how to use").
- Un plan de implementación (no contiene timeline ni PR breakdown).
- Una exploración de diseño (las open questions se resuelven antes de escribir, no se difieren).

NeoHaskell tiene actualmente **55+ ADRs** en `docs/decisions/`. Ejemplos reales:
- `0022-decimal-type.md` — feature nueva (`Decimal` type para finanzas).
- `0036-wave1-security-hardening.md` — wave de fixes de seguridad (3 ciclos de revisión documentados).
- `0040-neoql-mvp.md` — feature core (query language).

Cuándo se escribe ADR: renombrar/restructurar módulos, cambiar API pública de Core, nuevas implementaciones de EventStore, cambiar flujo de command execution, infraestructura significativa.

Cuándo NO: bug fixes triviales, perf sin cambio API, docs, nuevos commands/entities (lógica de dominio).

### 1.5 Actores

Múltiples actores intervienen en una pipeline:

| Actor | Rol | Persistencia |
|-------|-----|--------------|
| **Humano (maintainer)** | Origina la tarea, aprueba ADRs, mergea PRs. Único actor con permisos de merge en `main`. | Permanente |
| **Modelos LLM (agentes)** | Ejecutan cada subtarea. Diferentes tiers según complejidad (§2). Anthropic (Haiku 4.5, Sonnet 4.6, Opus) y Google (Gemini 2.5/3 Flash, Gemini 3 Pro). | Sin estado entre invocaciones; el estado vive en disco (§9). |
| **Skills** | Paquetes de instrucciones de dominio cargables sobre un agente. En NeoHaskell viven en `.claude/skills/<name>/SKILL.md`. Este documento las materializa como **bundles** (§3) y embebe su sustancia en §3.4. | Versionados con el repo. |
| **Verificadores** | Scripts deterministas que comprueban éxito/fallo (cabal build, cabal test, hlint, doctest, hurl, cspell, lychee, grep). Ver §7. | Versionados con el repo. |
| **CI** | GitHub Actions ejecuta build + test en PRs. Linux + macOS workflows con servicio Postgres. | Externo. |
| **Bot reviewer** | CodeRabbit comenta PRs con sugerencias automatizadas. | Externo. |

### 1.6 Skills, bundles, y la diferencia entre ambos

Una **skill** (en el ecosistema Claude Code) es un archivo `SKILL.md` con frontmatter YAML que un agente puede cargar para adquirir contexto de dominio (ej. `neohaskell-style-guide` carga las 12 reglas de estilo). Las skills son la unidad de distribución/versionado.

Un **bundle** (en este documento) es una unidad de inyección al prompt: un slot XML como `<style_guide>` que se inserta en el prompt de una subtarea según declaración. Un bundle puede:

- **Materializar una skill**: `<style_guide>` ≈ contenido operacional de `neohaskell-style-guide`.
- **Combinar fragmentos**: `<event_sourcing_patterns>` mezcla extractos de `neohaskell-implementer` + ejemplos del codebase.
- **Excluir activamente**: una subtarea declara `bundles_forbidden: [<ghc_internals>]` para garantizar que el modelo no vea ese contexto y no lo derrame en su output.

Por qué importa la distinción: las skills son humano-orientadas (largas, narrativas, con ejemplos extensos); los bundles son agent-orientados (compactos, operacionales, con tamaño objetivo en tokens). §3.4 contiene los bundles ya destilados desde las skills.

### 1.7 ¿Qué problema resuelve este documento?

Es importante distinguir dos cosas que a menudo se confunden:

| Concepto | Qué define | Estado |
|----------|------------|--------|
| **Pipeline** (§1.3) | El workflow del trabajo. Qué fases, qué dependencias, qué artifacts produce cada fase. | **No-negociable** — un feature siempre necesita ADR + reviews + tests + impl + verificación + PR. |
| **Orquestación** (lo que define este documento) | Cómo se ejecuta cada paso: qué modelo, con qué contexto, con qué verificadores, con qué políticas de fallo y escalado. | Es lo que estamos rediseñando. |

Este documento define la **orquestación**. La pipeline ya existe (en el repo NeoHaskell vive como skill `neohaskell-feature-pipeline` + script `pipeline.py` con 17 fases); el problema es que su orquestación actual ejecuta todo en Opus, fragmenta mal el trabajo, y no modela los loops de revisión.

El objetivo concreto: descomponer la pipeline en **subtareas atómicas** (verificables, reversibles, idealmente single-tool-call) que modelos baratos puedan ejecutar fielmente, con verificadores deterministas que sustituyan la supervisión humana excepto en escalado (§8.3).

**Lo que un agente leyendo este doc en otro repo va a hacer con él**:

- **Si va a portar el diseño a otro repo**: lee §1 → §2 → §3 → §5 → §6 para entender el modelo conceptual; luego diseña las skills/bundles equivalentes para el dominio del nuevo repo.
- **Si va a construir el orquestador**: lee §5 → §6 → §7 → §8 → §9 → §10 para entender cómo se ejecuta; los esquemas de §7.2 son el contrato entre subtareas.
- **Si va a escribir prompt templates**: usa §3.4 (contenido sustantivo de bundles) + §5 (anatomía de hojas) + §6 (árboles canónicos) + §7.2 (esquemas de output) como contrato.

### 1.8 Motivación económica

Por qué importa abaratar la orquestación:

- **Coste por feature**: una feature típica de tipo `feat(service)` consume hoy entre 200k–1M tokens de Opus, equivalente a varios euros por feature. Multiplicado por decenas de features mensuales, es coste significativo.
- **Latencia**: un Opus que delibera ~10 minutos por fase × 17 fases ≈ horas de wall clock por feature.
- **Calidad inconsistente**: PAUSE gates humanos se usan como muleta para compensar verificadores débiles; introducen idle wait de horas/días sin retroalimentación clara.
- **Bias del modelo único**: si solo Opus revisa, sus blind spots no se detectan. Cross-vendor (Anthropic ↔ Google) los expone.
- **Escalabilidad**: para correr varias features en paralelo, el coste lineal en Opus es prohibitivo; con modelos baratos, se vuelve viable.

La hipótesis de retorno: con esta orquestación, **el 80% de las tareas reales debería completarse sin tocar Sonnet ni superior**. Sonnet/Pro quedan reservados para reviews cualificados, diagnóstico de bugs no triviales y cross-vendor review obligatorio. Opus desaparece del runtime salvo escalado de último recurso.

### 1.9 El problema con la pipeline actual
La pipeline existente (`neohaskell-feature-pipeline`, 17 fases) ejecuta **todo a través de Opus**, con phases largas y monolíticas. Esto produce tres patologías:

1. **Coste y latencia inaceptables** para tareas triviales (un `chore(deps)` consume el mismo tier de modelo que un `feat(syntax)`).
2. **Contexto excesivo por fase** — Opus recibe la totalidad del prompt aunque solo necesite el subset relativo a la fase.
3. **Pausas humanas (PAUSE gates) usadas como muleta** para compensar la falta de verificadores duros y la imposibilidad de revertir cuando una fase ha contaminado el estado.

Adicionalmente, la pipeline actual **no documenta el loop entre el ADR-architect, los reviewers de seguridad/performance y el revisor DevEx**. En la práctica los ADRs reales (ej. ADR-0036) pasaron por 3 ciclos de revisión, pero la pipeline lo trata como pasos lineales sin retroalimentación. Lo mismo ocurre con los review loops post-implementación.

### 1.10 La hipótesis
Si una tarea se descompone en **subtareas atómicas** (verificables, reversibles, idealmente single-tool-call), y los **loops de revisión** se modelan explícitamente como nodos del árbol con budget de iteraciones, entonces:

- Cada subtarea cabe en el contexto de un modelo barato.
- Cada subtarea tiene un check binario al final (compila / pasa el test / pasa hlint / no findings críticos).
- Si falla, se descarta su diff y se re-intenta — sin contaminación.
- Los loops de revisión convergen o disparan escalado, no pausan indefinidamente.
- Los gates humanos solo aparecen como **escalado** ante fallo repetido, no como sincronización por defecto.

### 1.11 Principios

| # | Principio | Implicación |
|---|-----------|-------------|
| P1 | **Verificable** | Cada subtarea termina con un check binario automatizado. Si no se puede verificar, no es una subtarea — es un grupo de subtareas mal partido. Subtareas read-only verifican el output estructurado contra un esquema. |
| P2 | **Idealmente single-tool-call** | Una subtarea típica = un `Edit` + un `Bash` de verificación, o un `Read` + un YAML emitido. Cuando esto no es posible, es señal de que la subtarea está mal acotada o requiere modelo superior. |
| P3 | **Reversible** | Toda subtarea de escritura produce un diff atómico. Si falla, el diff se descarta y el estado del repo es idéntico al pre-subtarea. Sin secuelas. |
| P4 | **Modelo barato por defecto** | Asignación inicial al tier más bajo con histórico de éxito (ver §10). Escalado solo bajo fallo. |
| P5 | **Cross-vendor review** | Si Anthropic escribió, Google revisa. Y viceversa. Mitiga sesgos del modelo y errores idiosincráticos. **Obligatorio en categorías no-triviales** (ver §4 columna "Cross-vendor"). |
| P6 | **Sin PAUSE gates por defecto** | Los humanos solo intervienen cuando el escalado lo pide explícitamente (3 fallos consecutivos o categoría no determinista). |
| P7 | **Aprendizaje persistido** | Cada subtarea append-only en `learning.jsonl` para rediseño retrospectivo. |
| P8 | **Loops convergentes con budget** | Todo loop (ADR-design, impl-review, build-test) tiene techo de iteraciones. Excederlo dispara escalado, no continuación silenciosa. |

### 1.12 Vocabulario mínimo

Términos que aparecen a lo largo del documento. La definición operativa:

| Término | Significado |
|---------|-------------|
| **Hot path** | Sección de código en el camino crítico del target 50k req/s (parsing de comandos, aplicación de eventos, ejecución de queries, persistencia). Material para `<perf_baseline>`. |
| **Strict (extension)** | Extensión GHC que hace que todos los let bindings, function arguments y record fields sean strict por defecto. Activado globalmente en NeoHaskell. Implica: `!` en fields es redundante, `~` es la única forma de opt-in lazy. |
| **NoImplicitPrelude** | Extensión GHC que oculta el Prelude estándar. NeoHaskell la activa para forzar imports desde nhcore (`Text`, `Array`, `Result`, etc.). |
| **Hspec** | Framework de testing usado por NeoHaskell. Spec-style: `describe ... do; it "..." \_ -> do ...`. Re-exportado vía módulo `Test`. |
| **Hurl** | Herramienta CLI para tests de HTTP integration. Scripts en `testbed/tests/`. |
| **hlint** | Linter de Haskell. CI lo trata como errores. |
| **Cabal** | Build tool estándar de Haskell. NeoHaskell usa Hix (Nix + Cabal). Comando típico: `cabal build all`, `cabal test <suite>`. |
| **doctest** | Tests embebidos en docstrings de Haskell. `./scripts/run-doctest`. |
| **Event-sourcing / CQRS** | Patrón arquitectónico usado por nhcore: el estado se reconstruye desde un log append-only de eventos; los **Commands** modifican (producen eventos), las **Queries** leen (read models). |
| **EventStore** | Abstracción central para persistir/leer eventos. Implementación principal: PostgreSQL con LISTEN/NOTIFY. |
| **RequestContext** | Tipo que se threadrea por todos los Commands para garantizar que se chequea autorización. Imposible de saltarse. |
| **`Redacted`** | Newtype que envuelve secrets (tokens, keys) y previene leak vía `Show`/`ToJSON`. Imprime `<REDACTED>`. |
| **`canAccess` / `canView`** | Funciones de autorización en dos fases que toda Query debe definir. `canAccess` filtra por usuario, `canView` filtra por instancia. |
| **`constEq`** | Comparación constant-time para tokens. Tiene `{-# INLINE #-}` mandatorio (sin él, GHC puede romper el constant-time). |
| **Skill** | Paquete de instrucciones de dominio cargable sobre un agente. En NeoHaskell viven en `.claude/skills/<name>/SKILL.md`. Este documento las materializa como **bundles** (§3). |
| **CodeRabbit** | Bot reviewer automatizado que comenta PRs. |
| **Atomic diff** | Cambio reversible producido por una subtarea. Si la subtarea falla, el diff se descarta y el repo vuelve al estado anterior. Ver §5.1. |
| **Bundle** | Slot XML con contenido temático (style, security, perf, etc.) que se inyecta al prompt de una subtarea según declaración. Ver §3 y §1.6. |
| **Verificador** | Check binario automatizado al final de una subtarea. Ver §7. |
| **Cross-vendor review** | Review por modelo de vendor distinto al de la implementación (Anthropic ↔ Google). Mitiga sesgos del modelo único. |
| **Loop con budget** | Nodo del árbol que itera hasta que el verificador de salida pasa o se agotan las iteraciones (en cuyo caso escala). Ver §5.2. |
| **Findings YAML** | Output estructurado de un review (security, perf): lista de issues con severity, file:line, fix recomendado. Schema en §7.2.2. |
| **Accumulator artifact** | Artifact que persiste estado entre iteraciones de un loop (ej. findings cumulativos). Permite que la iteración N+1 sepa qué intentó la N. |
| **PAUSE gate** | Intervención humana obligatoria. En este diseño, **solo** existe como último escalón (§8.3) o como gate final de merge. |
| **Verificación final 100%** | Gate después del impl review loop. All-must-pass. Cubre todos los ejes obligatorios (security, perf, devex, style, test, doc) según categoría (§5.3). |
| **Eje de correctitud** | Dimensión verificable que aplica a una categoría (security, performance, devex, style, test, doc). Ver §5.3. |
| **Hoja** | Nodo terminal del árbol. Anatomía formal en §5.1. |
| **Árbol canónico** | Plantilla de subtareas asociada a una categoría de §4. Ver §6. |

---

## 2. Lineup de modelos

| Modelo | Tier | Rol primario | Rol secundario | Cuándo NO usar |
|--------|------|--------------|----------------|----------------|
| **Haiku 4.5** | dumb | Execution: edits dirigidos, scaffolding, dep bumps, test scaffold, doc drafts, registración cabal | Self-review trivial | Diseño nuevo, parser/syntax, security, perf review, ADR drafting |
| **Gemini 2.5 Flash** | dumb | Execution paralelo (cuando Haiku va saturado), dep bumps, traducciones, link checks | Cross-vendor review barato | Razonamiento sobre tipos avanzados |
| **Gemini 3 Flash** | dumb+ | Execution con razonamiento ligero (refactor mecánico, test additions, single-edit fix, fix-findings simples) | Cross-vendor review primario para findings YAML | Diseño arquitectónico |
| **Sonnet 4.6** | smart | Reviewer cualificado (security/perf de implementación), planner de subárbol cuando la tarea no encaja en plantilla, execution arriesgada (parser, type-level), diagnóstico de bugs no triviales | Architect cuando Pro está saturado | Tareas triviales (over-kill) |
| **Gemini 3 Pro** | smart | Cross-vendor review de PRs críticas, threat-modeling, segunda opinión arquitectónica, ADR architect (interview phase) | Planner de fallback si Sonnet escala | Tareas triviales |
| **Opus** | escalado | **Último recurso**: planner cuando la pipeline no es clara (P4→escalado §8.3), o execution única tras 3 fallos en Sonnet | — | Por defecto, NUNCA en runtime |

**Regla de coste:** la pipeline debe poder completar el 80% de las tareas reales (medido contra el commit log) sin tocar Sonnet ni superior. El uso de Opus en runtime es una **señal de fallo de diseño** que dispara una entrada en el learning file.

---

## 3. Context bundles

Los bundles son slots XML mutuamente excluyentes (en la mayoría de casos) que se inyectan **declarativamente** por subtarea. Esto evita contaminación semántica (ej.: un modelo escribiendo NeoHaskell idiomático no debe ver el Prelude de GHC).

### 3.1 Catálogo de bundles

| Bundle | Propósito | Contenido (resumen, sustancia en §3.4) | Tamaño objetivo |
|--------|-----------|-----------------------------------------|-----------------|
| `<neo_prelude>` | El Prelude **visible** del usuario NeoHaskell | tipos primitivos, sintaxis, anti-patrones, imports, test conventions | ≤4k tokens |
| `<ghc_internals>` | Bridge a GHC. Solo para módulos en `core/core/` que tocan tipos crudos | type families, hidden Prelude, mtl/transformer escapes, `unsafeCoerce` | ≤6k tokens |
| `<bridge_layer>` | Código que **traduce** entre el `<neo_prelude>` y `<ghc_internals>` | wrappers, newtype boundaries, re-exports en `Core.hs` | ≤3k tokens |
| `<event_sourcing_patterns>` | Event sourcing, CQRS, multi-tenant, MCP, integraciones | `EventStore`, `CommandExecutor`, `Query`, `OutboundIntegration`, `EventVariantOf`, `RequestContext` | ≤6k tokens |
| `<test_patterns>` | Hspec idiomatic, doctest, hurl, testbed, integration scripts | shapes de `it`/`describe`, generators, fixtures, registración en cabal, suites | ≤4k tokens |
| `<style_guide>` | Reglas duras del style guide | 12 reglas operacionales + extensiones GHC | ≤2k tokens |
| `<adr_format>` | Plantilla ADR + protocolo de numeración + index | template completo, when/when-not, naming conventions | ≤2k tokens |
| `<ide_layer>` | LSP, parser, syntax, layout | shapes de `Parser`, `Layout`, TextMate grammar | ≤4k tokens |
| `<security_baseline>` | Checklist operacional de seguridad | 7 categorías + red lines + Jess test | ≤3k tokens |
| `<perf_baseline>` | Reglas duras de perf | 7 categorías + hot path budget + Strict semantics | ≤3k tokens |
| `<devex_jess>` | Persona Jess + criterios DevEx | 6 criterios + tres principios + anti-patterns | ≤2k tokens |
| `<community_voice>` | Voz de Jess para docs, PRs, release notes | tono, palabras a evitar, plantilla PR body, release note style | ≤2k tokens |
| `<qa_rubric>` | Rubrica QA-designer outside-in | input/output analysis, category patterns, gates | ≤2k tokens |

### 3.2 Reglas de mutua exclusión

- `<neo_prelude>` y `<ghc_internals>` **nunca** coexisten salvo en subtareas explícitas del `<bridge_layer>`.
- `<adr_format>` no coexiste con `<test_patterns>` (separación diseño vs implementación).
- `<security_baseline>` y `<perf_baseline>` pueden coexistir.
- `<community_voice>` no coexiste con `<event_sourcing_patterns>` (registros separados: docs vs código).

### 3.3 Declaración por subtarea
Cada hoja del árbol declara su matriz:

```yaml
bundles_required:
  - <neo_prelude>
  - <test_patterns>
bundles_forbidden:
  - <ghc_internals>      # bloqueo explícito anti-leak
bundles_optional:
  - <style_guide>        # se inyecta si quedan tokens en el budget
```

**Regla de oro:** si un bundle aparece en `forbidden`, su ausencia en el output es parte del verificador (cross-grep negativo).

### 3.4 Contenido sustantivo de cada bundle

> Esta sección es **operacional**: define qué reglas/fragmentos van dentro de cada bundle. Embebe la sustancia de las skills NeoHaskell para que un agente en otro repo pueda generar los bundles sin acceso a los archivos originales.

#### 3.4.1 `<neo_prelude>`

12 reglas duras:

| # | Regla | Correcto | Incorrecto |
|---|-------|----------|------------|
| 1 | Pipe sobre nesting | `x \|> foo \|> bar` | `bar $ foo x`, `bar (foo x)` |
| 2 | Do + let para bindings | `do let y = expr` | `let y = expr in ...`, `where y = expr` |
| 3 | Case-only para pattern match | `case x of { ... }` | Pattern match en función |
| 4 | If-then-else para Bools | `if cond then a else b` | `case cond of True -> ...` |
| 5 | Type params descriptivos | `forall element result.` | `forall a b.` |
| 6 | Imports cualificados | `import Module qualified` | `import Module` |
| 7 | nhcore antes que base | `import Array qualified` | `import Data.Vector qualified` |
| 8 | Interpolación con `[fmt\|...\|]` | `[fmt\|Hello #{name}!\|]` | `"Hello " <> name` |
| 9 | Result, no Either | `Result error value` | `Either error value` |
| 10 | Task, no IO | `Task err val` | `IO a` |
| 11 | `Task.yield`, no pure/return | `Task.yield value` | `pure value`, `return value` |
| 12 | INLINE en hot paths | `{-# INLINE fn #-}` | Falta de INLINE en función pequeña hot |

Tipos canónicos (mapa GHC → NeoHaskell): `IO a`→`Task err val`; `Either a b`→`Result error value`; `Data.Text`→`Text`; `Data.Map`→`Map`; `Data.List`/`[]`→`Array`; `Data.UUID`→`Uuid`; `Data.ByteString`→`Bytes`.

Imports: tipo unqualified, módulo qualified (`import Array (Array); import Array qualified`). GHC modules: `import Data.X qualified [as GhcX]`.

Extensiones globales: `NoImplicitPrelude`, `Strict` (todos los campos/let bindings strict; `~` para opt-in laziness; `!` redundante), `OverloadedStrings`, `OverloadedRecordDot`, `QuasiQuotes`.

Anti-patrones prohibidos: `let..in`, `where`, point-free, single-letter type params, `pure`/`return`, raw `IO`, `Either`, `++`/`<>` para concat.

Test conventions: `spec :: Spec Unit`; `\_ ->` en `it`; `value |> shouldBe expected`; `result |> shouldSatisfy Result.isOk`.

#### 3.4.2 `<style_guide>`

Mismas 12 reglas que §3.4.1 pero presentadas como **enforcement**: para cada regla, un grep negativo o positivo que un script pueda correr. Ej.:

- Regla 1: `grep -E '\$\s+\w' --include='*.hs'` debe ser ∅ en archivos tocados.
- Regla 2: `grep -E '\b(let.*\bin\b|^where\b)' --include='*.hs'` debe ser ∅.
- Regla 11: `grep -E '\b(pure|return)\s' --include='*.hs'` debe ser ∅ en código nuevo.

#### 3.4.3 `<adr_format>`

Plantilla operacional (markdown):

```markdown
# ADR-NNNN: [Título descriptivo, no categoría]

## Status
Proposed

## Context
### Current State
### Use Cases
- [caso 1]
- [caso 2]
- [caso 3]
### Design Goals
1. [goal con rationale]
### GitHub Issue
- [#NNN: Título](url)

## Decision
### 1. [Decisión 1]
| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Option A | Rejected | [por qué] |
| Option B | **Chosen** | [por qué] |

### N. Type Definitions
```haskell
-- ...
```

### N+1. Public API
```haskell
-- signatures only
```

## Consequences
### Positive
### Negative
### Risks
### Mitigations

## References
```

Reglas:
- Numeración secuencial 4 dígitos. `ls docs/decisions/*.md | tail -1` + 1.
- Slug kebab-case del concepto, no de la categoría: `decimal-arithmetic-operations` ✅, `new-type` ❌.
- Status siempre `Proposed` al crear; solo el maintainer cambia a `Accepted`.
- Comparación tabular obligatoria por cada decisión no trivial. **Una decisión sin alternativa rechazada no es una decisión** — interview again.
- Cada bullet de Consequences concreto. "Better performance" ❌; "avoids one allocation per event in the hot path" ✅.
- No bodies de implementación salvo cuando el body **es** la decisión (ej. algoritmo).
- No "TBD" dentro del ADR. Decide o márcalo out-of-scope explícitamente.
- Tras crear, añadir fila a `docs/decisions/README.md`: `| [NNNN](NNNN-slug.md) | Title | Proposed |`.

When to write ADR: renombrar/restructurar módulos, cambiar API pública de Core, nuevas implementaciones de EventStore, cambiar flujo command execution, cambiar style conventions, infraestructura significativa.

When NOT: bug fixes (salvo arquitectónicos), perf sin cambio API, docs, nuevos commands/entities (lógica de dominio).

#### 3.4.4 `<security_baseline>`

7 categorías de checklist (cada finding rated Critical/High/Medium/Low):

1. **Sensitive data protection**: tipos con secrets envueltos en `Redacted` O con `Show` hand-written que imprime `<REDACTED>`. Nunca `deriving (Show)` en types con secret fields. Minimal `Redacted.unwrap`. No secrets en `[fmt|...|]`. No raw secrets en error types.
2. **Authorization enforcement**: queries definen ambos `canAccess` Y `canView`. `publicAccess` solo con justificación comentada. Commands chequean `RequestContext`.
3. **Input validation**: parse-don't-validate. Numeric inputs bounds-checked. Text inputs sanitized para paths/URLs/queries. `Decider.reject` messages no exponen internos.
4. **Error message safety**: no stack traces, connection strings, SQL fragments, credentials/tokens. HTTP errors usan mensajes genéricos para 401/403/500.
5. **Cryptographic safety**: tokens/nonces de `Crypto.Random` (NUNCA `System.Random`). Token comparison con `constEq` (NUNCA `==`). `constEq` retiene `{-# INLINE #-}`. HMAC keys ≥32 bytes.
6. **SQL/data access**: TODAS las queries con Hasql `Statement` + typed `Encoders`. Nunca string concat. Entity names y stream IDs sanitized.
7. **Unsafe usage**: nuevo `unsafePerformIO` con `{-# NOINLINE #-}` y comment de seguridad. Nuevo `unsafeCoerce` con comment probando type compatibility.

Red lines (NEVER): requerir a Jess decisiones de seguridad; añadir config opcional; aceptar `deriving (Show)` con secrets; aceptar `publicAccess`+`publicView` sin justificación; `==` para tokens; `unsafe*` sin safety comments; SQL string concat; `System.Random` para crypto; raw secrets en logs/errors/`fmt`.

Output template (see §7.2.2): findings YAML con file:line, severity, category, finding, fix.

Jess test (heuristic): "¿podría Jess tomar el camino INSEGURO siguiendo el path of least resistance?" Si sí → la API es insegura por diseño, no por uso.

#### 3.4.5 `<perf_baseline>`

Target: 50,000 req/s. Hot path budgets:

| Path | Budget |
|------|--------|
| Command intake (parse→validate→decide→persist) | <1ms |
| Event application (load→fold→return) | <0.5ms |
| Query execution (read→serialize) | <0.2ms |
| Event persistence | <1ms |

Compiler context (CRITICAL): `Strict` extension global → todos los fields/lets strict. **No** chequear ni recomendar `!` en records (redundante). **No** recomendar `foldl'` (nhcore's `foldl` IS `foldl'`). SÍ chequear `~` annotations (potential space leaks). `NoImplicitPrelude` global → no `Prelude.foldl` lazy, no `String`, no `[]`.

7 categorías de checklist (Blocking/Advisory):

1. **INLINE pragmas**: nuevas funciones públicas hot-path con `{-# INLINE fn #-}`. Helpers <10 líneas inlined. NO INLINE en funciones >25 líneas. No remover INLINE existente sin benchmark.
2. **UNPACK en primitive fields**: types hot-path con `Int`/`Word`/`Double`/`Int64` usan `{-# UNPACK #-}`. NO en polymorphic fields. Ej.: `entityId :: {-# UNPACK #-} !Uuid`.
3. **Laziness opt-outs**: no `~` en hot-path modules sin justificación. No `~` en entity state o event streams.
4. **Serialization**: types hot-path definen `toEncoding` (no solo `toJSON`). `toEncoding = GhcAeson.genericToEncoding GhcAeson.defaultOptions`.
5. **Allocation en hot paths**: no `[fmt|...|]` en tight loops. No `Array.map f \|> Array.map g` (fusionar). No constantes reconstruidas en loops. No `Text.pack`/`Text.unpack` round-trips.
6. **Concurrency**: `ConcurrentMap` no en high-contention. `Channel` ops no bloquean event loop. STM transactions cortas (no IO dentro).
7. **SPECIALIZE**: para funciones polimórficas hot-path >25 líneas: `{-# SPECIALIZE processCommand :: MyCommand -> Task CommandError MyEntity #-}`.

Red lines (NEVER): requerir a Jess añadir annotations; sugerir "high performance mode" config; advisar "use strict fields" (Strict ya lo hace); advisar `foldl'`; añadir `!` sin `UNPACK`; remover INLINE sin benchmark; `~` en hot path sin justificación; aceptar "good enough".

Output template (§7.2.3): findings YAML con file:line, severity (Blocking/Advisory), category, finding, fix.

#### 3.4.6 `<devex_jess>`

Jess persona: junior dev TS/Java, 15-30 min/día, no lee docs, autocomplete-driven. **Regla 15-min**: si Jess se atasca >15 min, es bug del API/docs, no de Jess.

Tres principios:
1. **Least Astonishment**: APIs como Jess espera. Sin sorpresas Haskell.
2. **Developer Happiness**: usar el API empodera, no agobia.
3. **Least Effort**: operaciones comunes con código mínimo, cero boilerplate.

6 criterios checklist:

1. **Naming**: funciones = verbos (`create`, `validate`); tipos = nombres (`User`, `OrderId`); sin abreviaturas; vocabulario de dominio; consistente entre módulos.
2. **Pipe-friendliness**: subject primero, transforms después. `user |> User.validate |> User.save`. No funciones con múltiples "primary" args.
3. **Discoverability**: exports organizados (Types, Construction, Operations); funciones relacionadas en mismo módulo; autocomplete natural; sin funcionalidad oculta.
4. **Error messages**: describen el problema en términos del usuario; sugieren fix; tipo específico (no `Text` genérico); no leak de internos.
5. **Defaults**: sensibles para opcionales; uso común = código mínimo; power-user options disponibles pero no obligatorios.
6. **Consistency**: signatures similares entre módulos; `Result error value` para errors; `Task` para async; nombres siguen patrones nhcore.

Anti-patterns: `validateAndCreate` (split); `processUserData` (vago); `userOrError` (use `Result`); config como first arg (rompe pipe); `Text` como error type; boolean blindness (`processUser True False`).

Red lines (NEVER): jargon Haskell en APIs (no `monoid`, `functor`); funciones que requieren docs para uso básico; naming inconsistente; "power user" como default path; errors que culpan al usuario o exponen internos.

Test (Jess at 10 PM): "¿si Jess encuentra esto a las 22h tras día largo, con 20 min antes de dormir, se siente empoderada o frustrada?". Si hay duda → revisar.

#### 3.4.7 `<test_patterns>`

Hspec en NeoHaskell:

```haskell
module MyModuleSpec (spec) where

import Core
import Test

spec :: Spec Unit
spec = do
  describe "MyModule" do
    describe "functionName" do
      it "describes expected behavior" \_ -> do
        input |> MyModule.functionName |> shouldBe expectedOutput
```

Reglas: `spec :: Spec Unit`; `\_ ->` en `it`; pipes en assertions; `Result.isOk`/`Result.isErr` con `shouldSatisfy`; `Test.fail [fmt|msg|]` para custom failures; tests IMMUTABLE.

Suites:

| Suite | Scope | Postgres | Discovery |
|-------|-------|----------|-----------|
| `nhcore-test-core` | Text, Array, Result, Decimal, Redacted | No | `hspec-discover` |
| `nhcore-test-auth` | Auth, JWT, OAuth2 | No | `hspec-discover` |
| `nhcore-test-service` | EventStore, Commands, Queries | Yes | Manual en `core/test-service/Main.hs` |
| `nhcore-test-integration` | Integrations | Yes | Manual |
| `nhcore-test` | Todo | Yes | — |

Registración: nuevos `*Spec.hs` en `core/test/` para hspec-discover. Para `nhcore-test-service`: importar y añadir a `Hspec.describe` list en `Main.hs`. Todos los modulos test en `other-modules` de `core/nhcore.cabal`.

Acceptance: Hurl scripts en `testbed/tests/` corren contra testbed levantado (`cabal run nhtestbed &`).

#### 3.4.8 `<qa_rubric>`

Outside-in TDD: tests primero, implementación después. El spec es el contrato comportamental.

Cobertura obligatoria por feature: happy paths, edge cases, error conditions, serialization round-trips, property-based invariants, concurrency (cuando aplique), integration points.

Input analysis (per parámetro):

```
What is T's zero/empty value?         → Test it
What is T's minimum boundary?         → Test it
What is T's maximum boundary?         → Test it
What is T's one-past-maximum?         → Test it (expect error)
T's special values?                   → Test each (NaN, Infinity, 0, -1, maxBound)
Can T contain unicode/special chars?  → Test it
```

Output analysis (per función):

```
What does success look like?          → Happy path
What does failure look like?          → Error condition
All error constructors?               → One test per constructor
Is R serializable?                    → Round-trip test
```

Category patterns:
- **Collections**: empty, single, many, duplicates, neg index, out-of-bounds, length post-op.
- **Text/String**: `""`, single char, unicode, very long, whitespace, special (`\0`, `\\`), injection.
- **Numeric**: 0, 1, -1, maxBound, minBound, overflow, /0, NaN/Infinity.
- **Serialization**: round-trip, malformed, missing fields, extra fields, wrong types, null, empty.

Quality gates:
- Cada función pública ≥3 test cases (happy + edge + error).
- Cada error constructor con ≥1 test.
- Cada tipo serializable con round-trip.
- Empty/zero tested para toda función que los acepte.
- Boundary tested para todo numeric.
- Total test count documentado.
- Mínimo 3:1 edge-to-happy ratio.

Anti-patterns: testing implementation details; `shouldBe True`/`False` (use `shouldSatisfy Result.isOk`); missing error case tests; only happy paths; testing private functions.

#### 3.4.9 `<event_sourcing_patterns>`

Patrones canónicos:

**New Command**:
```haskell
data MyCommand = MyCommand { entityId :: Uuid, someField :: Text }
  deriving (Eq, Show, Generic)

instance Json.ToJSON MyCommand
instance Json.FromJSON MyCommand

command "MyCommand" ''MyCommand ''MyEntity [ 'someEndpoint ]

decide :: MyCommand -> Maybe MyEntity -> RequestContext -> Decision MyEvent
decide cmd entity _ctx =
  case entity of
    Just _ -> Decider.reject "Entity already exists"
    Nothing -> Decider.acceptNew [MyEventCreated { entityId = cmd.entityId }]
```

**New Entity**:
```haskell
data MyEntity = MyEntity { entityId :: !Uuid, someField :: !Text }
  deriving (Eq, Show, Generic)

data MyEvent
  = MyEventCreated { entityId :: Uuid }
  | MyEventUpdated { someField :: Text }
  deriving (Eq, Show, Generic)

type instance EntityOf MyCommand = MyEntity
type instance EventOf MyEntity = MyEvent
```

Pitfalls: orphan instances (definir con el tipo); TH staging (splices DESPUÉS de definiciones referenciadas); cabal entries faltantes (nuevos modules en `nhcore.cabal`); `NoImplicitPrelude` (importar todo de nhcore).

#### 3.4.10 `<community_voice>`

Tono: friendly, encouraging, empathetic, never condescending.

Words to avoid:

| Avoid | Use instead |
|-------|-------------|
| "Simply" | [just explain] |
| "Obviously" | [remove] |
| "Just" | [be specific] |
| "Monad" | "action" or "Task" |
| "Functor" | "mappable" or behavior |
| "Type class" | "capability" or specific name |
| "Lambda" | "anonymous function" |

PR body template:

```markdown
## Summary
[1-3 sentences user-facing. What can Jess DO now.]

Closes #[ISSUE_NUMBER]

## Changes
- [Change 1 — what and where]
- [Change 2 — what and where]

## Release Note
[Una línea. Lenguaje user-facing.]

## Checklist
- [x] ADR created (ADR-NNNN)
- [x] Security review passed
- [x] Performance review passed
- [x] Tests written and passing ([N] tests)
- [x] hlint clean
- [ ] CodeRabbit review addressed
```

Release note style: empezar con verbo de acción (Added/Fixed/Improved); user benefit, no implementation; una sola oración; tipo/módulo en backticks; beneficio concreto al final.

Buenos: "Added `Decimal` type for precise financial calculations — no more floating-point surprises with money."
Malos: "Implemented ADR-0022", "Refactored internal serialization pipeline".

#### 3.4.11 `<ghc_internals>` y `<bridge_layer>`

Estos bundles solo se inyectan en módulos de `core/core/` que tocan tipos crudos. Contenido específico al repo (referencia: `core/core/Task.hs`, `core/core/Result.hs`, `core/core/Array.hs`). Cuando se necesiten, especificar en trabajo futuro.

Como heurística operativa para el orquestador: `<ghc_internals>` se inyecta solo si el path de file tocado matchea `core/core/.*\.hs$` Y la subtarea declara `requires_ghc_bridge: true`.

#### 3.4.12 `<ide_layer>`

Para `feat(syntax)`, `feat(parser)`, `feat(layout)`, `feat(ide)`. Contiene shapes de `Parser`, `Layout`, TextMate grammar, LSP message types. Estos son repo-specific; especificar al portar.

---

## 4. Taxonomía de tareas

Categorías derivadas del commit log real de `main` (últimos ~60 commits no-merge). El porcentaje es orientativo y se ajustará con el learning file.

| Categoría | Prefijo commit | % aprox | Ejemplo real | Modelo objetivo (P4) | ADR? | Cross-vendor §5? |
|-----------|----------------|---------|--------------|----------------------|------|--------------------|
| **Feature service** | `feat(service)` | ~13% | #603 multi-tenant, #599 MCP STDIO, #589 SHA-256 dedup | Gemini 3 Flash → Sonnet | Sí | Obligatorio |
| **Feature core** | `feat(core)` | ~8% | #586 `EventVariantOf`, #448 NeoQL MVP | Sonnet | Sí | Obligatorio |
| **Feature syntax/parser** | `feat(syntax)` `feat(parser)` `feat(layout)` | ~12% | #569 Function parsing, #494 Parser combinator | Sonnet (alta dificultad) | Sí | Obligatorio (Pro) |
| **Feature integration** | `feat(integration)` | ~5% | #568 `Integration.Agent`, #459 audio | Gemini 3 Flash | Sí | Obligatorio |
| **Feature IDE/tooling** | `feat(ide)` `feat(transpiler)` | ~7% | #485 VSCode, #565 TextMate, #487 LSP | Haiku 4.5 | Opcional | Opcional |
| **Bugfix core** | `fix(core)` | ~5% | #574 re-throw async exceptions | Sonnet | A veces | Obligatorio |
| **Bugfix layer** | `fix(service)` `fix(integration)` `fix(schema)` `fix(http)` `fix(eventstore)` | ~10% | #570 cmd logs, #436 TLS 1.2+ | Gemini 3 Flash | A veces | Obligatorio |
| **Bugfix test (flake)** | `fix(test)` | ~3% | #445 flaky high-frequency event | Sonnet (diagnóstico) | No | Opcional |
| **Dependency bump** | `chore(deps)` | ~18% | #618 npm group, #594 nix action | Haiku 4.5 (trivial) | No | Opcional |
| **Test additions puras** | `test:` | ~3% | #619 `core/Text.hs` test suite | Haiku 4.5 | No | Opcional |
| **Docs** | `docs:` | ~5% | #608 draft, #597 ADR statuses | Haiku 4.5 | No | Opcional |
| **Perf** | `perf:` | ~2% | #442 auth optimization | Sonnet | A veces | Obligatorio |
| **Security hardening (wave)** | `fix:` Wave N | ~3% | #431 W1, #574 close gaps | Sonnet + Pro reviewer | Sí | Obligatorio (doble) |
| **AI/skill tooling** | `ai:` | ~2% | #609 ADR architect, #620 trim context | Sonnet | No | Obligatorio |
| **CI/infra** | `ci:` `chore:` (no deps) | ~2% | #441 build-once-test-many, #425 monorepo | Sonnet (riesgo medio) | A veces | Obligatorio |
| **Translation auto** | `[translation]` | ~2% | auto-translate docs | Gemini 2.5 Flash (paralelo) | No | Opcional |

Cada categoría mapea a uno de los **árboles canónicos** de §6. Categorías compuestas (ej. `feat(core)` + `test:`) se descomponen en dos subárboles paralelos con punto de unión en la verificación final.

---

## 5. Anatomía y semántica

### 5.1 Anatomía de una hoja

Una hoja del árbol es **la unidad atómica de ejecución**. Toda hoja tiene la siguiente forma:

```
hoja:
  id: <ruta jerárquica única, ej. "feature.adr-loop.iter-2.review-security">
  type: leaf
  model: <tier-and-vendor, ej. "gemini-3-flash">
  bundles_required: [...]
  bundles_forbidden: [...]
  bundles_optional: [...]
  inputs:                         # qué consume
    - artifact: <path|reference>  # ej. "{adr_path}", "{architecture_path}"
      role: read-only             # read-only | will-modify
  outputs:                        # qué produce
    - artifact: <path>            # ej. ".pipeline/findings/security-adr-{iter}.yaml"
      schema: <schema_id>         # ej. "findings-yaml-v1"
      reversibility: <atomic-diff|none>
  verifier:
    type: <verifier_id>           # ej. "findings-yaml-schema"
    success_criteria: <expression sobre el output>
  fail_policy:
    on_verifier_fail: <retry|escalate|abort>
    max_retries_same_model: 2     # antes de escalar al siguiente tier
  budget:
    tokens_in: <hint, no enforcement>
    tokens_out: <hint>
    wall_clock_ms: <hint>
```

#### 5.1.1 Roles del verificador

Hay **tres clases** de verificador, según qué genera la hoja:

| Clase | Cuándo aplica | Verificador | Ejemplo |
|-------|---------------|-------------|---------|
| **Diff verifier** | Hoja modifica código fuente | Toolchain check (compile, test, hlint, doctest) | `cabal-build-ok` después de un Edit en `core/decimal/Decimal.hs` |
| **Schema verifier** | Hoja produce un artifact estructurado (YAML, markdown) | JSON schema / YAML schema match | `findings-yaml-schema` valida que el output de security-review tenga campos `severity`, `file`, `line`, `category`, `finding`, `fix` |
| **Cross-grep verifier** | Hoja debe garantizar ausencia de algo | grep negativo en el diff o el output | `no-secret-leak`: grep de la lista de secrets conocidos en el diff debe ser ∅ |

Los verificadores **invariantes**: no se relajan ante fallos. Si el modelo no los pasa, escala — no se debilita el check.

#### 5.1.2 ¿Qué verifica una hoja "read-only"?

Las hojas de descubrimiento (`§6.1.0.discovery.*`) y spec (`§6.1.1.spec.*`) son **read-only** (no producen diff). El verificador es siempre del tipo "schema verifier": el modelo emite un YAML/markdown estructurado, y el verificador comprueba:

1. **Schema match**: los campos requeridos están presentes con los tipos correctos.
2. **Referencias válidas**: si el output cita file:line, esos paths existen.
3. **Cobertura mínima**: ej. en discovery, el output debe enumerar **todos** los símbolos públicos del módulo target (chequeo: `Public API enumerated count >= grep "^[a-z].*::" target_module count`).
4. **Bundle compliance**: el output no contiene nada del bundle `forbidden` (ej. discovery con `<ghc_internals>` forbidden no debe mencionar `unsafeCoerce`).

**Esto es lo que hacía falta clarificar:** los verificadores no son decoración. En las hojas read-only convierten un texto libre en un artifact con contrato — y ese contrato es lo que las hojas downstream consumen.

#### 5.1.3 Ejemplo concreto: `discovery.read-related-modules`

```yaml
id: feature.discovery.read-related-modules
model: haiku-4.5
bundles_required: [<neo_prelude>]
bundles_forbidden: [<ghc_internals>]
inputs:
  - artifact: "{module_path}"
    role: read-only
  - artifact: "{related_modules_glob}"
    role: read-only
outputs:
  - artifact: ".pipeline/discovery/related-modules.yaml"
    schema: discovery-summary-v1
verifier:
  type: schema-verifier
  schema: discovery-summary-v1
  success_criteria:
    - field "modules" non-empty array
    - field "modules[].path" matches existing files
    - field "modules[].public_symbols" enumerates all top-level signatures
    - field "modules[].dependencies" lists imports
```

Schema `discovery-summary-v1` (YAML):

```yaml
modules:
  - path: <string>                           # required, must exist
    public_symbols:                          # required, non-empty
      - name: <string>
        signature: <string>
        kind: function|type|typeclass|instance
    dependencies:                            # required
      - module: <string>
        kind: nhcore|external|local
    summary: <string, ≤500 chars>            # required
```

### 5.2 Semántica de loops

Los loops son nodos no-hoja con tipo `loop` y la siguiente forma:

```
nodo_loop:
  id: <ruta>
  type: loop
  iteration_body: <subárbol>     # se ejecuta por iteración
  max_iterations: <int>          # techo duro
  exit_condition:
    verifier: <verifier_id>      # ej. "no-blocking-findings"
    success_criteria: <expr>
  on_budget_exhausted: escalate  # ver §8.3
  iteration_state:
    accumulator_artifact: <path> # ej. ".pipeline/findings/cumulative.yaml"
                                 # ← aquí se acumulan findings entre iteraciones
                                 # para que la siguiente revisión sepa qué se ya intentó
```

Reglas:

1. **Cada iteración es un subárbol independiente** — sus hojas siguen el modelo del §5.1.
2. **El `iteration_state.accumulator_artifact`** se pasa como input read-only a las hojas de la siguiente iteración. Esto permite que un fix-findings en iteración N sepa qué se intentó (y falló) en iteraciones <N.
3. **La verificación de salida del loop es externa al body** — un verificador adicional fuera del body comprueba si las condiciones de éxito se cumplen. Si sí, el loop termina; si no, otra iteración (hasta `max_iterations`).
4. **Excedido `max_iterations`** → `on_budget_exhausted: escalate` dispara el path §8.3 (cambio de modelo o pause gate).
5. **Reversibilidad**: si el loop entero falla y escala a humano, los diffs producidos por iteraciones anteriores quedan en estado "tentativo" — el humano decide consolidar o descartar.

### 5.3 Ejes universales de correctitud

Toda categoría de tarea declara qué **ejes de correctitud** debe satisfacer. Cada eje tiene una skill/bundle source, una serie de verificadores, y un punto en el árbol donde se aplica.

| Eje | Source bundle | Verificadores | Aplica a (categorías) | Cuándo escala |
|-----|----------------|---------------|------------------------|----------------|
| **Security correctness** | `<security_baseline>` | `security-baseline-grep` (cross-grep), `security-findings-schema` (schema), `security-review-skill` (cross-model) | feat(*), fix(*), perf, refactor (si toca auth/http), security, ai (si runtime), ci (si CI tokens) | Critical o High finding |
| **Performance correctness** | `<perf_baseline>` | `perf-baseline-grep`, `perf-findings-schema`, `perf-review-skill`, `numeric-baseline`, `numeric-improvement` | feat(*), fix(*), perf, refactor (si hot path) | Blocking finding |
| **DevEx correctness** | `<devex_jess>` | `devex-checklist-schema`, `jess-test-yaml`, `naming-grep` (anti-patterns) | feat(*), refactor (si API), docs (si API-facing) | Failed Jess test (any criterion) |
| **Style correctness** | `<style_guide>` | `grep-anti-patterns`, `hlint-clean` | TODA tarea que toque `*.hs` | Cualquier violación de las 12 reglas |
| **Test correctness** | `<test_patterns>`+`<qa_rubric>` | `tests-exist-for-pub-api`, `edge-happy-ratio`, `error-constructors-covered`, `cabal-test-ok` | feat(*), fix(*), refactor, test, perf | Coverage < threshold por categoría |
| **Doc correctness** | `<community_voice>` | `release-note-exists`, `pr-body-schema`, `link-check`, `spell-check`, `jess-language-grep` | feat(*) (release note), fix con cambio API, docs | Falta release note user-facing, link roto |
| **Style anti-Haskell-jargon** | `<community_voice>` | `jargon-grep` (no `monoid`, `functor` en exports) | API pública nueva, docs | Hay jargon sin alias |

#### 5.3.1 Mapa categoría → ejes obligatorios

| Categoría | Sec | Perf | DevEx | Style | Test | Doc |
|-----------|-----|------|-------|-------|------|-----|
| feat(service) | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| feat(core) | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| feat(syntax/parser) | — | ✓ | ✓ | ✓ | ✓ | ✓ |
| feat(integration) | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| feat(ide/tooling) | — | — | ✓ | (TS/JS si aplica) | ✓ | ✓ |
| fix(core) | ✓ | ✓ | (si API change) | ✓ | ✓ | (si API change) |
| fix(layer) | ✓ | ✓ | — | ✓ | ✓ | — |
| fix(test) | — | — | — | ✓ | ✓ | — |
| chore(deps) | (si dep crypto/auth) | (si dep hot-path) | — | — | (smoke) | — |
| test: | — | — | — | ✓ | ✓ | — |
| docs: | — | — | — | (si código en docs) | — | ✓ |
| perf: | ✓ | ✓ | — | ✓ | ✓ | (si user-visible) |
| security wave | ✓ | ✓ | (si API) | ✓ | ✓ | ✓ |
| ai/skill | ✓ (si runtime) | — | ✓ (DX del agente) | — | (skill eval) | ✓ |
| ci/infra | ✓ | ✓ (si build hot) | — | — | (smoke) | (si user-visible) |
| translation | — | — | — | — | — | (link check) |

**Cómo se inyecta cada eje en el árbol:**

- Cada eje obligatorio añade **una hoja específica** al subárbol "verificación final" de la categoría. Ej.: para `feat(service)` la verificación final tendrá 6 hojas, una por cada eje obligatorio.
- Cada eje obligatorio añade **un loop de revisión** al árbol cuando aplica review (security, perf, devex). Ver §6.1 para el caso completo.
- Las hojas de verificación de eje son cross-vendor cuando la categoría lo exige (ver tabla §4).

---

## 6. Árboles canónicos

Convenciones de notación para cada hoja:
- **M:** modelo objetivo (`haiku` / `flash` / `flash3` / `sonnet` / `pro` / `opus`).
- **B:** bundles requeridos.
- **V:** verificador (binario, automatizado).
- **R:** reversibilidad (`atomic-diff` por defecto; `none` si la subtarea es read-only).
- **F:** política de fallo si el verificador no pasa (ver §8).
- **A:** artifact producido (cuando aplica).
- **I:** inputs read-only consumidos (cuando aplica).

Un nodo no-hoja es un grupo (`group`/`parallel`/`loop`) sin verificador propio; su éxito se deriva del éxito de sus hijos según la semántica del nodo (§5.2 para loops).

### 6.1 Feature (canónico, expandido con loops)

```
0. trigger                                                  [group]
   0.1 read GitHub issue / user description                 M:haiku  B:-                                       V:structured-summary    R:none A:.pipeline/trigger.yaml
   0.2 detect category & subcategory                        M:haiku  B:-                                       V:enum-match            R:none A:.pipeline/category.yaml

1. ADR-DESIGN-LOOP                                          [loop, max_iterations=4, accumulator=.pipeline/findings/adr-cumulative.yaml]
   exit_condition: "no-blocking-findings AND devex-checklist-pass"
   iteration_body:
     1.a architect-interview-or-revise                      M:pro    B:<adr_format>+<style_guide>+<event_sourcing_patterns>+<devex_jess>
                                                                                                                V:adr-schema-valid       R:atomic-diff
                                                                                                                A:docs/decisions/NNNN-slug.md
                                                                                                                I:.pipeline/trigger.yaml, accumulator (iter ≥ 2)
       (en iter=1: entrevista al usuario o lee issue + escribe draft inicial)
       (en iter≥2: lee findings YAML del accumulator y revisa ADR)

     1.b parallel-review                                    [parallel]
       1.b.1 security-adr-review                            M:flash3 B:<security_baseline>+<adr_format>          V:findings-yaml-schema  R:none
                                                                                                                 A:.pipeline/findings/security-adr-iter{N}.yaml
                                                                                                                 I:docs/decisions/NNNN-slug.md
       1.b.2 performance-adr-review                         M:flash3 B:<perf_baseline>+<adr_format>              V:findings-yaml-schema  R:none
                                                                                                                 A:.pipeline/findings/perf-adr-iter{N}.yaml
       1.b.3 devex-adr-review                               M:flash3 B:<devex_jess>+<adr_format>                  V:devex-checklist-schema R:none
                                                                                                                 A:.pipeline/findings/devex-adr-iter{N}.yaml

     1.c merge-findings-into-accumulator                    M:none   B:-                                          V:yaml-merge-ok        R:atomic-diff
                                                                                                                  A:.pipeline/findings/adr-cumulative.yaml
                                                                                                                  I:1.b.1, 1.b.2, 1.b.3

     1.d cross-vendor-adr-review (only iter where 1.b passes) M:pro   B:<adr_format>+<security_baseline>+<perf_baseline>
                                                                                                                  V:approve-yaml         R:none
                                                                                                                  A:.pipeline/findings/cross-vendor-adr-iter{N}.yaml

     1.e exit-check                                         M:none   B:-                                          V:no-blocking-findings R:none
                                                                                                                  reads: 1.c output

2. architecture-design                                       [group, sequential, after ADR loop exits]
   2.1 module-placement-map                                 M:sonnet B:<event_sourcing_patterns>+<style_guide>+<adr_format>
                                                                                                                  V:architecture-schema-valid R:atomic-diff
                                                                                                                  A:.pipeline/architecture.yaml
                                                                                                                  I:docs/decisions/NNNN-slug.md
   2.2 public-api-signatures-only                           M:sonnet B:<neo_prelude>+<event_sourcing_patterns>     V:signatures-compile    R:atomic-diff
                                                                                                                  A:.pipeline/api-stubs.hs
                                                                                                                  (módulo con type sigs + `undefined` que el implementer rellenará)

3. test-spec-design                                          [group, sequential]
   3.1 enumerate-cases                                      M:sonnet B:<qa_rubric>+<test_patterns>                 V:qa-spec-schema        R:atomic-diff
                                                                                                                  A:.pipeline/test-spec.md
                                                                                                                  I:.pipeline/architecture.yaml, .pipeline/api-stubs.hs

4. tests-first                                               [group, sequential]
   4.1 write-failing-test-scaffold                          M:flash3 B:<test_patterns>+<neo_prelude>               V:cabal-build-ok        R:atomic-diff
                                                                                                                  I:.pipeline/test-spec.md, .pipeline/api-stubs.hs
   4.2 register-in-cabal-and-runner                         M:haiku  B:<test_patterns>                             V:cabal-build-ok        R:atomic-diff
   4.3 confirm-fails-for-right-reason                       M:flash3 B:-                                           V:test-fails-expected   R:none

5. implementation                                            [group, sequential]
   5.1 skeleton-from-stubs                                  M:flash3 B:<neo_prelude>+<event_sourcing_patterns>     V:cabal-build-ok        R:atomic-diff
                                                                                                                  I:.pipeline/architecture.yaml, .pipeline/api-stubs.hs
   5.2 per-function-impl                                    [parallel-per-function]
       5.2.k impl-function-k                                M:flash3 B:<neo_prelude>+<event_sourcing_patterns>     V:cabal-build-ok        R:atomic-diff
                                                                                                                  I:relevant subset of architecture for function k

6. green-loop                                                [loop, max_iterations=5, accumulator=.pipeline/build-fail-history.jsonl]
   exit_condition: "cabal-test-ok AND hlint-clean"
   iteration_body:
     6.a run-target-test-suite                              M:none   B:-                                          V:cabal-test-ok         R:none
     6.b on-fail: diagnose                                  M:sonnet B:<neo_prelude>+test-output                   V:diagnosis-schema      R:none
                                                                                                                  A:.pipeline/build-fail-iter{N}.yaml
                                                                                                                  (only runs if 6.a failed)
     6.c on-fail: single-edit-patch                         M:flash3 B:<neo_prelude>+<event_sourcing_patterns>     V:cabal-test-ok         R:atomic-diff
                                                                                                                  I:6.b output

7. quality-polish                                            [group, parallel]
   7.1 hlint                                                M:haiku  B:<style_guide>                              V:hlint-clean           R:atomic-diff
   7.2 doctest (if applicable)                              M:haiku  B:<test_patterns>                             V:doctest-ok            R:atomic-diff
   7.3 style-enforcement-anti-patterns                      M:none   B:-                                          V:grep-anti-patterns    R:none

8. IMPL-REVIEW-LOOP                                          [loop, max_iterations=3, accumulator=.pipeline/findings/impl-cumulative.yaml]
   exit_condition: "no-critical-or-high-findings"
   iteration_body:
     8.a parallel-impl-review                               [parallel]
       8.a.1 security-impl-review                           M:sonnet B:<security_baseline>+<event_sourcing_patterns>+source
                                                                                                                  V:findings-yaml-schema  R:none
                                                                                                                  A:.pipeline/findings/security-impl-iter{N}.yaml
                                                                                                                  I:all changed *.hs files
       8.a.2 performance-impl-review                        M:sonnet B:<perf_baseline>+<event_sourcing_patterns>+source
                                                                                                                  V:findings-yaml-schema  R:none
                                                                                                                  A:.pipeline/findings/perf-impl-iter{N}.yaml

     8.b fix-findings                                       M:flash3 B:<neo_prelude>+<event_sourcing_patterns>+findings
                                                                                                                  V:cabal-build-ok AND cabal-test-ok R:atomic-diff
                                                                                                                  I:8.a.* outputs

     8.c re-trigger-policy                                  M:none   B:-                                          V:re-trigger-decision   R:none
       (si 8.b tocó >N líneas de un módulo crítico (auth, http, crypto), forzar otra iteración
        aunque exit_condition pase para asegurar el fix no introdujo regresiones)

     8.d cross-vendor-impl-review                           M:pro    B:<security_baseline>+<perf_baseline>+source  V:approve-yaml          R:none
                                                                                                                  (only on the last iteration that passes 8.a + 8.b)

9. FINAL-VERIFICATION-100%                                   [gate, all-must-pass]
   9.1 cabal-build-all                                      —        B:-                                          V:exit-0                R:none
   9.2 cabal-test-affected-suites                           —        B:-                                          V:exit-0                R:none
   9.3 hlint-clean                                          —        B:-                                          V:exit-0                R:none
   9.4 doctest-ok                                           —        B:-                                          V:exit-0                R:none
   9.5 grep-anti-patterns                                   —        B:-                                          V:no-matches            R:none
   9.6 cross-model-final-review (vendor opposite to most leaves) M:pro B:<neo_prelude>+source                       V:approve               R:none
   9.7 doc-axis: release-note-exists                        M:none   B:-                                          V:release-note-grep     R:none
                                                                                                                  (busca línea matching en CHANGELOG / PR body)
   9.8 axis-coverage-check                                  M:none   B:-                                          V:axis-coverage-yaml    R:none
                                                                                                                  (verifica que TODOS los ejes obligatorios de §5.3.1 se han ejecutado)

10. PR-creation                                              [group, sequential, only after 9 passes]
    10.1 write-pr-body                                      M:haiku  B:<community_voice>                           V:pr-body-schema        R:atomic-diff
                                                                                                                  A:.pipeline/pr-body.md
                                                                                                                  I:.pipeline/architecture.yaml, .pipeline/test-spec.md, all findings YAMLs
    10.2 commit-and-push                                    M:haiku  B:-                                           V:branch-pushed         R:none
    10.3 gh-pr-create                                       M:haiku  B:-                                           V:pr-url-extracted      R:none

11. POST-PR                                                  [group, sequential]
    11.1 wait-for-ci                                        M:none   B:-                                          V:ci-finished           R:none
    11.2 BOT-COMMENTS-LOOP                                  [loop, max_iterations=5]
         exit_condition: "ci-green AND coderabbit-no-blockers"
         11.2.a fetch-bot-comments                          M:none   B:-                                          V:json-fetched          R:none
         11.2.b address-comments                            M:flash3 B:<neo_prelude>+<event_sourcing_patterns>     V:cabal-test-ok         R:atomic-diff
         11.2.c push-fix                                    M:haiku  B:-                                           V:branch-pushed         R:none

12. HUMAN-MERGE-GATE                                         [gate, human]
    12.1 maintainer-merge                                   (no model, awaits human)
```

#### 6.1.1 Notas críticas sobre el ADR-design-loop (§6.1.1)

- El loop converge cuando **simultáneamente**: 0 findings Critical/High de seguridad, 0 findings Blocking de performance, todos los criterios DevEx pasan. Si alguno falla → otra iteración.
- En `1.a` iteración 1, el architect entrevista al usuario o lee el issue (interview phase del adr-architect skill: 3-7 questions/turn, batched, hasta cubrir trigger, current state, decision points, alternatives, rationale, type shapes, blast radius, consequences, risks, NeoHaskell fit, scope boundary).
- En `1.a` iteración ≥2, el architect tiene como input el `accumulator.yaml` con los findings de iteraciones previas. Su trabajo es revisar el ADR para abordar **cada** finding; cada finding tiene un campo `addressed_in_iter` cuando se resuelve.
- El `cross-vendor-adr-review (1.d)` se ejecuta solo en la iteración donde `1.b` ha pasado por primera vez. Si Pro detecta un finding adicional → vuelve al loop con el accumulator extendido.
- Si max_iterations=4 se exhauste → escalado §8.3 (típicamente: pause gate humano para que decida si re-formular el problema o split en varios ADRs).

#### 6.1.2 Notas críticas sobre el IMPL-REVIEW-LOOP (§6.1.8)

- El loop converge cuando 0 findings Critical/High de seguridad y 0 Blocking de perf en la última iteración.
- `8.c re-trigger-policy` es lo que hace que el loop **no se cierre prematuramente** tras un fix. Si el fix tocó un módulo crítico (lista predefinida: `core/auth/`, `core/http/`, `core/crypto/`, `core/eventstore/`), se fuerza otra iteración aunque las findings pendientes parezcan resueltas, para asegurar no haberse introducido nuevas.
- `8.d cross-vendor-impl-review` valida con un vendor distinto la última iteración pasante. Si rechaza → el rechazo entra al accumulator y se inicia otra iteración. Si max_iterations=3 se exhauste tras un rechazo cross-vendor → escalado.

### 6.2 Bugfix (canónico)

```
1. reproduce                                                 [group, sequential]
   1.1 read-bug-report-and-relevant-module                   M:haiku  B:<neo_prelude>+<event_sourcing_patterns>    V:structured-summary    R:none
   1.2 write-failing-regression-test                         M:flash3 B:<test_patterns>                             V:cabal-build-ok        R:atomic-diff
   1.3 confirm-fails-for-right-reason                        M:flash3 B:-                                           V:test-fails-expected   R:none

2. diagnose                                                  [group]
   2.1 stack-log-analysis                                    M:sonnet B:<neo_prelude>+log+source                    V:diagnosis-schema      R:none A:.pipeline/diagnosis.yaml
   2.2 localize-to-file-line                                 M:sonnet B:-                                           V:file-line-yaml        R:none

3. patch                                                     [single-edit ideal]
   3.1 single-edit-fix                                       M:flash3 B:<neo_prelude>+<event_sourcing_patterns>     V:cabal-test-ok         R:atomic-diff
                                                                                                                    I:.pipeline/diagnosis.yaml

4. regression-check                                          [gate]
   4.1 full-test-run-affected-suites                         —        B:-                                           V:exit-0                R:none

5. IMPL-REVIEW-LOOP-LIGHT                                    [loop, max_iterations=2]
   exit_condition: "no-critical-findings"
   iteration_body:
     5.a parallel-impl-review                                [parallel]
       5.a.1 security-impl-review                            M:sonnet B:<security_baseline>+source                  V:findings-yaml-schema  R:none
       5.a.2 performance-impl-review                         M:flash3 B:<perf_baseline>+source                       V:findings-yaml-schema  R:none
                                                             (flash3 en bugfix porque típicamente no hay grandes cambios; sonnet escala en feat)
     5.b fix-findings                                        M:flash3 B:<neo_prelude>                                V:cabal-test-ok         R:atomic-diff

6. quality-polish                                            [group, parallel]
   6.1 hlint                                                 M:haiku  B:<style_guide>                                V:hlint-clean           R:atomic-diff
   6.2 grep-anti-patterns                                    M:none   B:-                                            V:no-matches            R:none

7. FINAL-VERIFICATION-100%                                   [gate, all-must-pass]
   7.1 cabal-build-all                                       —                                                       V:exit-0                R:none
   7.2 cabal-test-full                                       —                                                       V:exit-0                R:none
   7.3 hlint-clean                                           —                                                       V:exit-0                R:none
   7.4 cross-model-review                                    M:pro    B:<neo_prelude>+source                         V:approve               R:none
                                                             (obligatorio si tocó eventstore/auth/http/crypto; opcional si solo test/schema/integration)
   7.5 axis-coverage-check                                   M:none   B:-                                            V:axis-coverage-yaml    R:none

8. PR + 9. POST-PR                                           (idéntico a §6.1.10-11)

10. HUMAN-MERGE-GATE                                         (idéntico a §6.1.12)
```

#### 6.2.1 ADR para bugfix (cuándo)

Bugfixes triviales (typo, off-by-one, missing import) → **sin ADR**. Bugfixes que cambian comportamiento observable o tocan invariantes (ej. ADR-0038 fix-flaky-subscription, ADR-0039 listen-notify-leak) → **sí ADR**, usando el §6.1.1 ADR-design-loop pero con `max_iterations=2` (los bugs tienden a tener decisiones más acotadas).

### 6.3 Dependency bump (canónico — el más automatable)

```
1. read-changelog                                            M:haiku  B:-                                            V:structured-summary    R:none A:.pipeline/dep-changelog.yaml
2. update-lockfile                                           M:haiku  B:-                                            V:cabal-build-ok        R:atomic-diff
3. build-and-smoke-test                                      —        B:-                                            V:cabal-test-affected-suites R:none

4. CONDITIONAL-BRANCH                                         [conditional]
   if 3.fail:
     4.a.1 attempt-revert-and-issue-create                   M:haiku  B:-                                            V:branch-clean           R:atomic-diff
     4.a.2 escalate-decision                                 M:sonnet B:-                                            V:plan-yaml              R:none
   else:
     4.b conditional-eje-checks                              [parallel]
        4.b.1 security-axis (only if dep is auth/crypto/http) M:flash3 B:<security_baseline>                          V:findings-yaml-schema   R:none
        4.b.2 perf-axis (only if dep is hot-path)             M:flash3 B:<perf_baseline>                              V:findings-yaml-schema   R:none

5. FINAL-VERIFICATION-100%                                    [gate]
   5.1 cabal-build-all                                       —        B:-                                            V:exit-0                R:none
   5.2 cabal-test-affected-suites                            —        B:-                                            V:exit-0                R:none

6. PR + 7. POST-PR                                           (idéntico a §6.1, sin cross-vendor en 6.6)
```

### 6.4 Test additions puras

```
1. read-source-under-test                                    M:haiku  B:<neo_prelude>                                V:structured-summary    R:none
2. enumerate-cases-boundary-happy-error                      M:flash3 B:<qa_rubric>+<test_patterns>                  V:qa-spec-schema        R:atomic-diff A:.pipeline/test-spec.md
3. write-test-file                                           M:haiku  B:<test_patterns>                              V:cabal-build-ok        R:atomic-diff
4. register-and-run                                          M:haiku  B:<test_patterns>                              V:cabal-test-ok         R:atomic-diff
5. quality-polish                                            (idéntico a §6.1.7)

6. FINAL-VERIFICATION-100%                                   [gate]
   6.1 cabal-build-all                                       —                                                       V:exit-0                R:none
   6.2 cabal-test-affected                                   —                                                       V:exit-0                R:none
   6.3 hlint-clean                                           —                                                       V:exit-0                R:none
   6.4 axis-coverage: test ≥ qa-rubric thresholds            M:none                                                  V:edge-happy-ratio      R:none
   (cross-vendor opcional)
```

### 6.5 Docs

```
1. read-source-or-adr-context                                M:haiku  B:-                                            V:structured-summary    R:none
2. draft                                                     M:haiku  B:<community_voice>                            V:doc-schema            R:atomic-diff
3. translation-pass (if applicable)                          M:flash  B:<community_voice>                            V:diff-only-translations R:atomic-diff
4. (optional) API-facing-review                              M:sonnet B:<devex_jess>+<community_voice>               V:approve               R:none

5. FINAL-VERIFICATION-100%                                   [gate]
   5.1 link-check                                            —                                                       V:lychee-exit-0         R:none
   5.2 spell-check                                           —                                                       V:cspell-exit-0         R:none
   5.3 jess-language-grep (no jargon prohibido)              M:none                                                  V:no-matches            R:none
   5.4 doctest-if-code-blocks-executable                     —                                                       V:exit-0                R:none
   (cross-vendor opcional)
```

### 6.6 Refactor (sin cambio de comportamiento)

```
0. lock-down: confirmar tests verdes pre-refactor            —        B:-                                            V:exit-0                R:none A:.pipeline/pre-refactor-test-output.txt
1. move-or-rename                                            M:flash3 B:<neo_prelude>+<event_sourcing_patterns>      V:cabal-build-ok        R:atomic-diff
2. update-call-sites                                         [parallel-per-call-site]
   2.k update-site-k                                         M:haiku  B:<neo_prelude>                                V:cabal-build-ok        R:atomic-diff
3. test-run                                                  —        B:-                                            V:exit-0-AND-output-byte-equal-to-pre-refactor R:none
                                                             (esto es la regla estricta §6.6.4: si el output difiere de .pipeline/pre-refactor-test-output.txt, fallar)
4. on-fail-revert-entire-subtree                             —        B:-                                            V:branch-clean          R:atomic-diff
                                                             (descarta TODOS los diffs de §6.6.1-2; principio P3 a nivel de árbol)
5. quality-polish                                            (idéntico a §6.1.7)
6. FINAL-VERIFICATION-100%                                   [gate]
   6.1 cabal-build-all                                       —                                                       V:exit-0                R:none
   6.2 cabal-test-output-byte-equal                          —                                                       V:exit-0-AND-byte-equal R:none
   6.3 grep-anti-patterns                                    M:none                                                  V:no-matches            R:none
   6.4 hlint-clean                                           —                                                       V:exit-0                R:none
```

**Regla estricta §6.6.4**: el output de tests debe ser **byte-equal** al pre-refactor (no solo exit code). Si difiere → revert completo del subárbol §6.6.1-2. Esto garantiza P3 a nivel de árbol entero.

### 6.7 Perf

```
1. baseline-benchmark                                        —        B:-                                            V:numeric-baseline-stored R:none A:.pipeline/perf-baseline.json
2. hypothesis                                                M:sonnet B:<perf_baseline>+<neo_prelude>                V:hypothesis-yaml-schema R:atomic-diff A:.pipeline/perf-hypothesis.yaml
3. APPLY-LOOP                                                [loop, max_iterations=N (number of hypothesis items)]
   exit_condition: "all-hypotheses-tried"
   iteration_body:
     3.a apply-change-k                                      M:flash3 B:<perf_baseline>+<neo_prelude>                V:cabal-build-ok         R:atomic-diff
     3.b benchmark                                           —        B:-                                            V:numeric-improvement-or-flat R:none
     3.c on-regression-revert-change-k                       —        B:-                                            V:numeric-baseline-restored R:atomic-diff

4. FINAL-VERIFICATION-100%                                   [gate]
   4.1 cabal-build-all                                       —                                                       V:exit-0                R:none
   4.2 cabal-test-full                                       —                                                       V:exit-0                R:none
   4.3 benchmark-better-or-equal                             —                                                       V:numeric-improvement   R:none
   4.4 perf-impl-review-cross-vendor                         M:pro    B:<perf_baseline>+source                       V:approve               R:none
   4.5 security-impl-review (any change can introduce regression) M:flash3 B:<security_baseline>+source              V:findings-yaml-schema  R:none
```

### 6.8 Security hardening (multi-finding wave)

```
1. threat-model                                              M:sonnet B:<security_baseline>+<event_sourcing_patterns>+source
                                                                                                                     V:threat-model-schema   R:atomic-diff A:.pipeline/threat-model.yaml
2. PER-FINDING-FORK                                          [parallel-per-finding]
   2.k subárbol §6.2 (bugfix) con bundles + <security_baseline> forzado en todas las hojas
       y con re-trigger-policy más agresiva en 8.c (siempre re-trigger)

3. CONSOLIDATION-LOOP                                        [loop, max_iterations=2]
   exit_condition: "no-cross-finding-conflicts"
   iteration_body:
     3.a integration-test-suite                              —        B:-                                            V:cabal-test-integration-ok R:none
     3.b on-conflict: per-finding-fix                        (escala a §6.2 sub-loop)

4. FINAL-VERIFICATION-100% (doble cross-vendor)              [gate]
   4.1 cabal-build-all                                       —                                                       V:exit-0                R:none
   4.2 cabal-test-full                                       —                                                       V:exit-0                R:none
   4.3 security-cross-vendor-pro                             M:pro    B:<security_baseline>+source                   V:approve               R:none
   4.4 security-cross-vendor-sonnet (different vendor than 4.3) M:sonnet B:<security_baseline>+source                  V:approve               R:none
   4.5 axis-coverage-check                                   M:none                                                  V:axis-coverage-yaml    R:none
   4.6 ADR exists and references all findings                M:none                                                  V:adr-references-grep   R:none
                                                             (cada finding del threat model debe estar referenciado en el ADR §6.1.1.a)
```

### 6.9 Feature parser/syntax (alta dificultad)

Variante de §6.1 con sustituciones:

- Bundle base: `<ide_layer>` + `<neo_prelude>` (forbid `<ghc_internals>` salvo en `<bridge_layer>`).
- §6.1.5.2 (per-function impl) **NO** se ejecuta en Haiku/Flash. Modelo mínimo: Sonnet.
- §6.1.9.6 cross-model review usa Gemini 3 Pro **obligatorio** (sin opción).
- ADR-design-loop en §6.1.1 con `max_iterations=5` (parser decisions tienen más alternativas).
- Extra: §6.9.X **golden-test-vs-syntax-fixtures** (compare nuevo parser output contra fixtures existentes en `transpiler/design/syntax/fixtures/`).

### 6.10 AI/skill tooling y CI/infra

Categorías con baja determinismo. Por defecto **escalan a humano** (pause gate, §8.3) tras una pasada inicial de plan en Sonnet. No tienen árbol canónico — cada caso se planifica como árbol ad-hoc heredando de §6.1 o §6.2 según naturaleza.

Reglas mínimas:
- Si la tarea cambia el comportamiento de un agente en runtime (ej. nuevo skill que modifica orquestación) → security axis obligatorio.
- Si la tarea cambia CI workflows → smoke test en branch separada antes de merge.

---

## 7. Catálogo de verificadores

### 7.1 Lista

Cada verificador debe ser **binario**, **automatizado** y **reproducible**.

| Verificador | Cómo se ejecuta | Qué bloquea | Coste relativo | Observable |
|-------------|-----------------|-------------|----------------|------------|
| `cabal-build-ok` | `cabal build all` (o targets afectados) | compilación | medio | exit code |
| `cabal-test-ok` | `cabal test <suite>` | comportamiento | medio-alto | exit code + nombres |
| `cabal-test-full` | `cabal test` completo | regresiones | alto | exit code |
| `cabal-test-affected-suites` | suites determinadas por archivos modificados | regresiones acotadas | medio | exit code |
| `cabal-test-integration-ok` | `cabal test nhcore-test-integration` | flujos cross-module | alto | exit code |
| `test-fails-expected` | corre el test y exige fallo, además del nombre del fallo | reproducción correcta | medio | log parsed |
| `hlint-clean` | `hlint .` (o ficheros tocados) | estilo | bajo | exit code |
| `doctest-ok` | `./scripts/run-doctest` | docs ejecutables | medio | exit code |
| `grep-anti-patterns` | grep negativo de las 12 reglas en ficheros tocados | style guide enforcement | trivial | match count = 0 |
| `hurl-ok` | scripts hurl de testbed | contratos HTTP | medio | exit code |
| `bash-integration-ok` | scripts bash de testbed | flujos end-to-end | alto | exit code |
| `cross-model-review` | misma subtarea revisada por modelo de vendor opuesto, output binario aprobado/rechazado con razones | sesgo de modelo | medio (token cost) | YAML structured (§7.2.5) |
| `numeric-baseline-stored` | benchmark guardado a archivo | regresión perf | alto | float |
| `numeric-improvement` | benchmark vs baseline | mejora exigida | alto | float ratio |
| `numeric-improvement-or-flat` | benchmark ≥ baseline | no regresión perf | alto | float ratio |
| `branch-clean` | `git status --porcelain` | reversibilidad | trivial | empty output |
| `branch-pushed` | `git rev-parse @{u}` matches `git rev-parse HEAD` | upload OK | trivial | hashes match |
| `pr-url-extracted` | `gh pr view --json url` | PR creado | trivial | URL válida |
| `ci-finished` | `gh pr checks` no in-progress | espera CI | n/a | poll |
| `structured-summary` | output YAML con campos obligatorios validado por schema (§7.2.1) | tarea read-only completada | trivial | schema match |
| `enum-match` | output es uno de un enum predefinido | clasificación | trivial | enum membership |
| `findings-yaml-schema` | output cumple schema findings-yaml-v1 (§7.2.2) | reviews tienen forma | trivial | schema match |
| `devex-checklist-schema` | output cumple devex-checklist-v1 (§7.2.4) | DevEx review estructurado | trivial | schema match |
| `qa-spec-schema` | output cumple qa-spec-v1 (§7.2.6) | spec QA estructurado | trivial | schema match |
| `architecture-schema-valid` | output cumple architecture-yaml-v1 (§7.2.7) | architecture doc estructurado | trivial | schema match |
| `adr-schema-valid` | markdown cumple ADR template (§3.4.3) | ADR estructurado | trivial | section presence + headers |
| `signatures-compile` | archivo `.hs` con `undefined` compila | API skeleton OK | medio | exit code |
| `pr-body-schema` | markdown cumple PR body template (§3.4.10) | PR body estructurado | trivial | section presence |
| `link-check` | `lychee` o equivalente sobre docs tocadas | docs rotas | bajo | exit code |
| `spell-check` | `cspell` con diccionario NeoHaskell | docs typos | trivial | exit code |
| `jess-language-grep` | grep de palabras prohibidas (`monoid`, `functor`, `simply`, `obviously`, `just`) | jargon | trivial | match count = 0 |
| `naming-grep` | grep de patrones DevEx anti (ej. `validateAndCreate`) | DevEx naming | trivial | match count = 0 |
| `release-note-grep` | grep en CHANGELOG.md o PR body de línea matching format | release note exists | trivial | match count ≥ 1 |
| `axis-coverage-yaml` | output YAML enumera todos los ejes obligatorios y su estado | cobertura ejes | trivial | schema + all-required-present |
| `diff-size-guard` | `git diff --stat` | reversibilidad de la subtarea | trivial | LOC tocadas ≤ umbral |
| `security-review-skill` | invocación local del skill `neohaskell-security-review` | findings críticos | medio | YAML findings |
| `no-blocking-findings` | parsea findings YAML, devuelve true si 0 Critical/High/Blocking | exit condition de loops review | trivial | bool |
| `re-trigger-decision` | aplica regla §6.1.8.c | re-loop forzoso | trivial | bool |
| `yaml-merge-ok` | merge de YAMLs accumulator + nuevos findings sin conflictos | acumulador consistente | trivial | exit code |
| `approve-yaml` | output YAML con `verdict: approve` o `verdict: reject` + razones | cross-vendor review | trivial | enum match |
| `byte-equal` | `cmp -s file1 file2` | output idéntico | trivial | exit code |
| `adr-references-grep` | grep cruzado: cada finding del threat model está en el ADR | trazabilidad sec wave | trivial | all-found |
| `qa-rubric-thresholds` | parsea test spec, comprueba ratios (≥3 cases/fn, ≥3:1 edge:happy) | qa coverage | trivial | bool |
| `lychee-exit-0` | `lychee --offline` o `--remote` sobre docs | links | bajo | exit code |
| `cspell-exit-0` | `cspell **/*.md` | typos | trivial | exit code |

**Reglas:**
- Una subtarea siempre lleva al menos un verificador. Las puramente generativas (specs, summaries) llevan `structured-summary`/`findings-yaml-schema`/etc.
- Los verificadores son **invariantes** entre intentos. No se relajan ante fallos repetidos — eso es lo que dispara el escalado, no el debilitamiento del check.
- `diff-size-guard` aplica por defecto a toda subtarea con `R:atomic-diff` (umbral configurable por categoría: 50 LOC para single-edit, 200 LOC para skeleton, 500 LOC para feature impl).

### 7.2 Esquemas estructurados de output

Cada artifact producido por una hoja read-only tiene un **schema versionado**. Esto permite que el orquestador valide y que las hojas downstream consuman con garantías.

#### 7.2.1 `discovery-summary-v1`

Ya descrito en §5.1.3.

#### 7.2.2 `findings-yaml-v1` (security & performance)

```yaml
review:
  type: security|performance
  reviewer: <model-id>
  reviewed_at: <ISO-8601>
  reviewed_artifact: <path>           # ADR path o source file paths
findings:
  - id: F1
    severity: Critical|High|Medium|Low|Blocking|Advisory
    category: <category from §3.4.4 or §3.4.5>
    file: <path or "ADR">
    line: <int or null>
    finding: <description, ≤500 chars>
    fix: <recommendation, ≤500 chars>
    rationale: <why this matters, ≤300 chars>
    addressed_in_iter: <int or null>  # populated by accumulator merger
summary:
  critical: <int>
  high: <int>
  medium: <int>
  low: <int>
  blocking: <int>
  advisory: <int>
  overall_assessment: Pass|Conditional Pass|Fail
  blocking: bool                      # true if Critical/High/Blocking > 0
```

#### 7.2.3 `perf-findings-yaml-v1`

Idéntico a §7.2.2 pero con severities `Blocking|Advisory` y categorías de `<perf_baseline>`.

#### 7.2.4 `devex-checklist-v1`

```yaml
review:
  type: devex
  reviewer: <model-id>
  reviewed_at: <ISO-8601>
  reviewed_artifact: <path>
checklist:
  naming:
    pass: bool
    issues: [<string>]
  pipe_friendliness:
    pass: bool
    issues: [<string>]
  discoverability:
    pass: bool
    issues: [<string>]
  error_messages:
    pass: bool
    issues: [<string>]
  defaults:
    pass: bool
    issues: [<string>]
  consistency:
    pass: bool
    issues: [<string>]
jess_test:
  can_discover_via_autocomplete: bool
  matches_mental_model: bool
  usable_without_docs: bool
  errors_help_fix: bool
overall: Pass|Needs Work|Fail
```

#### 7.2.5 `cross-vendor-review-v1`

```yaml
review:
  type: cross_vendor
  reviewer_vendor: anthropic|google
  reviewer_model: <model-id>
  reviewed_at: <ISO-8601>
  reviewed_artifact: [<paths>]
verdict: approve|reject
reasons:
  - <string ≤300 chars>
specific_concerns:                    # only if reject
  - file: <path>
    line: <int>
    concern: <string>
counter_findings:                     # findings the original reviewer missed
  - severity: <enum>
    finding: <string>
    fix: <string>
```

#### 7.2.6 `qa-spec-v1`

Markdown, ya descrito por estructura en §3.4.8. Verificador comprueba:
- Sección "Module Under Test" presente con `module_path`, `test_file_path`, `test_suite`.
- Cada función pública tiene ≥3 sub-secciones (Happy / Edge / Error).
- Cada error constructor tiene ≥1 fila en tabla.
- Total test count documentado.
- Edge:happy ratio ≥3:1.

#### 7.2.7 `architecture-yaml-v1`

```yaml
feature: <name>
adr_reference: <path>
modules:
  - path: <path>
    purpose: <string>
    public_exports:
      - name: <string>
        signature: <string>
        kind: function|type|typeclass|instance|TH-macro
    dependencies:
      nhcore: [<module>]
      external: [<package>]
    cabal_changes:
      add_to_other_modules: [<module>]
      add_to_hs_source_dirs: [<dir>]
      add_test_module: [<module>]
integration_points:
  event_store: <yes|no, with details>
  command_executor: <yes|no, with details>
  query_dispatcher: <yes|no, with details>
  outbound_integration: <yes|no, with details>
nhcore_utilities_to_use:
  - <function or module reference>
```

#### 7.2.8 `threat-model-v1`

```yaml
scope:
  modules: [<path>]
  attack_surface: [<HTTP|CLI|Database|FileSystem|InterProcess|...>]
threats:
  - id: T1
    category: <OWASP/NIST identifier>
    actor: <unauthenticated|authenticated-user|admin|external-system>
    asset: <data or capability at risk>
    impact: <Confidentiality|Integrity|Availability>
    likelihood: Low|Medium|High
    severity: Low|Medium|High|Critical
    description: <string>
    proposed_mitigation: <string>
    affects_findings: [<finding-id from a security review YAML>]
mitigation_plan:
  - threat_id: T1
    fix_subtask_id: <subtree path>
    verification: <verifier_id>
```

---

## 8. Reglas de orquestación

### 8.1 Asignación de modelo
1. **Por defecto:** el modelo objetivo declarado en el árbol canónico (§6).
2. **Si el learning file (§10) tiene ≥30 muestras** para esa subtask, se usa el modelo con mayor `success_rate * (1 / token_cost)` siempre que `success_rate ≥ 0.85`.
3. **Override manual:** un campo `model_override` en el estado puede forzar tier. No persiste entre tareas.

### 8.2 Cross-vendor review
- Si la subtarea de implementación corrió en **Anthropic** (Haiku/Sonnet), el review final corre en **Google** (Flash/Pro).
- Si corrió en **Google**, el review corre en **Anthropic**.
- Si la categoría exige doble review (security wave, parser/syntax), corren ambos vendors en paralelo y se exige unanimidad.
- **Opcional para categorías triviales** (ver tabla §4 columna "Cross-vendor"): docs, dep bumps, test additions puras, translation, IDE/tooling, fix(test).

### 8.3 Escalado
Trigger: el verificador de una subtarea falla **3 veces consecutivas** con el mismo modelo y los mismos bundles, O un loop excede `max_iterations`.

Escalera:

```
Haiku 4.5    → Gemini 3 Flash    (mismo tier, distinto vendor)
Gemini 3 Flash → Sonnet 4.6      (subir tier)
Sonnet 4.6   → Gemini 3 Pro      (mismo tier, distinto vendor)
Gemini 3 Pro → pause gate humano
pause gate   → Opus (último recurso, si humano lo autoriza)
Opus         → abort + entrada en learning file marcada como redesign-required
```

Cada escalón es una **nueva subtarea independiente**, con su propio diff atómico. El intento previo se descarta (P3).

### 8.4 Reversibilidad y atomic diffs
- Toda subtarea con `R:atomic-diff` produce un blob en `.pipeline/atomic-diffs/<subtask-id>-<attempt-n>.diff`.
- Si el verificador falla, el diff se descarta antes del siguiente intento.
- La verificación final 100% (§6.x.FINAL) consolida todos los diffs aprobados en un único commit final.

### 8.5 Paralelización
Marcadores en el árbol:
- `[parallel]`: hijos independientes, ejecutar concurrentes.
- `[parallel-per-X]`: fan-out dinámico (por función, por call-site, por finding).
- `[group, sequential]`: orden fijo.
- `[loop, max_iterations=N]`: bucle con techo, dispara escalado al exceder.
- `[gate, all-must-pass]`: barrera; cualquier hijo fallido aborta el grupo.
- `[conditional]`: ejecuta una rama u otra según el resultado de un nodo predecesor.

### 8.6 Sin PAUSE gates por defecto
Eliminados los PAUSE gates del pipeline previo (ADR draft, DevEx, Architecture, Test Spec, Impl reviews, PR creation, Final approval). El humano interviene **solo** vía:
- Escalado §8.3 (último escalón antes de Opus).
- Categorías §6.10 (AI tooling, CI/infra) que son no-deterministas por naturaleza.
- `12. HUMAN-MERGE-GATE` después de PR (esto es el merge final, no un PAUSE arbitrario).
- Override manual explícito.

### 8.7 Re-trigger por área tocada (impl review)

Listas predefinidas de "áreas críticas" donde un fix automáticamente re-dispara el review loop incluso si el exit_condition pasa:

```yaml
critical_areas:
  - core/auth/
  - core/http/
  - core/crypto/
  - core/eventstore/
  - core/service/Service/Command/
  - core/service/Service/Integration/Outbound.hs
```

La regla `8.c re-trigger-policy` en §6.1.8 se evalúa así: si los archivos modificados en `8.b` matchean cualquier prefijo en `critical_areas`, force_iter=true regardless of exit_condition.

---

## 9. Estado y artefactos en disco

### 9.1 Estado del árbol

```yaml
task_id: 2026-04-25-feat-decimal-type
category: feat(core)
status: in_progress           # in_progress | done | aborted | escalated
created_at: 2026-04-25T12:00:00Z
tree:
  - id: "0.trigger.read-issue"
    type: leaf
    model: haiku-4.5
    bundles_required: []
    bundles_forbidden: []
    verifier: structured-summary
    reversibility: none
    artifact_path: ".pipeline/trigger.yaml"
    attempts:
      - n: 1
        model_used: haiku-4.5
        outcome: pass
        tokens_in: 4210
        tokens_out: 380
        wall_clock_ms: 2891
        timestamp: 2026-04-25T12:01:12Z
    diff_hash: null
    status: done
  - id: "1.adr-design-loop"
    type: loop
    max_iterations: 4
    accumulator_artifact: ".pipeline/findings/adr-cumulative.yaml"
    iterations:
      - n: 1
        body_status: completed
        exit_check: failed         # had blocking findings
        leaves: [...]
      - n: 2
        body_status: completed
        exit_check: passed
        leaves: [...]
    status: done
  - id: "2.architecture-design"
    ...
final_verification:
  required:
    - cabal-build-ok
    - cabal-test-affected-suites
    - hlint-clean
    - doctest-ok
    - grep-anti-patterns
    - cross-model-final-review
    - release-note-grep
    - axis-coverage-yaml
  status: pending
escalations: []
```

Decisiones clave:
- **Append-only en `attempts`**: nunca se reescribe un intento.
- **`diff_hash`** referencia un blob en `.pipeline/atomic-diffs/` aún si la subtarea se descarta.
- **`escalations`** es un log paralelo: cada escalado deja una entrada con `from`, `to`, `reason`, `subtask_id`.

### 9.2 Artifact registry

Todos los artifacts producidos por hojas viven en `.pipeline/`. Layout canónico:

```
.pipeline/
  state.json                          # estado del árbol (§9.1)
  atomic-diffs/
    <subtask-id>-<attempt-n>.diff
  trigger.yaml                        # output de §6.1.0
  category.yaml                       # output de §6.1.0.2
  discovery/
    related-modules.yaml              # output de §6.1.0... (cuando exista discovery extendido)
    related-tests.yaml
  findings/
    adr-cumulative.yaml               # accumulator de §6.1.1 ADR-design-loop
    security-adr-iter1.yaml
    perf-adr-iter1.yaml
    devex-adr-iter1.yaml
    security-adr-iter2.yaml
    ...
    impl-cumulative.yaml              # accumulator de §6.1.8 IMPL-REVIEW-LOOP
    security-impl-iter1.yaml
    perf-impl-iter1.yaml
    cross-vendor-adr-iter2.yaml
    cross-vendor-impl-iter2.yaml
    cross-vendor-final.yaml
  architecture.yaml                   # output de §6.1.2
  api-stubs.hs                        # output de §6.1.2.2
  test-spec.md                        # output de §6.1.3
  build-fail-history.jsonl            # accumulator de §6.1.6 green-loop
  diagnosis.yaml                      # output de §6.2.2 (bugfix)
  threat-model.yaml                   # output de §6.8.1 (security wave)
  perf-baseline.json                  # output de §6.7.1 (perf)
  perf-hypothesis.yaml                # output de §6.7.2
  pr-body.md                          # output de §6.1.10.1
  learning.jsonl                      # append-only learning (§10)
```

Reglas:
- Cada artifact tiene **un único productor** (una hoja específica del árbol).
- Cada artifact tiene **schema versionado** (§7.2).
- Las hojas downstream declaran sus inputs por path. El orquestador valida que el artifact existe y cumple el schema antes de invocar la hoja.
- `.pipeline/` está en `.gitignore` por defecto (estado efímero entre runs).

---

## 10. Aprendizaje persistido

Fichero append-only `.pipeline/learning.jsonl`. Una entrada por **intento** (no por subtarea), incluyendo los fallidos.

```json
{
  "task_id": "2026-04-25-feat-decimal-type",
  "task_category": "feat(core)",
  "subtask_canonical_id": "feature.impl-review-loop.iter-2.fix-findings",
  "subtask_in_loop": true,
  "loop_iteration": 2,
  "model": "gemini-3-flash",
  "bundles_required": ["<neo_prelude>", "<event_sourcing_patterns>"],
  "bundles_forbidden": ["<ghc_internals>"],
  "verifier": "cabal-test-ok",
  "outcome": "pass",
  "iterations_until_success": 1,
  "tokens_in": 6420,
  "tokens_out": 412,
  "wall_clock_ms": 8421,
  "escalated_from": null,
  "diff_lines_added": 12,
  "diff_lines_removed": 3,
  "axis_covered": ["security", "performance"],
  "timestamp": "2026-04-25T12:34:56Z"
}
```

### 10.1 Uso pasivo (rediseño)
Cada N tareas completadas (sugerido N=50), un **proceso reflexivo** (Sonnet, off-line, no en runtime) lee el `learning.jsonl` y produce:

- **Re-asignación de modelo por defecto**: si `success_rate(haiku, X) > success_rate(flash, X)` para subtarea `X`, actualizar §6.
- **Bundle minimization**: si `bundles_required` incluye un bundle que el verificador nunca discrimina (mismo outcome con/sin él), proponer eliminarlo.
- **Subtask atomicity audit**: si una subtarea tiene `iterations_until_success` mediana > 2, proponer re-dividirla.
- **Loop convergence audit**: si un loop alcanza `max_iterations` >10% de las veces, proponer aumentar techo o cambiar modelo del body.
- **Categoría faltante**: si N tareas se marcan `category: other`, agrupar y proponer nueva categoría.
- **Verificadores rotos**: si una subtarea pasa el verificador pero falla en `final_verification`, el verificador no es suficiente — auditar.
- **Axis coverage holes**: si el `axis-coverage-yaml` muestra ejes obligatorios omitidos, auditar el árbol canónico.

### 10.2 Uso activo (runtime)
Solo el sub-punto §8.1.2 (asignación de modelo basada en success rate). Todo lo demás es retrospectivo para no introducir feedback loops indeseados.

---

## 11. Anti-patrones (qué este diseño rechaza)

| Anti-patrón | Por qué se rechaza |
|-------------|--------------------|
| "Una fase larga ejecutada por Opus" | Coste, contexto excesivo, sin checks intermedios — el problema que originó este diseño. |
| "Un PAUSE gate por seguridad sin verificador" | Confunde sincronización humana con calidad. Los PAUSE gates son escalado, no flujo normal. |
| "Subtarea sin verificador" | Si no se puede verificar, no es una subtarea. Es un grupo. |
| "Reusar diffs entre intentos" | Rompe reversibilidad (P3). Cada intento es desde estado limpio. |
| "Mismo vendor revisa su propia output" | Sesgo no detectado. P5 es duro. |
| "Bundles 'completos' por seguridad" | Inflación de tokens y contaminación semántica. Bundles mínimos + `bundles_forbidden` explícito. |
| "Escalar a Opus al primer fallo" | Defeats the purpose. Hay 4 escalones antes de Opus. |
| "Adaptar verificadores al modelo" | Los verificadores son invariantes. Si el modelo no los pasa, escala — no se relaja el check. |
| "Loop sin budget" | Convergencia no garantizada. Todo loop tiene `max_iterations`. |
| "Loop sin accumulator" | La iteración N+1 no sabría qué intentó la N. Findings se acumulan. |
| "Categoría sin ejes obligatorios declarados" | El árbol no sabría qué verificar al final. Ver §5.3.1. |
| "Fix-findings sin re-trigger por área" | Un fix en `core/auth/` que pasa el exit_condition no garantiza no haber introducido regresiones. Re-trigger forzoso (§8.7). |
| "Cross-vendor opcional en feat(*) o security wave" | Pérdida de cobertura de sesgo en categorías de alto impacto. Solo opcional en triviales (§4). |
| "Refactor que cambia output de tests" | P3 a nivel de árbol (§6.6.4): byte-equal o revert completo. |
| "ADR sin alternativas rechazadas en tabla" | Una decisión sin alternativa no es una decisión. El verificador `adr-schema-valid` lo bloquea. |

---

## 12. Fuera de scope (trabajo futuro) y roadmap

Este documento NO especifica:

1. **Prompts ejecutables** por subtarea. (Manual ejecutable B en otra iteración.)
2. **Contenido palabra-por-palabra** de los bundles `<ghc_internals>`, `<bridge_layer>`, `<ide_layer>` — solo el contrato.
3. **Runtime de orquestación**: si es CLI, daemon, GitHub Action, etc.
4. **Routing entre vendors**: si vía API directa, broker (LiteLLM, OpenRouter), o multi-cliente.
5. **Política de retención** de diffs descartados, logs, learning file.
6. **Telemetría / observabilidad** del orquestador.
7. **UI** para inspeccionar el árbol en ejecución.
8. **Definición de éxito de la "verificación final 100%"** para categorías §6.10 (no deterministas) — pendiente caso a caso.
9. **Esquemas YAML formales** (JSON Schema documents). En §7.2 se da el shape; falta la formalización validable.
10. **Test fixtures cross-categoría**: 1-2 ejemplos worked-out por categoría para regression testing del orquestador.

### 12.1 Roadmap sugerido

1. **Fase 0**: bundle authoring — completar el contenido real de los bundles `<ghc_internals>`, `<bridge_layer>`, `<ide_layer>` (los demás están en §3.4).
2. **Fase 1**: schema authoring — convertir §7.2 a JSON Schema documents validables.
3. **Fase 2**: prompt templates — un template por hoja de árbol canónico, parametrizado por bundles.
4. **Fase 3**: orquestador de referencia — script/daemon que lee el árbol, invoca modelos, gestiona diffs y artifacts.
5. **Fase 4**: rodar con 1-2 tareas reales bajo observación humana (sugerido: 1 `chore(deps)` + 1 `fix(test)` para minimizar riesgo), ajustar.
6. **Fase 5**: rodar 50 tareas, primera pasada del proceso reflexivo §10.1.
7. **Fase 6**: portar el `pipeline.py` actual a este modelo o decommissionarlo.
8. **Fase 7**: implementar `feat(core)` o `feat(service)` end-to-end (categoría más compleja con todos los loops activos).

### 12.2 Riesgos abiertos

- **Coste del cross-vendor obligatorio**: si Pro/Sonnet se invocan en cada feat para review final, el ahorro vs. Opus puede no ser tan grande. Métrica a vigilar: % de tokens facturados en tier `smart` vs `dumb` por categoría.
- **Bundle drift**: si las skills source (`neohaskell-style-guide`, etc.) evolucionan en el repo original, los bundles embebidos en §3.4 quedan stale. Mitigación: proceso CI que diff'ee skill files con §3.4 y abra issue.
- **Schema drift**: §7.2 versionado pero sin migración automática. Si v2 sale, hojas downstream deben declarar la versión.
- **Loop oscilación**: en teoría un fix-findings podría introducir un nuevo finding que el siguiente review detecta, ad infinitum. `max_iterations` lo limita, pero conviene métrica de "% loops que convergen sin escalar".
- **Re-trigger en `critical_areas` (§8.7) demasiado conservador**: podría forzar iteraciones extra innecesarias. Auditar con learning file tras N tareas.

---

## 13. Glosario

- **Atomic diff**: cambio reversible producido por una subtarea, identificado por hash, descartable sin afectar al resto del árbol. Vive en `.pipeline/atomic-diffs/`.
- **Artifact**: output estructurado de una hoja, en `.pipeline/<path>`, con schema versionado. Consumido por hojas downstream.
- **Accumulator artifact**: artifact especial usado en loops para persistir estado entre iteraciones (ej. findings cumulative).
- **Bundle**: slot XML con contenido temático específico inyectado declarativamente al prompt (ver §3).
- **Cross-vendor review**: review por modelo de vendor distinto al de la implementación. Obligatorio en categorías no-triviales.
- **Eje de correctitud**: dimensión verificable que aplica a una categoría (security, performance, devex, style, test, doc). Cada eje tiene bundle source + verificadores. Ver §5.3.
- **Escalado**: substitución de modelo (mismo o superior tier, distinto vendor) tras fallo repetido del verificador. Escalera en §8.3.
- **Exit condition**: criterio que termina un loop. Verificable, binario.
- **Finding**: hallazgo de un review (security/perf), con severity, file:line, fix. Schema en §7.2.2.
- **Hoja**: nodo terminal del árbol. Anatomía formal en §5.1.
- **Iteration body**: subárbol que se ejecuta por cada iteración de un loop.
- **Jess**: persona junior dev, 15-30 min/día, target principal de DevEx y community-voice.
- **Learning file**: log append-only de intentos para rediseño retrospectivo. `.pipeline/learning.jsonl`.
- **Loop**: nodo no-hoja con iteration_body, max_iterations, exit_condition, accumulator. Semántica formal en §5.2.
- **PAUSE gate**: intervención humana obligatoria. En este diseño, **solo** existe como último escalón de §8.3 o como `12. HUMAN-MERGE-GATE`.
- **Re-trigger policy**: regla que fuerza otra iteración de un loop incluso si exit_condition pasa, basada en áreas tocadas. Ver §8.7.
- **Schema verifier**: verificador que valida un artifact YAML/markdown contra un schema versionado de §7.2.
- **Subtarea atómica**: hoja del árbol que cumple P1+P2+P3 (verificable, single-tool-call ideal, reversible).
- **Verificación final 100%**: gate después del impl review loop. All-must-pass. Cubre todos los ejes obligatorios de la categoría.
- **Verificador**: check binario automatizado que define el éxito de una subtarea (§7).
- **Árbol canónico**: plantilla de subtareas asociada a una categoría de §4. Ver §6.

---

## 14. Apéndice: tabla de referencia rápida (cheat-sheet para el agente que implementa)

### 14.1 ¿Qué hago si la categoría es...?

| Si... | Empieza por... | Loops involucrados | Ejes obligatorios |
|-------|----------------|---------------------|-------------------|
| `feat(*)` | §6.1 | ADR-design (§6.1.1), green (§6.1.6), impl-review (§6.1.8), bot-comments (§6.1.11.2) | sec, perf, devex, style, test, doc |
| `fix(core/layer)` | §6.2 | green (subset), impl-review-light (§6.2.5) | sec, perf, style, test |
| `fix(test)` | §6.2 (skip ADR, skip review-loop) | green | style, test |
| `chore(deps)` | §6.3 | — (lineal, conditional branch) | smoke + (sec/perf si dep sensible) |
| `test:` | §6.4 | — | style, test |
| `docs:` | §6.5 | — | doc |
| `perf:` | §6.7 | apply-loop (§6.7.3) | sec, perf, style, test |
| `security wave` | §6.8 | per-finding fork (§6.2 sub-loop), consolidation (§6.8.3) | sec, perf, style, test, doc |
| `refactor` | §6.6 | — (revert-on-diff regla §6.6.4) | style, test |
| `ai/skill`, `ci/infra` | §6.10 | escalado a humano | caso por caso |
| `[translation]` | §6.5 simplificado | — | doc (link check) |

### 14.2 ¿Qué bundle necesito si la subtarea es...?

| Subtarea | Required | Forbidden |
|----------|----------|-----------|
| ADR draft / revisión | `<adr_format>`, `<style_guide>`, `<event_sourcing_patterns>`, `<devex_jess>` | — |
| ADR security review | `<security_baseline>`, `<adr_format>` | `<perf_baseline>`, `<devex_jess>` (separación) |
| ADR perf review | `<perf_baseline>`, `<adr_format>` | `<security_baseline>`, `<devex_jess>` |
| ADR devex review | `<devex_jess>`, `<adr_format>` | `<security_baseline>`, `<perf_baseline>` |
| Architecture design | `<event_sourcing_patterns>`, `<style_guide>`, `<adr_format>` | — |
| Test spec design | `<qa_rubric>`, `<test_patterns>` | — |
| Test scaffold writing | `<test_patterns>`, `<neo_prelude>` | — |
| Implementation per-function | `<neo_prelude>`, `<event_sourcing_patterns>` | `<ghc_internals>` (salvo bridge) |
| Impl security review | `<security_baseline>`, `<event_sourcing_patterns>` | — |
| Impl perf review | `<perf_baseline>`, `<event_sourcing_patterns>` | — |
| Cross-vendor review | `<security_baseline>` + `<perf_baseline>` | — |
| PR body | `<community_voice>` | `<event_sourcing_patterns>` (separación docs vs código) |
| hlint / style polish | `<style_guide>` | — |
| Docs draft | `<community_voice>` | — |
| Threat model | `<security_baseline>`, `<event_sourcing_patterns>` | — |
| Parser/syntax impl | `<ide_layer>`, `<neo_prelude>` | `<ghc_internals>` (salvo bridge) |

### 14.3 ¿Qué modelo uso si la subtarea es...?

| Tipo de subtarea | Modelo objetivo | Notas |
|------------------|-----------------|-------|
| ADR architect (interview/draft) | Pro | Reasoning sobre decisiones, alternativas |
| ADR review (sec/perf/devex) | Flash 3 | YAML estructurado |
| Architecture design | Sonnet | Riesgo medio, decisiones de placement |
| Test spec design | Sonnet | Cobertura exhaustiva |
| Test scaffold | Flash 3 / Haiku | Casi mecánico desde spec |
| Implementation skeleton | Flash 3 | A partir de stubs |
| Per-function impl (típica) | Flash 3 | |
| Per-function impl (parser/syntax/type-level) | Sonnet | |
| Build-test diagnose | Sonnet | Razonamiento sobre errores |
| Single-edit fix | Flash 3 | |
| Impl security review | Sonnet | Razonamiento sobre vulnerabilidades |
| Impl perf review | Sonnet | Razonamiento sobre hot paths |
| Fix-findings | Flash 3 | Mecánico desde finding |
| Cross-vendor review | Pro | Segunda opinión cualificada |
| hlint / style polish | Haiku | Trivial |
| PR body | Haiku | A partir de plantilla |
| Dep bump | Haiku | Trivial |
| Docs draft | Haiku | Salvo API-facing → Sonnet |
| Translation | Flash | Paralelizable |

### 14.4 Verificadores por tipo de hoja (default)

| Tipo de hoja | Verificador default |
|--------------|---------------------|
| Modifica `*.hs` (single edit) | `cabal-build-ok` + `grep-anti-patterns` |
| Modifica `*.hs` (impl) | `cabal-build-ok` + `cabal-test-affected-suites` + `grep-anti-patterns` |
| Modifica test files | `cabal-build-ok` + `cabal-test-ok` |
| Read-only summary | `structured-summary` (schema-verifier) |
| Read-only review | `findings-yaml-schema` o equivalente |
| Cabal config | `cabal-build-ok` |
| Doc draft | `link-check` + `spell-check` + `jess-language-grep` |
| Benchmark | `numeric-baseline-stored` (apply) o `numeric-improvement-or-flat` (verify) |
| ADR file | `adr-schema-valid` |
| Architecture YAML | `architecture-schema-valid` |
| Threat model | `threat-model-v1` schema |
| PR body | `pr-body-schema` |
| Cross-vendor review output | `approve-yaml` |

### 14.5 Cuándo NO necesito ADR

- Bug fix trivial (typo, off-by-one, missing import).
- Performance improvement sin cambio de API pública.
- Documentation update.
- Adding new commands or entities (lógica de dominio, no infra).
- Dependency bump.
- Test addition pura.
- Translation.
- Refactor sin cambio de API pública.

Para todos los demás → ADR via §6.1.1 ADR-design-loop.
