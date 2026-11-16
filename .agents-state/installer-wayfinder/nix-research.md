# Investigación de instalación de Nix, cachés y plataformas

Fecha de consulta: 2026-11-16. Código revisado en `033ae4a570686cd885ca14c6595997d9498d5287`.

## Resumen

El CLI `neo` necesita `nix` en `PATH`. `neo run`, `neo build` y `neo test` entran al proyecto con `nix develop`. Los flakes del repositorio y del proyecto generado declaran los mismos dos cachés públicos. La flake puede pedir permiso para usarlos, pero no sustituye una configuración administrada por el sistema ni elimina las reglas del daemon. [Código del CLI](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/neo/src/subprocess/nix.rs), [flake raíz](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/flake.nix), [plantilla de proyecto](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/neo/assets/templates/flake.nix.j2).

La recomendación técnica es detectar y conservar una instalación existente. Para una instalación nueva, presentar un camino por sistema y arquitectura, y respetar NixOS y nix-darwin como configuraciones administradas. Determinate es el camino con cobertura explícita de WSL2 y flake habilitado, pero no cubre Intel macOS en su matriz actual. El instalador oficial de Nix cubre las dos arquitecturas macOS y los Linux indicados por Nix, aunque quien lo use debe habilitar `nix-command` y `flakes`. Lix es otra implementación y tiene instalador propio. Ninguna fuente consultada resuelve qué producto debe ser el valor predeterminado de NeoHaskell.

## Matriz de decisión

| Entorno | Compatibilidad documentada | Recomendación y límite |
| --- | --- | --- |
| Linux x86_64 o aarch64 | Nix soporta ambas arquitecturas. Su instalador usa daemon multiusuario con systemd. Sin systemd o con SELinux usa instalación de usuario único. Determinate y Lix documentan ambas arquitecturas. | Preferir modo multiusuario con systemd. Sin systemd, el instalador oficial conserva el modo usuario único. Determinate o Lix requieren `--init none`, que limita el uso a root o a usuarios que puedan elevar privilegios. [Nix](https://nix.dev/manual/nix/2.31/installation/installing-binary), [Determinate](https://github.com/DeterminateSystems/nix-installer/blob/main/README.md), [Lix](https://git.lix.systems/lix-project/lix-installer). |
| macOS aarch64 | Nix, Determinate y Lix documentan Apple Silicon. | Cualquiera de los tres encaja. Determinate recomienda su instalador gráfico para macOS. |
| macOS x86_64 | Nix y Lix documentan Intel. La matriz actual de Determinate lista solo Apple Silicon. El repositorio ya registró que Determinate dejó de publicar el instalador Intel y usó una versión fijada de Cachix para CI. | No seleccionar Determinate para Intel sin verificar soporte de la versión elegida. [Cambio histórico de CI](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/docs/changes/013-release-intel-nix.md). |
| WSL2 x86_64 o aarch64 | Determinate y Lix documentan WSL2. Nix upstream documenta Linux, pero no declara WSL2 de forma explícita. | Habilitar systemd en WSL2 antes de instalar. Microsoft requiere WSL 0.67.6 o más reciente para systemd configurable. Se reinicia la distribución con `wsl.exe --shutdown`. Sin systemd, los instaladores Determinate y Lix ofrecen `--init none`, con la limitación de privilegios indicada arriba. [Microsoft](https://learn.microsoft.com/en-us/windows/wsl/systemd), [Determinate](https://github.com/DeterminateSystems/nix-installer/blob/main/README.md). |
| WSL1 o Windows nativo | WSL1 no ofrece systemd ni kernel Linux completo. Ninguna matriz de Determinate o Lix lo soporta. Nix upstream no declara Windows ni WSL1. | No anunciar soporte. WSL2 es el límite documentado. [Microsoft](https://learn.microsoft.com/en-us/windows/wsl/wsl2-about). |
| NixOS | El sistema administra Nix y `/etc/nix/nix.conf` mediante opciones declarativas. | No ejecutar un instalador genérico. Cambiar `nix.settings` en la configuración NixOS y aplicar con `nixos-rebuild`. [Módulo NixOS](https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/config/nix.nix). |
| nix-darwin | `nix.enable` controla si nix-darwin administra Nix, el daemon y `/etc/nix/nix.conf`. Cuando está apagado, `nix.settings` no administra la instalación. | Si nix-darwin es dueño de Nix, cambiar su configuración `nix.settings`. Si no, no sobrescribir la instalación ajena. [Módulo nix-darwin](https://github.com/nix-darwin/nix-darwin/blob/master/modules/nix/default.nix). |

## Cómo funcionan los cachés

`flake.nix` y `neo/assets/templates/flake.nix.j2` declaran `https://cache.iog.io` y `https://neohaskell.cachix.org`, con estas claves públicas:

- `hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=`
- `neohaskell.cachix.org-1:mo2cLaGbwqbrxs9xhqKK8jeNsn3osi7t6XoAmxSZssc=`

Nix usa sustitutos si están disponibles y `substitute` sigue habilitado. Exige una firma reconocida por `trusted-public-keys`, salvo que se cambie esa política. Los flakes pueden solicitar `extra-substituters` y `extra-trusted-public-keys`. Nix pide consentimiento para esa configuración porque `accept-flake-config` tiene valor predeterminado `false`. [Manual de Nix sobre flakes](https://nix.dev/manual/nix/2.31/command-ref/new-cli/nix3-flake.html), [opciones de `nix.conf`](https://nix.dev/manual/nix/2.35/command-ref/conf-file).

En una instalación multiusuario, el daemon conserva una frontera de confianza. Los usuarios no confiables solo pueden elegir sustituters incluidos en `trusted-substituters`. Añadir alguien a `trusted-users` equivale esencialmente a darle acceso root. Para NixOS o nix-darwin, configurar en el dueño del sistema los dos URLs, ambas claves y, si el daemon necesita permitir la selección del usuario, la lista acotada de `trusted-substituters`. No añadir el usuario de la aplicación a `trusted-users` ni desactivar la verificación de firmas. [Manual de Nix](https://nix.dev/manual/nix/2.35/command-ref/conf-file), [módulo NixOS](https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/config/nix.nix).

El manual de Nix da una lista vacía por defecto para las funciones experimentales y clasifica `nix-command` y `flakes` como tales. Por eso no se debe asumir que una instalación upstream ya las habilitó. Neo requiere `extra-experimental-features = nix-command flakes` antes de poder usar `nix develop`. Determinate habilita ambas opciones y Lix declara flakes habilitados por defecto. La flake del proyecto no puede pedir esta configuración antes de que el usuario ya pueda ejecutar comandos de flakes. [Manual de Nix](https://nix.dev/manual/nix/2.35/command-ref/conf-file), [Determinate](https://github.com/DeterminateSystems/nix-installer/blob/main/README.md), [Lix](https://git.lix.systems/lix-project/lix-installer).

## Evidencia de uso real

`.github/workflows/cachix-push.yml` realiza los perfiles `ci` y `default` en Linux, y `default` en macOS. Después los publica en Cachix y consulta el perfil y cada ruta de su cierre contra los cachés públicos. Esta es una verificación real de que el contenido publicado está disponible en los cachés para Linux x86_64 y macOS Apple Silicon. No verifica las cuatro combinaciones que las flakes declaran (`x86_64-linux`, `aarch64-linux`, `x86_64-darwin`, `aarch64-darwin`). [Workflow en el SHA revisado](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/.github/workflows/cachix-push.yml), [sistemas de las flakes](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/flake.nix).

La consulta confirma presencia de rutas, no que un cliente frío las haya sustituido en vez de compilar. ADR-0072 describe una verificación histórica que medía uso de sustitutos por separado, pero declara retirado el SLO el 2026-10-22. No encontré evidencia de una ejecución reciente del workflow ni una prueba vigente de sustitución en una máquina limpia. Para reclamar uso efectivo, conservar la salida de una ejecución con store vacío que muestre descargas desde el caché y registrar las rutas consultadas. [ADR-0072](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/docs/decisions/0072-neo-cutover-and-onboarding-slo.md).

## Contexto histórico y desconocidos

El README histórico del instalador indica que delegaba la instalación de Nix a Determinate. ADR-0072 dice que el instalador y el SLO se retiraron. Esto describe el diseño anterior, pero no demuestra que deba reinstaurarse ese valor predeterminado. [README histórico](https://github.com/neohaskell/NeoHaskell/blob/7bd77c0fd88a4b56091fc015388df85837e97d96/installer/README.md), [ADR-0072](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/docs/decisions/0072-neo-cutover-and-onboarding-slo.md).

No se confirmó WSL1, Windows nativo, el estado actual de cada caché por arquitectura ni la política de configuración que usan los usuarios de nix-darwin. Tampoco se midió una sustitución de caché en este estudio. El workflow que publica Cachix solo cubre dos de las cuatro arquitecturas de las flakes.
