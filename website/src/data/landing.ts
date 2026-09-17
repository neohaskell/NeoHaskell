export const landingContent = {
  eyebrow: "NeoHaskell · software after AI",
  headline: ["Keep the meaning.", "Change the software."],
  intro:
    "A language and framework for software you can keep understanding. Make your rules explicit, preserve the history behind each change, and build with a coding agent while keeping your intent in view.",
  primaryCta: {
    label: "Read the docs",
    href: "/docs/",
  },
  secondaryCta: {
    label: "Assess fit and tradeoffs",
    href: "/start/fit-and-tradeoffs/",
  },
  shift: {
    eyebrow: "When code is abundant",
    title: "When code is cheap, meaning becomes the scarce part.",
    body:
      "AI can turn an idea into code quickly. The harder question is whether that code represents the right rules. NeoHaskell gives each change a visible path: someone makes a request, the application checks its rules, and an accepted fact records what happened. You and your agent have something concrete to discuss and check.",
  },
  history: {
    eyebrow: "History as a foundation",
    title: "Keep the facts behind the current answer.",
    body:
      "A balance, booking status, or approval state is a summary, not the whole story. Event sourcing keeps meaningful accepted events as facts and reconstructs current state from them. A correction adds an explicit event, leaving the earlier mistake visible; a new query can ask a different question of the same history. What gets remembered, retained, and migrated is still a design responsibility.",
    caption:
      "Illustrative: a current balance is derived from retained deposits, withdrawals, and corrections.",
  },
  model: {
    eyebrow: "A shared model",
    title: "Share one model from idea to code.",
    body:
      "Event Modeling puts actors, commands, decisions, entities, events, and queries in a language people can discuss before implementation. NeoHaskell gives those ideas executable counterparts, while the Neo IDE graph helps you explore how the model connects to source. A shared vocabulary makes a missing step, refusal, or boundary easier to question.",
  },
  slices: {
    eyebrow: "Growth by slices",
    title: "Add one complete capability at a time.",
    body:
      "A slice follows one useful behaviour from trigger and command through decision, event, and query. It gives an agent a bounded task and a human a concrete result to check. Stable contracts can make growth easier to reason about, while shared events still create real dependencies that deserve deliberate coordination.",
  },
  tools: {
    eyebrow: "Executable foundations",
    title: "Use the language, framework, CLI, and visual IDE together.",
    body:
      "An approachable Haskell dialect gives your model typed foundations. The framework connects commands, events, state, views, and integrations. Use Neo to create, build, run, and test your own project; explore its connections in the visual IDE. The compiler checks structure, and tests check chosen examples. You still decide whether the rules are right.",
  },
  audiences: [
    {
      title: "For builders",
      body:
        "Bring an application idea—perhaps a booking, membership, grant, order, or other long-lived process. Start with a rule you can explain, then delegate one bounded slice and inspect the evidence.",
      href: "/start/a-shop-on-paper/",
      label: "Model a small slice",
    },
    {
      title: "For evaluators",
      body:
        "Consider NeoHaskell where history, changing rules, and accountability matter. Explicit models and bounded changes can help teams share knowledge and review work. Weigh that against the cost of modeling, storage, and evolving event contracts. A static site or disposable script may need less machinery.",
      href: "/start/fit-and-tradeoffs/",
      label: "Assess the tradeoffs",
    },
    {
      title: "For contributors",
      body:
        "Help improve the public language, framework, CLI, visual IDE, documentation, and integrations. Learn the foundations, make a focused change, and leave the model clearer for the next person and agent.",
      href: "/operate/contributing/",
      label: "See how to contribute",
    },
  ],
  closing: {
    title: "Keep the rules visible as software changes.",
    body:
      "Read the ideas in order, try a no-install modeling exercise, or follow a practice project from its first slice into operation. The documentation keeps the benefits, evidence, costs, and limits together so you can decide what fits.",
    label: "Start with the docs",
    href: "/docs/",
  },
} as const;
