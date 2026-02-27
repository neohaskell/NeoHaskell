---

# Review of the NeoHaskell Documentation Plan

**Reviewer: Julia Evans**
**Date: February 2026**

---

## Preface: What I'm Bringing to This Review

I make zines that teach people about systems programming, networking, SQL, and other topics that are traditionally taught in dry, intimidating ways. My whole thing is: **hard things can be fun**, and the moment you make something visible that was previously invisible, people's eyes light up and they go "OHHH! That's what that means!" I've spent years figuring out how to create those moments.

I've read the full DOCS-PLAN.md, the research synthesis, the Svelte and Go analyses (which are closest to what I care about — interactive teaching and simplicity), and Steve Klabnik's review. Steve's review is excellent — he brings real "I shipped a famous programming book" experience and catches structural problems I wouldn't have caught. I agree with most of his points, especially about the tutorial layer system being overengineered, the Bloom's taxonomy problem, and the missing checkpoints.

But Steve and I care about different things. Steve cares about **structural soundness** — is the plan architecturally correct? Will it scale? Does it contradict itself? I care about **the reader's emotional experience** — will they feel excited? Will they feel lost? Will they feel like someone is on their side? Will they have fun?

This plan is impressively thorough. It's also, in several important ways, **not fun enough.** It reads like it was designed by someone who deeply understands pedagogy but has forgotten what it feels like to be confused. Let me explain what I mean.

---

## I. The Plan Describes Teaching But Doesn't Describe Delight

**Responding to: The plan as a whole, but especially the Guiding Principles (lines 9-17), the Tutorial Progression (lines 128-136), and the Exercise Strategy (lines 492-523)**

The plan has seven guiding principles. They're all correct. They're also all *structural*: "Learner before structure," "Ship value early," "If it doesn't compile, it doesn't ship." There is no guiding principle about **how the reader should feel**.

When I design a zine page, my first question isn't "what's the learning objective?" It's "what's the moment where the reader goes 'wait, REALLY?!'" The plan calls these "wow moments" — and to its credit, it lists them in the tutorial progression table:

> Part 1: "That's 12 lines of code?"  
> Part 2: "The compiler just prevented a real bug"  
> Part 3: "A bank statement IS a projection"  
> Part 6: "I accidentally built a compliance engine"

These are great! They're genuinely exciting hooks. But here's the problem: **they're listed in a planning table and never mentioned again.** There's no section that says "here's how we actually CREATE the wow moment." The plan describes *what* the reader should feel but not *how* the writing will produce that feeling.

### Why this matters: the difference between a correct tutorial and a delightful one

Let me give you a concrete example from my own work. In my zine about DNS, I could have written:

> "DNS queries use UDP by default but fall back to TCP for responses larger than 512 bytes."

That's correct. It's also boring. What I actually wrote was something more like:

> "Wait — DNS uses UDP?! That means your DNS query could just... get lost? And nobody would tell you? Yes! That's exactly what happens! And here's the wild part: your computer just... tries again."

The second version creates an emotional reaction. It makes the reader feel like they're discovering something surprising alongside me. It's not just information transfer — it's *shared curiosity*.

The NeoHaskell docs plan describes a tutorial that *transfers information correctly*. It does not describe a tutorial that *makes the reader feel like they're discovering something wild.* The banking domain has SO MUCH potential for this:

- "Wait — so when I check my bank balance, the bank doesn't just look it up in a field somewhere? It CALCULATES it? Every time?!"
- "Hold on — every bank on Earth has been doing event sourcing since literally before computers existed? This isn't a new fancy architecture pattern? It's the OLDEST pattern?"
- "So you're telling me that audit trails, time-travel debugging, and undo are all the SAME THING — just replaying events to different points?"

These are genuine "holy crap!" moments that the plan's domain makes possible. But the plan never talks about how to write prose that produces them.

### My specific recommendation

Add an 8th guiding principle:

> **8. Delight is a teaching strategy.** Surprise, humor, and "wait, really?!" moments aren't decoration — they're how insights stick. Every tutorial page should have at least one moment where the reader feels genuinely surprised by what they just learned. If a page doesn't produce that feeling, it's not done yet.

And in the tutorial progression table, expand the "wow moment" column into actual prose guidelines. For each part, write out: What's the surprising thing? How do we set it up? What does the reader expect, and how does reality differ from their expectation? The wow moment should be *designed*, not just hoped for.

---

## II. Where Are the Pictures?! (This Is Not the Same Point as Steve's)

**Responding to: The entire plan, which contains zero visual specifications, and the Interactive Elements section (lines 567-583)**

Steve already flagged the absence of visual aids (his Section V) and recommended an event-flow SVG diagram. He's right. But I want to push much harder on this, because I think the plan's relationship to visual explanation reveals a deeper problem: **the plan treats visuals as supplementary when they should be primary.**

### What I mean by "visual explanation"

When I say "visual," I don't just mean "add a diagram." I mean: **design the explanation visually first, then add prose to support it.** This is the opposite of how most docs are written (write prose, then maybe add a diagram).

Here's why this matters for event sourcing specifically. Event sourcing is a **temporal** concept. Things happen in order. State changes over time. The current balance is the result of everything that happened before. This is *inherently visual* — it's a timeline! And yet the plan describes teaching it entirely through code and prose.

Let me describe what I would actually design for Tutorial Part 1, as a zine page:

```
┌─────────────────────────────────────────────────────────┐
│                                                         │
│   CRUD world:           Event Sourcing world:           │
│                                                         │
│   ┌──────────┐          ┌──────────────────────────┐    │
│   │ balance:  │          │ 1. AccountOpened          │    │
│   │   $150   │◄── ???    │ 2. MoneyDeposited $100    │    │
│   │          │          │ 3. MoneyDeposited $50     │    │
│   └──────────┘          └──────────────────────────┘    │
│                                ↓ fold                    │
│   Where did $150              $0 + $100 + $50 = $150    │
│   come from?                                             │
│   WHO KNOWS!                  Oh! THAT'S where it        │
│                               came from!                 │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

This is a terrible ASCII approximation of what I'd actually draw, but the point is: **the visual IS the explanation.** The prose underneath this image would be three sentences. The image does the heavy lifting. The reader looks at it and the entire concept of "state derived from events" clicks *before they read a single line of code.*

### Specific visual opportunities the plan misses

The plan's Rosetta Stone table (lines 138-146) is a TABLE. It maps "Something happened" → "Transaction" → "Event" → "An immutable record of a fact." This is useful reference material. But it should ALSO be a visual:

I would draw the NeoBank as a **little comic** showing Alex at a bank counter:

- Panel 1: Alex says "I want to deposit $50" → that's a **Command** (the deposit slip)
- Panel 2: The teller writes it in the ledger → that's an **Event** (the transaction)
- Panel 3: Alex asks "What's my balance?" → The teller reads through the whole ledger and adds it up → that's a **Projection** (the account statement)
- Panel 4: Alex's face: "Wait... you ADD IT UP every time? From ALL the transactions?" Teller: "How else would you know it's correct?"

This comic would take 10 minutes to sketch and would teach the core mental model more effectively than any amount of prose. It works because it **makes the invisible visible** — it shows the reader what's actually happening inside the system by mapping it to a physical process they already understand.

### The timeline visualization is too important to be Phase 2

The plan's Interactive Elements section (lines 567-583) lists "Interactive transaction timeline — click a deposit/withdrawal, see balance recalculate in real-time" as a **Phase 2 enhancement**. Steve says (correctly) that a static SVG is 80% of the value at 10% of the cost. I want to go further: **the event timeline should be the FIRST visual element designed, before any prose is written.**

Here's what the event timeline should look like (static version):

```
Time ──────────────────────────────────────────────►

  AccountOpened    MoneyDeposited    MoneyDeposited    MoneyWithdrawn
       │               $100              $50              $30
       ▼                ▼                 ▼                ▼
   ┌───────┐       ┌───────┐        ┌───────┐       ┌───────┐
   │  $0   │  ──►  │ $100  │  ──►   │ $150  │  ──►  │ $120  │
   └───────┘       └───────┘        └───────┘       └───────┘
                                         ▲
                                    "What's my
                                     balance?"
                                    ═══════════
                                    THIS is a
                                    projection!
```

Every single tutorial part could reference back to this timeline, adding new events to it. By Part 6 (Audit Everything), the timeline is long and the reader can SEE what "replay" means: go back to any point in the timeline and recalculate the balance from there.

### My specific recommendation

1. **Add "Visual Design" as a Phase 0 task**, alongside code verification infrastructure. Design the core visual assets before writing prose: the event flow diagram, the event timeline, and the CRUD-vs-events comparison visual. These should be SVGs, reusable across tutorial parts.

2. **Add a visual specification to the Concept Page template** (lines 393-418). Currently the template has: One-Sentence Version → In NeoBank Terms → Full Picture → How NeoHaskell Enforces This. Add a required field: **"## The Picture"** — a visual that explains this concept without words. Not every concept page needs one, but the template should DEMAND that the writer consider one. If the writer can't draw it, maybe they don't understand it well enough yet.

3. **Create a "comic panel" format** for the Rosetta Stone (lines 138-146). The table is good reference material; keep it. But for Tutorial Part 1, also show the banking analogy as a visual story. This doesn't require an illustrator — simple boxes and arrows and stick figures work fine.

4. **Move the transaction timeline from Phase 2 Interactive Elements to Phase 0 Foundation** — as a static SVG, not an interactive widget. It should appear in Tutorial Part 1 and grow through Part 6.

---

## III. The "Implicit-First" Language Teaching Strategy Will Fail Without a Safety Net for Confused Readers

**Responding to: Guiding Principle #7 (line 17), the Tutorial Layer System (lines 179-232), and the "What We're NOT Doing" section (lines 763-766)**

The plan's position on teaching NeoHaskell syntax is:

> "The tutorial teaches the language implicitly. Concept pages teach it explicitly."
> "No one reads a grammar chapter before cooking dinner."

I love this principle. The cooking dinner analogy is perfect. And the research supports it — the Svelte analysis notes that Svelte's tutorial teaches reactivity by USING `$state`, not by explaining what `$state` is first.

**But here's where I worry, based on making hundreds of zine pages about unfamiliar syntax:** The plan assumes readers will absorb unfamiliar syntax "by doing." This works when the syntax is *incrementally unfamiliar* — like, you know JavaScript and you're learning TypeScript, so `let x: number = 5` is just JavaScript-with-annotations. But NeoHaskell syntax is going to look **genuinely alien** to a JavaScript/Python developer. Consider what the reader sees in Part 1:

```haskell
handleCommand :: Command -> Account -> Result Error Event
handleCommand (Deposit amount) account =
  account
    |> validatePositiveAmount amount
    |> Result.map (\_ -> MoneyDeposited { accountId = account.id, amount = amount })
```

For a JavaScript developer, this code raises immediate questions:
- What does `::` mean? (type annotation)
- What's `Command -> Account -> Result Error Event`? (function type signature — and why are there so many arrows?)
- What are the parentheses around `(Deposit amount)`? (pattern matching on a constructor)
- What is `|>`? (pipe operator)
- What's `Result.map`? (mapping over a Result type)
- What's `\_ ->`? (lambda with unused parameter)
- Why are there no `return` statements? (expression-based language)

That's SEVEN unfamiliar things in SIX lines of code. The plan's Layer 2 sidebar (which Steve recommended dropping) was trying to solve this problem. Steve's alternative — bold the first occurrence and add a one-sentence parenthetical — is better, but I still think it underestimates how lost a JavaScript developer will feel.

### The real problem: what does the confused reader DO?

When I make a zine about, say, how `strace` works, and I show a command like `strace -e trace=write -p 1234`, I always think about the reader who looks at this and goes "wait, what is ALL of this?" My approach is to add **annotations** — little arrows pointing from each part of the command to a short explanation. The syntax becomes a LABELED DIAGRAM, not a block of code you're supposed to absorb by osmosis.

The plan doesn't describe what happens when a reader hits a block of NeoHaskell code and can't parse it. The tutorial narrative continues. The concept links are there but "optional." The syntax aside is collapsible and (per Steve's recommendation) might not exist. The reader is lost and the plan says, essentially, "keep going and it'll make sense later."

For some readers that WILL work. For others, it'll be the moment they close the tab.

### The "annotated code" pattern the plan almost has

The plan's Phase 0 includes `reading-neohaskell.mdx` — "5-minute annotated code walkthrough." This is described as "Take a real 20-line NeoHaskell snippet and annotate every line with what it does and why it looks like that." **This is exactly the right idea but it's in the wrong place.**

The annotation pattern shouldn't be a separate page — it should be INSIDE the tutorial, on the first code block where the reader encounters unfamiliar syntax. Here's what I mean:

```
handleCommand :: Command -> Account -> Result Error Event
─────────────    ───────    ───────    ─────────────────
function name    input 1    input 2    what it returns
                                       (success or error)

handleCommand (Deposit amount) account =
              ────────────────
              "if the command is a Deposit,
               grab the amount from it"
```

This is the "annotated code" format. The code is THERE, in context, in the tutorial, and the reader can see what each piece does. It's visual. It's inline. It doesn't require clicking anything. And it gradually fades away — by Part 3, you stop annotating `|>` because the reader already knows it, and you only annotate NEW constructs.

### My specific recommendation

1. **Create an "annotated code block" component** in Phase 0 (an Astro/Starlight component that renders code with visual annotations — arrows, labels, highlights). This is a CORE INFRASTRUCTURE piece, not a "nice to have."

2. **Use annotated code blocks for the FIRST occurrence of every significant code example in Tutorial Parts 1-3.** By Part 4, transition to regular code blocks with minimal annotation. By Part 6, no annotations — the reader should be fluent.

3. **Keep `reading-neohaskell.mdx` as an optional overview** (Steve's point about making it optional is correct), but also use the SAME annotation format inside the tutorial. The standalone page previews the tutorial's visual pattern.

4. **Design the annotation format to answer "what does this piece do?" for each syntactic element**, not "what is this language construct called." The reader doesn't care that `::` is a "type signature annotation." They care that it means "this function takes these inputs and returns this output." Explain function, not terminology.

5. **Steve recommended dropping Layer 2 (the syntax sidebar).** I agree, but with a caveat: replace it with annotated code blocks, not with nothing. The sidebar was solving a real problem (confused readers) with the wrong mechanism (a parallel information channel). Annotated code blocks solve the same problem with a better mechanism (visual explanation inline).

---

## IV. The Exercises Are the Right Types but the Wrong Emotional Tone

**Responding to: Exercise Strategy (lines 492-523), Exercise Types (lines 496-505), and Bloom's Taxonomy Progression (lines 510-523)**

Steve already made the case that Bloom's Taxonomy should be a retrospective lens, not a design constraint. I agree completely. My point is different: **the exercises are described as cognitive activities when they should be described as emotional experiences.**

Look at the exercise types (lines 496-505):

1. **Modify**: "Change the event type to include X. What happens when you compile?"
2. **Predict**: "What will this code output? Think first, then run it."
3. **Extend**: "Add a `DailyWithdrawalLimitSet` event."
4. **Break**: "Remove the type annotation. What error do you get? Why?"
5. **Compare**: "Write this same feature in [language you know]."
6. **Audit**: "Replay all events from scratch. Does the derived balance match?"

These are fine exercise *shapes*. But they're phrased as instructions, not invitations. Compare:

**Plan's version:** "Add a `DailyWithdrawalLimitSet` event. Wire it through the handler so withdrawals are declined above the limit."

**How I'd write it:** "Here's a challenge: what if Alex's bank has a daily withdrawal limit of $500? You've already seen how the compiler enforces overdraft protection — can you make it enforce a daily limit too? Hint: you'll need a new event type. Try it before looking at the solution!"

The second version does several things:
- It frames the exercise as a **challenge**, not an instruction. "Can you?" instead of "Do this."
- It connects to something the reader already accomplished. "You've already seen how..."
- It gives a hint without giving the answer.
- It acknowledges there IS a solution to look at if you get stuck.
- It creates a feeling of "ooh, can I figure this out?" instead of "ugh, homework."

### The "Break" exercises are the closest to fun — lean into that

The "Break" exercise type is my favorite in the list because it's inherently playful: "go ahead, BREAK it, see what happens!" This is the closest the plan gets to the spirit of my zines, where I often say things like "What happens if you run `kill -9` on PID 1? Let's find out! (spoiler: don't do this on a production server)"

The Compiler-as-Teacher pattern (lines 527-559) captures this well, and Steve praised it. But the plan limits Break-It exercises to **one per tutorial part** (line 553). That's too few! Breaking things is how you build intuition. Every time a reader triggers an error and understands WHY it happened, they've built a mental model that no amount of prose reading can create.

### The "Predict" exercise type is underused and uniquely powerful for event sourcing

Predict exercises ("What will this code output? Think first, then run it.") are PERFECT for teaching projections, because projections ARE predictions — you're deriving state from events. The reader who can mentally "fold" over a list of events and predict the resulting balance has UNDERSTOOD event sourcing at a deep level.

The plan maps Predict exercises to concept pages (line 507: "Concept pages: 1 Predict or 1 Break per page"). This is backwards. Predict exercises belong IN the tutorial, especially Parts 3 (Transaction History) and 6 (Audit Everything). Here's a specific example:

> **Quick challenge:** Here are five events that happened to Alex's checking account:
>
> 1. `AccountOpened`
> 2. `MoneyDeposited $100`
> 3. `MoneyDeposited $50`
> 4. `MoneyWithdrawn $30`
> 5. `MoneyDeposited $200`
>
> What's the current balance? Work it out in your head (or on paper!) before running the code.
>
> *If you got $320, you just did a projection in your head. That's literally all a projection is — folding over events to derive current state.*

This is an "aha moment" exercise. The reader realizes they already understand projections because they just DID one. No code required. And the reveal — "you just did a projection" — is the kind of delightful surprise that makes learning feel like discovery.

### My specific recommendation

1. **Rewrite all exercise descriptions in an inviting, challenge-based tone.** "Can you...?" instead of "Do this." "What happens if...?" instead of "Modify X." "Here's a puzzle:" instead of "Exercise 3."

2. **Increase Break-It exercises to 2-3 per tutorial part** (not one). Breaking things is fun, and it builds the error-literacy that the Compiler-as-Teacher pattern depends on. Make them feel like experiments, not tests.

3. **Move Predict exercises into the tutorial**, especially Parts 3 and 6. The "fold events in your head" exercise described above should be in Tutorial Part 3, not on a concept page. It's the most powerful "aha moment" available for teaching projections.

4. **Add "What If" exercises** — a type the plan doesn't include. "What if you needed to undo a deposit? In CRUD, you'd UPDATE the balance. In NeoHaskell, what would you do?" These force the reader to think in events and discover that the answer is "add a new event" — which is the core insight of the entire paradigm.

5. **Every exercise should have a visible solution** — either a "Show solution" toggle or a link to a solutions page. Exercises without solutions are frustrating. The Svelte analysis (svelte.md line 175) notes that Svelte's tutorial has a "Solve" button that "Encourages trying first" with the message "Try not to rely on it too much." That's the right psychology — solutions reduce anxiety, and the gentle nudge encourages effort.

---

## V. Error Messages Should Feel Like Detective Work, Not Punishment

**Responding to: Compiler-as-Teacher Pattern (lines 527-559), Common Errors page (line 645 in Phase 3 tasks), and Open Question #5 (line 789)**

The plan's Compiler-as-Teacher pattern is strong (Steve praised it, and he's right). But I want to push on the EMOTIONAL framing of errors. The plan treats errors as "teaching moments." I want to treat them as **mysteries to solve.**

### The difference between "teaching moment" and "mystery"

The plan's five-step pattern is:
1. Show correct code
2. Instruct reader to break it
3. Show the compiler error
4. Explain what the error means
5. Reader fixes it

This is pedagogically sound. But it's also procedural — do step 1, then step 2, then step 3. There's no room for the reader to feel CLEVER. Let me show you what I mean:

**Plan's approach (paraphrased):** "Replace the `do` block with `let..in`. You'll see this error: 'NeoHaskell uses do blocks for all bindings.' This means NeoHaskell enforces a single binding style for readability."

**My approach:** "Try replacing the `do` block with `let..in` (if you've used Haskell before, this might feel natural). Go ahead — run it!

*What did you see?*

If you got 'NeoHaskell uses do blocks for all bindings' — congratulations, you've just discovered one of NeoHaskell's opinions! NeoHaskell picks ONE way to bind variables, and it picks `do`. Why? Because when you're reading someone else's code at 2am, you don't want to wonder 'is this a `let..in` or a `where` or a `do`?' You want ONE pattern to recognize.

There are a few more of these opinions coming. When you hit one, think of it as NeoHaskell saying: 'I know there are three ways to do this. I picked one. Here's why.'"

The second version:
- Invites the reader to experiment, not follow instructions
- Frames the error as a DISCOVERY ("you've just discovered...")
- Explains the WHY in human terms ("at 2am reading someone else's code")
- Sets a pattern ("there are more of these opinions coming") that makes FUTURE errors feel familiar instead of surprising
- Makes the reader feel like they're learning NeoHaskell's personality, not memorizing its rules

### The "Common Errors" page should be a treasure map, not a lookup table

The plan mentions a "Common Errors" page in Phase 3 (line 645): "error message as heading, fix as content." This is useful but dry. The Svelte research (svelte.md lines 224-231) notes that Svelte has dedicated pages for compiler errors, warnings, and runtime errors, each with explanations and fixes.

I'd go further. The Common Errors page should be organized as a **decision tree**, not a flat list:

> **Something went wrong? Let's figure it out!**
>
> **What happened?**
> - My code won't compile → [Compiler errors](#compiler-errors)
> - My code compiles but does the wrong thing → [Logic errors](#logic-errors)
> - I got a runtime error → [Runtime errors](#runtime-errors)
> - The linter is complaining → [Linter warnings](#linter-warnings)
>
> **Compiler errors:**
> - Is the error about types? → [Type errors](#type-errors)
> - Is the error about missing things? → [Scope errors](#scope-errors)
> - Is the error about NeoHaskell conventions? → [Convention errors](#convention-errors)
> - I can't figure out what category it is → [Search by error message](#search)

This makes debugging feel like following a trail of clues instead of scanning a wall of text. It also teaches readers to CATEGORIZE errors, which is a skill that transfers to any language.

### My specific recommendation

1. **Reframe the Compiler-as-Teacher pattern's emotional tone** from "teaching moment" to "discovery." The five steps are correct; the framing should shift from instructional to investigative. "Let's see what happens when..." instead of "Now break the code by..."

2. **Design the Common Errors page as a decision tree**, not a flat lookup table. Make debugging feel like detective work.

3. **Add a "Reading Error Messages" micro-guide** in Getting Started — literally a visual that labels the parts of a NeoHaskell error message (error code, source location, the actual problem, the help text, the suggestion). This is like my approach to teaching `strace` output — before you can use the tool, you need to know how to READ its output. A labeled diagram of an error message is worth a thousand words of explanation.

4. **Resolve Open Question #5 honestly.** Steve flags this too: if NeoHaskell's error messages aren't actually friendly yet, don't pretend they are. Show the REAL error output and add prose explanation. Readers who see a documented ugly error feel better than readers who expect a friendly error and get a cryptic one.

---

## VI. The Plan Is Missing "What Happens When" Explanations — The Pattern That Makes Invisible Things Visible

**Responding to: Core Concepts section (lines 371-427), the Concept Page Template (lines 393-418), and the absence of "runtime behavior" explanations throughout**

This is the point I feel most strongly about, and it's something Steve didn't raise.

When I teach networking in my zines, one of the most powerful tools I have is "what happens when you type google.com into your browser?" The reader knows the input (typing a URL) and the output (a webpage appears), but they have NO IDEA what happens in between. The magic is in showing the hidden steps: DNS lookup → TCP handshake → TLS negotiation → HTTP request → server processing → HTTP response → rendering. Each hidden step is a learning opportunity.

Event sourcing has the EXACT SAME opportunity, and the plan doesn't exploit it.

### "What happens when you deposit $50?"

Here's what the plan SHOULD include, probably as a standalone concept page or a section of Tutorial Part 1, as a step-by-step "what happens when" walkthrough:

> **What happens when you deposit $50 into your NeoBank account?**
>
> Let's trace through exactly what NeoHaskell does, step by step:
>
> **Step 1:** Your code calls `deposit 50 alexAccount`. This creates a `Command`:
> ```
> Deposit { accountId = "alex-checking", amount = 50 }
> ```
> *This is a REQUEST, not a fact. Nothing has changed yet.*
>
> **Step 2:** NeoHaskell passes the command to your `handleCommand` function, along with the current state of the account. Your function checks: is $50 a valid deposit? (Is it positive? Is the account open?)
>
> **Step 3:** The validation passes! Your function returns an `Event`:
> ```
> MoneyDeposited { accountId = "alex-checking", amount = 50 }
> ```
> *THIS is the fact. This happened. It's immutable. It's going into the event store.*
>
> **Step 4:** NeoHaskell appends the event to the event store. The event store now looks like:
> ```
> [AccountOpened, MoneyDeposited 100, MoneyDeposited 50]  ← new!
> ```
>
> **Step 5:** The next time someone asks "what's Alex's balance?", NeoHaskell replays ALL the events:
> ```
> $0 → +$100 → +$50 → $150
> ```
>
> **That's it. That's event sourcing.**

This "what happens when" format makes the invisible machinery of event sourcing VISIBLE. The reader can SEE the data flowing through the system. They can SEE that the command is separate from the event. They can SEE that the balance is derived, not stored.

### The plan's concept page template misses this

The concept page template (lines 393-418) has:
- The One-Sentence Version
- In NeoBank Terms
- The Full Picture
- How NeoHaskell Enforces This
- Going Deeper

None of these sections specifically calls for a "what happens when" trace. "The Full Picture" is the closest, but it's described as "Complete explanation with code examples." Code examples show WHAT the code looks like. A "what happens when" trace shows HOW the system behaves at runtime.

### My specific recommendation

1. **Add a "## What Happens When..." section to the concept page template**, between "In NeoBank Terms" and "The Full Picture." This section traces through a concrete scenario step by step, showing the data at each stage. It's the section that makes the invisible visible.

2. **Write "What Happens When You Deposit $50?" as a canonical example** — either as a standalone concept page or as a section of Tutorial Part 1. This should be one of the FIRST things written, because it forces the writer to understand the runtime behavior deeply enough to explain it step by step.

3. **Use the "what happens when" format for the most confusing concepts**: "What happens when a withdrawal is declined?" (showing how the command handler returns an error), "What happens when you transfer money between accounts?" (showing two aggregates and the coordination), "What happens when you replay events?" (showing the fold).

4. **These traces should be heavily visual** — not just prose with code blocks, but annotated timelines showing data flowing from step to step. Each step should show the BEFORE and AFTER state.

---

## VII. The Plan Doesn't Handle the "I'm Lost" Moment

**Responding to: The tutorial design as a whole, the absence of any "stuck reader" strategy, and the Iteration Protocol (lines 667-689)**

The Iteration Protocol (lines 667-689) says: "After Every Page: Can a fresh reader follow it without help? (test with 1 person)." This is good! But what about the reader who CAN'T follow it without help? What do they do?

The plan has:
- Layer 3 deep-dive links (optional)
- A Discord community (mentioned in Misconception #20)
- A "Common Errors" page (Phase 3)
- Exercises (but no solutions are specified)

What the plan DOESN'T have:
- **An explicit "If you're stuck" section on tutorial pages** — "If this doesn't make sense, here are 3 things to try: (1) re-read the code annotations, (2) go to the REPL and experiment, (3) check the common errors page, (4) ask in Discord."
- **A visible "Get help" button or link** — always available, always pointing to the right resource for the current page.
- **Solutions for every exercise** — the plan mentions exercises but never specifies that solutions should be available. The Svelte analysis (svelte.md lines 173-177) specifically highlights the "Solve" button as critical UX.
- **"If you see X instead" troubleshooting** in checkpoints — Steve recommends this in his checkpoint template, and I want to underline it: the MOST VALUABLE part of a checkpoint is the troubleshooting section. It's the plan for when things go wrong.
- **A "Don't worry" reassurance pattern** — when the tutorial introduces something complex (like Part 5's cross-aggregate transfers), a brief acknowledgment that "this is the hardest part of the tutorial, and it's okay if it takes a few reads to click" reduces anxiety enormously.

### Why "I'm lost" handling matters more than getting the teaching right

Here's a thing I've learned from making zines and watching people read them: **you can't prevent confusion.** No matter how clear your explanation is, some percentage of readers will be confused at some point. The question isn't "how do we prevent confusion?" (impossible) but "what does a confused reader DO?" If the answer is "close the tab," your documentation has failed regardless of how well it teaches.

The Go analysis (go.md lines 117-119) notes that Go has "No Gatekeeping — All docs are accessible from any entry point." Svelte's research shows the tutorial has a "Solve" button. The synthesis (Pattern 12) identifies progressive disclosure and expandable sections. All of these are strategies for the moment when the reader gets stuck.

### My specific recommendation

1. **Add an "If You're Stuck" section template** to the tutorial page design. At the bottom of every tutorial page (above "See Also"):

   > **Stuck?**
   > - [Re-read the annotated code above](#code-section) — the annotations explain each piece
   > - [Open the REPL and experiment](#verify-your-progress) — try changing values and see what happens
   > - [Check common errors](/guides/common-errors) — someone else probably hit this too
   > - [Ask in Discord](https://discord.gg/neohaskell) — we're friendly, we promise

2. **Every exercise MUST have a visible solution**, either as a `<details>` toggle or a "Show solution" button. No exceptions. The Svelte analysis confirms this is standard practice.

3. **Add "Don't worry" reassurances before hard sections.** Tutorial Part 5 should start with: "This is the most complex part of the tutorial. If transfers feel confusing at first, that's normal — you're learning to coordinate multiple aggregates, which is a genuinely new concept. Take your time, and don't hesitate to go back to Parts 1-4 to refresh."

4. **Steve recommended formal checkpoint/verification sections.** I want to add: the troubleshooting subsection of each checkpoint is MORE IMPORTANT than the happy-path verification. "If you see X instead" catches the reader who's already confused and gives them a lifeline. Design these troubleshooting lists by actually watching test readers get stuck (Phase 0's observation testing).

---

## VIII. The "NeoBank Sounds Like Enterprise Software" Conceit Is Delightful — But the Plan Doesn't Lean Into It Hard Enough

**Responding to: "The Scope" table (lines 113-123) and "The Opening Line of the Tutorial" (lines 165-170)**

The Scope table is one of the most genuinely fun parts of the plan:

| Sounds like | Actually is |
|---|---|
| Enterprise banking platform | A map of account IDs to event streams |
| Bloomberg terminal | A list of events, rendered chronologically |
| FinTech infrastructure | A fold over deposit/withdrawal events |
| Risk management system | A type-level constraint the compiler enforces |

This is GREAT. It's playful, it's surprising, and it creates exactly the kind of "wait, REALLY?!" moment I talked about in Section I. The contrast between the impressive-sounding left column and the simple right column is genuinely delightful.

But the plan treats this table as internal planning material. **This table — or something very like it — should appear IN the tutorial, probably at the end of Part 6.**

### The "big reveal" moment

Imagine the reader finishes Tutorial Part 6. They've built NeoBank. They have accounts, transactions, transfers, overdraft protection, and an audit trail. They feel good about it. And then the tutorial says:

> **Let's take a look at what you just built:**
>
> | What you built | What it's called in fintech |
> |---|---|
> | A map of account IDs to event streams | Multi-account management platform |
> | A list of events, rendered chronologically | Real-time transaction ledger |
> | A fold over deposit/withdrawal events | Balance reconciliation engine |
> | A type-level constraint | Overdraft risk management |
> | Two commands in two bounded contexts | Payment settlement network |
> | The event store you already have | Regulatory compliance audit trail |
>
> You didn't set out to build fintech infrastructure. You wrote about 200 lines of NeoHaskell. The event sourcing model made the impressive stuff *free.*

THIS is the "I accidentally built a compliance engine" wow moment from the progression table (Part 6). But the plan just lists it as a wow moment — it doesn't describe HOW to create it. The reveal table above IS the mechanism.

### My specific recommendation

1. **Include a "What You Built" reveal table at the end of Tutorial Part 6**, showing the reader what their ~200 lines of code would be called in the fintech industry. This is the emotional payoff for the entire tutorial.

2. **Echo this reveal in the tutorial's closing paragraph**: "You started with 'I want to deposit money.' You ended with a banking platform that has accounts, a ledger, reconciliation, risk management, a settlement network, and an audit trail. That's event sourcing. The complexity you didn't have to write is the point."

3. **Use this reveal to bridge to the second example domain** (logistics): "Now you know the pattern. Could you do the same for package tracking? A package moves through locations, status changes are events, delivery confirmation is a projection... sound familiar?"

---

## IX. The Plan Should Steal Svelte's "One Concept Per Lesson" Granularity

**Responding to: The Tutorial Progression (lines 128-136), the Svelte analysis (svelte.md lines 265-273), and Steve's pacing gap critique (his Section XI)**

The plan's tutorial has 6 parts. The Svelte tutorial has **48 lessons** for basic Svelte alone. The plan's Part 1 ("Your First Transaction") introduces: event types, command handlers, the event store, and balance-as-fold. That's four concepts in one tutorial part.

The Svelte analysis (svelte.md lines 265-271) specifically warns about this:

> NeoHaskell should match [Svelte's lesson size]: Don't try to teach "event sourcing" in one lesson. Break it down: (1) What is an event? (2) Storing events (3) Replaying events (4) Building state from events. Each lesson adds one small feature to NeoBank.

Steve flags the Part 4 → Part 5 jump as the biggest pacing problem. I think the pacing problem starts in Part 1. Four concepts in one part is too many, and it means the first tutorial page will be long, dense, and intimidating.

### What Part 1 should look like: 4 lessons, not 1 part

- **Lesson 1.1: "Hello, NeoBank"** — Create the project, run it, see output. ONE concept: project structure. Reader's feeling: "This works on my machine."

- **Lesson 1.2: "Your First Event"** — Define a `MoneyDeposited` event type. ONE concept: events as data types. Reader's feeling: "So an event is just a data structure?"

- **Lesson 1.3: "Storing Events"** — Add the event to the event store. Show the event list growing. ONE concept: append-only storage. Reader's feeling: "It's just a list? That I add to? That's it?"

- **Lesson 1.4: "What's My Balance?"** — Fold over events to derive the balance. ONE concept: state derived from events. Reader's feeling: "OHHH — the balance isn't stored, it's CALCULATED. That's the whole idea!"

Each lesson is short (5-10 minutes), has one checkpoint, introduces one concept, and ends with running code. The reader gets four small wins instead of one big slog. And the "aha moment" — the fold — hits at lesson 1.4, not buried in a long Part 1.

### My specific recommendation

1. **Break each tutorial "part" into 3-4 small lessons.** The current 6-part structure becomes 18-24 lessons. This matches the Svelte model (48 lessons for basic Svelte) and the FastAPI model (progressive enhancement, one concept per page).

2. **Keep the 6-part structure as "units" or "chapters"**, with lessons inside each. The sidebar shows: "Chapter 1: Your First Transaction" → Lessons 1.1, 1.2, 1.3, 1.4. This preserves the plan's narrative arc while making each individual page shorter and more focused.

3. **Add progress indicators** — the synthesis recommends this, and the Astro analysis mentions "Unit 1 (3/6 Complete)" as a pattern. For a 20+ lesson tutorial, knowing "you're on lesson 8 of 22" reduces the "how much longer?" anxiety that Steve mentions.

4. **Design each lesson to be completable in 5-10 minutes.** If a lesson takes longer, it's trying to teach too much — split it. This is the Svelte model, and it works because it creates **frequent wins.** Each lesson is a small dopamine hit: "I did it. I understand this. Next!"

---

## X. The Second-Person Voice Is the Right Choice, But the Plan Doesn't Commit to It

**Responding to: The "Alex" character (lines 109-111), the tutorial opening (lines 165-170), and the general tone throughout the plan**

Steve flags the "Alex" character risk — that readers stop identifying with Alex when their situation differs. He recommends letting Alex fade into second person ("you") by Part 2. I agree, and want to add something Steve didn't say: **the tone of the plan itself switches between "we" (the documentation team), "the reader" (third person), and "you" (second person) in ways that signal the plan hasn't decided who it's talking to.**

Look at these excerpts from the plan:
- "Every page must place **the reader** somewhere on this path" (third person about the reader)
- "**Alex** is the reader. Not a banker — a developer." (third person character)
- "**Let's** prove it works by building a bank." (first person plural)
- "**Your** NeoBank will handle accounts..." (second person)

The tutorial opening (lines 165-170) uses "Let's" and "your" — that's perfect. But the plan's internal language keeps saying "the reader," which creates a subtle emotional distance. When you write a plan that says "the reader" a lot, the resulting documentation tends to feel like it was written ABOUT the reader rather than FOR the reader.

### Why this matters for approachability

My zines are addressed directly to "you." I say "you" a lot. "When you run strace, you'll see..." "If you're confused by this, that's okay! Here's what's happening..." "You might be wondering why..."

This isn't just style — it's a teaching strategy. "You" creates a sense of being personally guided. "The reader" creates a sense of being observed by a curriculum designer. One feels like a friend explaining something; the other feels like a textbook.

### My specific recommendation

1. **Commit to second person ("you") as the tutorial voice from the start.** The opening line should address "you": "When you deposit $50, your bank doesn't find your balance and change it to $150." (Steve's improved version of the opening is perfect for this.)

2. **Drop "Alex" as a named character entirely** — or use Alex only in the tutorial intro page ("Meet Alex. Alex is you. Alex is building a bank.") and never again. The reader should BE the protagonist, not read about one. (I'm going slightly further than Steve here — he says let Alex fade by Part 2; I say Alex adds nothing after the very first page.)

3. **In the plan itself, replace "the reader" with "you" wherever possible.** This will change how the documentation team thinks about the audience. It's the difference between designing for an abstraction ("the reader") and designing for a person ("you").

---

## XI. Building on Steve's Points: Where I Agree, Disagree, or Add

Steve's review is thorough and I won't repeat his points, but I want to note where my perspective differs or adds:

### Steve's Layer 2 critique (his Section I): I agree, but with a different replacement

Steve says drop Layer 2 and use inline parentheticals. I say drop Layer 2 and use **annotated code blocks** (see my Section III). The difference: inline parentheticals work for a text-heavy tutorial. Annotated code blocks work for a code-heavy tutorial. Since NeoHaskell's tutorial will necessarily be code-heavy (you're teaching a programming language), the visual annotation approach is more appropriate.

### Steve's visual aids point (his Section V): I want to go much further

Steve says "add a visual aids requirement to the tutorial design constraints." I say **design visuals FIRST, then write prose to support them.** Steve's recommendation is additive (add diagrams to the existing plan). My recommendation is structural (make visual explanation a primary, not supplementary, teaching method). See my Section II for the full argument.

### Steve's checkpoint sections (his Section VI): I want to emphasize troubleshooting over verification

Steve's checkpoint template focuses on the happy path: "Run this, see this." I want to emphasize the unhappy path: "If you see X instead..." The plan should spend MORE space on what to do when things go wrong than on confirming things went right. Why? Because the reader who's on the happy path doesn't need the checkpoint (they can see it's working). The reader who's confused needs the checkpoint desperately. Design for the reader who needs it, not the one who doesn't.

### Steve's point about the Part 4→5 pacing gap (his Section XI): I think the problem starts earlier

Steve identifies the Part 4→5 jump as the steepest. I think the Part 1 jump is already too steep for a JavaScript developer encountering NeoHaskell syntax for the first time (see my Section III). The solution to both problems is the same: break parts into smaller lessons (see my Section IX).

### Steve's "200 lines" verification point (his Section XV): Absolutely — and also verify the "10 minutes" claim

The plan says "Part 1 must produce visible output within 10 minutes of starting" (line 153). This is a great constraint. But has anyone actually TRIED this? Time claims in documentation are almost always wrong because the writer (who already knows the system) tests by reading, not by doing. You need to test the 10-minute claim with a fresh developer who's literally never seen NeoHaskell, including Nix installation time (which, if Nix isn't already installed, is NOT 10 minutes).

---

## XII. One Last Thing: Where's the Joy?

**Responding to: The entire plan**

I want to close with something that might sound soft but is actually the most important point in this review.

The plan is rigorous. It's well-structured. It draws on real research. It has clear phases, success metrics, and validation protocols. It would produce *competent* documentation.

But would it produce documentation that makes someone EXCITED to learn NeoHaskell? Would someone reading Tutorial Part 1 text their friend and say "you HAVE to see this"? Would they feel, at the end, like they just discovered something genuinely cool?

The Svelte tutorial makes people excited. The FastAPI docs make people excited. My zines make people excited. What do these have in common? They treat learning as something **joyful** — as an activity that's fun in itself, not just instrumentally useful for building software.

The plan's most joyful element is the Scope table (line 113-123) — the "sounds like enterprise, actually is trivial" conceit. That's FUN. That creates DELIGHT. The plan needs MORE of that spirit throughout:

- In the tutorial prose: "Okay wait, did we just build a COMPLIANCE ENGINE? With... a list of events? That we already had?"
- In the exercises: "Here's a weird question: what happens if you replay your events starting from event 3 instead of event 1? Try it!"
- In the concept pages: "You know how Git stores every version of every file? Event sourcing is the same idea for your application data. Your event store IS version control for your business logic."
- In the error explanations: "NeoHaskell just caught a bug that would have cost your bank REAL MONEY in production. That type error? That's the compiler saying 'hey, you almost let someone withdraw money they don't have.' Pretty cool, right?"

This isn't decoration. This is the difference between documentation that people tolerate and documentation that people recommend. Every "pretty cool, right?" moment is a moment the reader feels like they're part of a community of people who are excited about the same thing.

### My specific recommendation

After the plan is drafted and before it's written, do a **"joy pass."** For every tutorial page, every concept page, and every exercise, ask: "Is there a moment of genuine delight on this page? Is there something that would make the reader smile, or feel surprised, or feel clever?" If the answer is no, add one. It doesn't have to be a joke. It can be a surprising insight, a satisfying "aha," a moment where the reader realizes they understand something they thought was hard.

The best documentation doesn't just teach. It makes the reader feel smart. That's the real goal.

---

## Summary of Recommendations (in priority order)

1. **Add a guiding principle about delight** — make "the reader should feel excited, not just informed" an explicit design goal
2. **Design visual explanations as primary teaching tools** — event timeline, CRUD-vs-events comparison, annotated code blocks, "what happens when" traces — BEFORE writing prose
3. **Create an annotated code block component** for the first 3 tutorial chapters that visually labels unfamiliar syntax elements
4. **Break 6 tutorial parts into 18-24 small lessons** (Svelte model: one concept, one exercise, 5-10 minutes each)
5. **Add "What Happens When..." sections** to the concept page template — step-by-step runtime traces that make invisible machinery visible
6. **Rewrite exercises in an inviting, challenge-based tone** — "Can you...?" not "Do this." Include visible solutions for every exercise.
7. **Add "If You're Stuck" sections and troubleshooting to every tutorial page** — design for the confused reader, not just the successful one
8. **Include the "What You Built" reveal table at the end of the tutorial** — the "sounds like enterprise, actually is trivial" moment is the emotional payoff for the whole journey
9. **Increase Break-It exercises and add "What If" exercises** — more opportunities for discovery, experimentation, and "aha" moments
10. **Frame error messages as mysteries/discoveries**, not failures — make debugging feel like detective work
11. **Commit to second-person voice ("you") throughout** — let Alex fade after page 1; the reader should BE the protagonist
12. **Do a "joy pass" after drafting** — every page should have at least one moment of genuine delight

---

## Closing

Steve closed his review with "Ship Tutorial Part 1. Watch someone use it. Fix everything. Then write Part 2." That's the right process advice.

My closing is about the spirit of the work: **the most important thing NeoHaskell's documentation can do is make a CRUD developer feel like event sourcing is not just learnable, but genuinely cool.** Not "cool" like "this technology is impressive." Cool like "oh my god, I can't believe that worked, and it was SO simple, I need to tell someone about this."

That feeling — the feeling of discovering that something you thought was hard is actually beautiful and simple — is what makes people adopt new technologies. Not feature lists. Not benchmarks. Not even great tutorials. It's the "OHHH!" moment. Design for that.

— Julia Evans