# The Google AI Studio prompt

CDAR 2026 · Course 1 · Module 6 · Day 3 · Prof. Sungjong ROH

---

## How to use this file

1. Open **[aistudio.google.com](https://aistudio.google.com)** and start a **Build** app.
2. Upload two files from `app/src/model/`: **`scorer.ts`** and **`model_card.json`**.
3. Paste the prompt below.

The upload matters more than the prompt. A model asked to invent a stroke
scorer will invent one, and it will be confidently wrong. A model handed a
scorer that has already passed 2,005 parity tests has nothing left to invent.

**Why the prompt is written the way it is.** Generative tools are eager. Ask
for "a stroke risk app" and you get a logistic regression with plausible
coefficients, because that is what the training data is full of. The whole
prompt is built to make that failure impossible: the arithmetic arrives as a
file, the file is declared read-only, and the request is reduced to what these
tools are genuinely good at — layout, states, and accessibility.

---

## The prompt

> Build a single-page **React + TypeScript** app called **Stroke Risk Decision
> Support**, using **Vite**. It is a clinical screening tool.
>
> **Two files are attached. Treat both as read-only.**
>
> - `scorer.ts` — the model, ported from R and already verified. It exports
>   `prepareCard`, `predictProb`, `verdict`, and the types.
> - `model_card.json` — the trained parameters and the input schema.
>
> **Absolute constraints. Breaking any of these makes the app wrong:**
>
> 1. **Do not modify, refactor, reformat, or "improve" `scorer.ts`.** Import
>    from it. Its arithmetic matches a tidymodels XGBoost workflow to 1e-7 and
>    is checked in CI against 2,005 patients scored in R.
> 2. **Do not write any scoring, probability, weighting, or risk arithmetic of
>    your own**, anywhere in the app, for any reason — including "just for a
>    fallback", a loading placeholder, or an example. `predictProb` is the only
>    source of a probability. If you need a number before the card loads, show
>    a dash.
> 3. **Call `prepareCard(cardJson)` exactly once at module scope**, and score
>    against the result. Passing the raw parsed JSON to `predictProb` silently
>    changes the answers — the split thresholds must be narrowed to float32
>    first.
> 4. **Derive every input control from `card.schema`.** Do not hard-code a
>    slider range, a dropdown option, or a factor level. If the model is
>    retrained, only the JSON should need to change.
> 5. **Send the exact strings from `card.schema`** as values. `hypertension`
>    and `heart_disease` are the strings `"0"` and `"1"`, not booleans and not
>    numbers. Display "No" and "Yes" to the user; send `"0"` and `"1"` to the
>    model.
>
> **Layout** — three input panels across the top, then three result tiles,
> then a verdict panel, then an about section.
>
> - *Demographics*: gender, age, ever married, work type, residence type.
> - *Clinical History*: hypertension, heart disease, smoking status, average
>   glucose, BMI.
> - *Decision Policy*: a threshold slider (1%–90%, default
>   `card.suggested_threshold`) and a borderline half-width slider (1%–15%,
>   default 5%), plus an "Evaluate against threshold" button.
> - *Result tiles*: predicted probability as a percentage to one decimal; the
>   predicted class (`prob >= threshold ? "Yes" : "No"`); the active threshold.
> - *Verdict panel*: `verdict()` output. Red for "Intervene", amber for
>   "Borderline", green for "No action".
> - *About*: the `card.meta` values — winning workflow id, training date,
>   metrics, and the top variable importances as a small bar list.
>
> **Behaviour** — the three result tiles update live as any input changes. The
> verdict panel updates **only** when the button is pressed, and shows a small
> "inputs have changed, press again" note when the inputs have moved since the
> last press. That split is deliberate: exploring is continuous, deciding is an
> act.
>
> **Design** — a clinical instrument, not a consumer dashboard. System sans,
> generous whitespace, one restrained accent per panel, and colour reserved
> almost entirely for the verdict so nothing competes with it. Support light
> and dark via `prefers-color-scheme`. Tabular numerals for every figure.
> Responsive down to 380px.
>
> **Accessibility** — every control gets a real `<label>`. The verdict panel is
> an `aria-live="polite"` region. Never signal risk by colour alone; the
> headline text always carries the meaning. Visible focus rings.
>
> **Deliverables** — `package.json`, `vite.config.ts`, `tsconfig.json`,
> `index.html`, `src/main.tsx`, `src/App.tsx`, `src/styles.css`. Put the two
> attached files at `src/model/` and import them from there. No UI or state
> libraries beyond React. No network calls: the app must work fully offline.

---

## After it generates: check these four things

AI Studio will produce something that runs. Running is not the bar.

| # | Check | How |
|---|-------|-----|
| 1 | `scorer.ts` is byte-identical to what you uploaded | `diff` it against your copy. If the tool "tidied" it, restore yours. |
| 2 | No stray arithmetic | Search the generated source for `Math.exp`, `Math.pow`, `0.5`, `coefficient`, `weight`. Anything outside `scorer.ts` is a bug. |
| 3 | `prepareCard` is actually called | Search for it. If the app passes raw JSON to `predictProb`, roughly 1 patient in 140 gets a wrong answer, and none of them announce it. |
| 4 | Parity still passes | Copy the generated `src/` over the reference app and run `npm run parity`. |

Check 4 is the only one that is not an opinion. Run it before you deploy, and
run it again after every regeneration.
