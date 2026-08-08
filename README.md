# From `.RDS` to a live URL

**CDAR 2026 · Course 1 · Module 6 · Day 3**
Prof. Sungjong ROH · Lee Kong Chian School of Business, SMU · sroh@smu.edu.sg

Taking `CDAR_2026_C1M3_Day3_Deploy_Classification_to_Dashboard_REVIEW.R` and
`Saving_millions_at_a_time.RDS` from "works in RStudio" to "works at a URL".

---

## 1. Why the local app does not deploy

The Shiny app does exactly one thing that matters:

```r
artifacts <- readRDS("Saving_millions_at_a_time.RDS")
prob_yes  <- predict(artifacts$workflow, new_data = new_data, type = "prob")$.pred_Yes
```

That single `predict()` call needs an R interpreter, tidymodels, recipes,
workflows, parsnip, hardhat, and xgboost — about 1.2 GB of installed
dependencies — resident in memory, holding a live WebSocket to the browser for
as long as the user has the tab open.

**Vercel provides none of that.** It runs Node.js, Python, Go, Ruby, Rust and
Bun. There is no R runtime and there is no plan to add one. Netlify, GitHub
Pages and Cloudflare Pages are the same. This is not a configuration problem
and no amount of `vercel.json` fixes it: Shiny is a *stateful server process*,
and these hosts serve *static files and short stateless functions*.

The error you hit locally-to-hosting is not really an error. It is a category
mismatch.

### The three legal architectures

| | Where R lives | Cost | Cold start | Verdict |
|---|---|---|---|---|
| **A. Shiny server** | shinyapps.io / Posit Connect / your own container | Free tier is 25 active hours/month, then paid | 5–30 s | Correct if you need R at runtime. Not Vercel. |
| **B. R API + thin frontend** | Cloud Run container behind a Vercel function | ~$0 idle, pay per request | 2–10 s | Right answer for a real production system. Two things to operate. |
| **C. Port the model** | Nowhere. The maths ships as JSON. | $0 | 0 ms | Right answer for a portfolio piece, a teaching demo, or a screening tool. |

Your Day 2 script already builds **B** (`m6_day2_deploy/vercel/api/score.js`
proxies to a Cloud Run R backend). This folder builds **C**, because that is
what "TypeScript on Vercel" means.

**What C costs you, stated plainly.** No R at runtime means no R at runtime:
you lose `predict()` on arbitrary new preprocessing, you lose k-NN imputation
(which needs the training set), and you lose the ability to change the model
without re-exporting. What you gain is a page that costs nothing, never cold
starts, works offline, and cannot leak a patient's data because nothing ever
leaves the browser. For a screening tool with ten inputs, that trade is
strongly favourable.

---

## 2. What actually gets shipped

Not the `.RDS`. The **arithmetic inside it**.

A prediction from this workflow is a fixed sequence of operations, and every
constant in it is a number you can write down:

```
raw inputs
  → Box-Cox      3 lambdas
  → normalise    3 means, 3 SDs
  → dummy encode the level-to-column map
  → 500 trees    split feature, threshold, two child pointers, a default
  → sum leaves + logit(base_score)
  → sigmoid      P(stroke = "Yes")
```

Write those to JSON and any language reproduces the prediction. Here that JSON
is **175 KB** — smaller than the 675 KB `.RDS`, and readable by everything.

```
m6_day3_stroke_ts/
├── 01_export_model_card.R      R: artifact → model_card.json + fixtures
├── 02_check_parity.mjs         Node: does TypeScript still agree with R?
├── AI_STUDIO_PROMPT.md         the prompt, and what to check afterwards
└── app/
    ├── src/model/scorer.ts     the model as arithmetic (the only maths)
    ├── src/model/model_card.json      generated — do not hand-edit
    ├── src/model/parity_fixtures.json generated — 2,005 patients scored in R
    ├── src/App.tsx             the Shiny UI, rebuilt in React
    └── vercel.json
```

---

## 3. Step by step

### Step 1 — export the model card

```bash
cd scripts && Rscript m6_day3_stroke_ts/01_export_model_card.R
```

```
Artifact loaded.  Winner: up_xgb  | trained: 2026-05-23 18:06:50
Box-Cox lambdas : age=1.307326  avg_glucose_level=-0.800148  bmi=0.090618
Dummy layout reconciled against the trained model matrix.  OK
Trees extracted : 500  | nodes: 7110
Model card written: .../model_card.json  (175 KB)
Parity fixtures written: ...  ( 2005 patients )
```

The script refuses to write a card it cannot reconcile. It rebuilds the dummy
layout from the schema and asserts it against the actual trained model matrix,
and it stops if `step_zv` dropped a column. An export that cannot prove its own
column order is worse than no export, because it fails silently downstream.

### Step 2 — generate the UI

Follow **[AI_STUDIO_PROMPT.md](AI_STUDIO_PROMPT.md)**. Upload `scorer.ts` and
`model_card.json`, paste the prompt, and run the four checks at the bottom of
that file.

`app/` here is a complete working implementation. Use it as the reference to
diff against, or as the fallback if the generated UI disappoints.

### Step 3 — prove it agrees with R

```bash
cd scripts/m6_day3_stroke_ts/app && npm install && npm run parity
```

```
Patients compared : 2005
Tolerance         : 0.000001
Largest deviation : 1.865e-7
PARITY PASSED. The browser and RStudio agree.
```

2,000 random patients plus five deliberate corner cases: every factor at its
reference level, every factor at its last level, and the numeric sliders at
both end stops. Random draws never land on an end stop, and end stops are
precisely where a port breaks.

`npm run build` runs this before it compiles, so a broken port cannot produce
a `dist/`.

### Step 4 — run it locally

```bash
npm run dev
```

Open the app next to RStudio and type the same patient into both. The numbers
match to seven decimal places. Do this once with your own eyes; the parity
suite is more thorough, but seeing it is what makes you believe it.

### Step 5 — push to GitHub

```bash
git add scripts/m6_day3_stroke_ts .github/workflows/stroke-parity.yml
git commit -m "Port stroke model to TypeScript for Vercel deployment"
git push
```

Commit `model_card.json` and `parity_fixtures.json`. They are build inputs, not
build outputs — CI has no R and must not need any.

`.github/workflows/stroke-parity.yml` re-runs the parity check on every push.
It is the gate: a change to `scorer.ts` that breaks agreement with R cannot
reach the URL.

### Step 6 — deploy

On [vercel.com](https://vercel.com) → **Add New Project** → import the repo, then:

| Setting | Value |
|---|---|
| Framework Preset | Vite |
| **Root Directory** | `scripts/m6_day3_stroke_ts/app` |
| Build Command | `npm run build` (default) |
| Output Directory | `dist` (default) |

**Root Directory is the one that catches people.** The repo root has no
`package.json`, so Vercel looks there, finds nothing, and reports a build
failure that says nothing about the real cause.

Or from the terminal:

```bash
cd scripts/m6_day3_stroke_ts/app && npx vercel --prod
```

Every push to `main` redeploys. Every pull request gets its own preview URL.

---

## 4. The three traps

These are not hypothetical. All three appeared while building this folder, and
each produced *wrong numbers with no error message*.

### Trap 1 — a sparse zero is not a zero

`parsnip::maybe_matrix()` hands XGBoost a sparse `dgCMatrix`. Sparse matrices
do not store zeros, so XGBoost cannot distinguish "this value is 0" from "this
value was never supplied". **It treats every one-hot 0 as missing** and routes
it by the node's `default_left` flag rather than by the numeric comparison.

This is why the tree dump is full of splits like
`ever_married_Yes < 2.00001`. Read literally, every 0/1 value satisfies that
test and the right child is unreachable. It is not unreachable — the zeros
never reach the comparison at all.

Encode those slots as `0` and the same patient scores **0.6124** in the browser
against **0.5559** in R. No exception, no warning, 5.6 percentage points.

`scorer.ts` writes `NaN` into every unset dummy slot and checks for missing
*before* comparing.

### Trap 2 — `auto_unbox` collapses a one-element vector

`jsonlite::write_json(auto_unbox = TRUE)` turns a length-1 vector into a bare
scalar. A binary factor has exactly one non-reference level, so:

```r
levels = c("Yes")        →     "levels": "Yes"      # a string, not an array
```

JavaScript then indexes into the *string*: `"ever_married_Yes"[0]` is `"e"`, a
column that does not exist, so the assignment goes nowhere and the feature
stays missing. `"Yes".indexOf("Yes")` still returns `0`, so the lookup *looks*
correct at every step.

This broke 286 of the first 300 patients — every one whose binary factors were
off the reference level, all understating risk. The 14 that passed were the
ones where the bug happened not to bite. Wrap anything that must stay an array
in `I()`.

### Trap 3 — JSON has no float32

XGBoost stores split thresholds and leaf weights as **float32**. JSON has only
double. The threshold written as `1.1143749` parses to a double a hair *above*
the float32 the model actually branches on, so a patient landing exactly on
that boundary takes the wrong branch.

One patient in 300 differed by 4.8e-4. The diagnostic said "tree 146 of 500, R
leaf 12, TypeScript leaf 11", and the node read `v=1.1143748760223389` against
`cond=1.1143749`. `prepareCard()` narrows both sides through `Float32Array` and
`Math.fround`, and the two land on the same leaf.

**What the three have in common.** None of them throws. Every one produces a
confident, plausible, wrong number. This is why the parity fixtures exist, and
why the build runs them before it compiles. A deployment you have not measured
against the original is a deployment you are guessing about.

---

## 5. What did not survive the port, and what to do about it

**k-NN imputation.** `step_impute_knn` fills a missing BMI by finding the five
nearest training rows, which needs the training set. The card carries no
training data, so `buildFeatures` throws if a numeric field is absent. The
Shiny app always supplies BMI from a slider, so nothing is lost here — but if
you add a "BMI unknown" option, you must decide the policy explicitly. The
honest choices are to require the field, or to export the training medians and
document that you are imputing more crudely than R did.

**Upsampling.** `step_upsample` has `skip = TRUE`; it shaped training and does
nothing at prediction time. Correctly absent.

**Retraining.** The card is a snapshot. Retrain the model and the deployed app
keeps serving the old one until you re-run step 1. Re-run steps 1 and 3
together, always — new card, new fixtures, new parity proof.

---

## 6. Choosing between the three architectures

Use **C** (this folder) when the model is fixed, the input is a form, and you
want a free, instant, offline-capable page. A screening tool, a portfolio
piece, a teaching demo.

Use **B** (Day 2's Cloud Run + Vercel proxy) when the model is large or
proprietary, when you need to log every prediction for monitoring, when
preprocessing genuinely needs the training data, or when the model is retrained
often enough that re-exporting is a chore.

Use **A** (shinyapps.io) when the app is R all the way down — when the point is
`ggplot`, `dplyr` on a live dataset, or a report an analyst regenerates
interactively. Do not port a Shiny app to TypeScript because TypeScript is
fashionable. Port it because the model has stopped changing and you want the
page to cost nothing.

The question is not "which is best". It is which constraint you are actually
under.
