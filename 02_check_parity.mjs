// CDAR 2026 · Course 1 · Module 6 · Day 3
// 02_check_parity.mjs—the gate. Run it before every deploy.
//
// Prof. Sungjong Roh, PhD (talktoroh.com) · SMU · sroh@smu.edu.sg
// Academic Director, Master of Science in Business Artificial Intelligence (MB+AI)
//
// Team, the deployment is only trustworthy if the browser and RStudio return
// the same number. This script scores the 2,005 patients R already scored and
// compares. It exits non-zero on any disagreement, which is what lets you wire
// it into a GitHub Action and stop a bad build reaching your URL.
//
//   cd app && npm run parity        (or: node --experimental-strip-types 02_check_parity.mjs)
//
// This file lives INSIDE app/ on purpose. Vercel copies only the Root
// Directory into its build container, so a gate that sits one level up is not
// there when the build runs:
//
//     Error: Cannot find module '/vercel/02_check_parity.mjs'
//     code: 'MODULE_NOT_FOUND'
//
// A check that does not travel with the thing it guards is not a gate.

import { readFileSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { dirname, join } from "node:path";

const here = dirname(fileURLToPath(import.meta.url));

// pathToFileURL is not decoration. A dynamic import() takes a URL, not a path,
// and path.join gives us a path. On macOS and Linux "/Users/.../scorer.ts"
// happens to be accepted; on Windows it is "C:\\Users\\...\\scorer.ts" and Node
// reads the leading "C:" as a URL scheme, then refuses it:
//
//     throw new ERR_UNSUPPORTED_ESM_URL_SCHEME(parsed, schemes)
//     code: 'ERR_UNSUPPORTED_ESM_URL_SCHEME'
//
// Converting to a file:// URL first makes the same line work on every platform.
const SCORER = pathToFileURL(join(here, "src", "model", "scorer.ts")).href;

let predictProb, prepareCard;
try {
  ({ predictProb, prepareCard } = await import(SCORER));
} catch (err) {
  console.error(`Could not load the scorer from:\n  ${SCORER}\n`);
  if (err && err.code === "ERR_UNKNOWN_FILE_EXTENSION") {
    console.error("Node cannot strip TypeScript types on this version. Node 22.6 to");
    console.error("23.5 need the --experimental-strip-types flag; 23.6 and later do it");
    console.error("by default. Check `node --version`.");
  }
  throw err;
}

const card     = prepareCard(JSON.parse(readFileSync(join(here, "src", "model", "model_card.json"), "utf8")));
const fixtures = JSON.parse(readFileSync(join(here, "src", "model", "parity_fixtures.json"), "utf8"));

const TOL = fixtures.tolerance ?? 1e-6;

let worst = 0;
let worstCase = null;
const failures = [];

for (const [i, c] of fixtures.cases.entries()) {
  const patient = {};
  for (const [k, v] of Object.entries(c.input)) patient[k] = v;
  for (const k of card.preprocess.numeric_vars) patient[k] = Number(c.input[k]);

  const js  = predictProb(card, patient);
  const r   = c.r_prob_yes;
  const err = Math.abs(js - r);

  if (err > worst) { worst = err; worstCase = { i, r, js, patient }; }
  if (err > TOL)   { failures.push({ i, r, js, err, patient }); }
}

console.log(`Patients compared : ${fixtures.cases.length}`);
console.log(`Tolerance         : ${TOL}`);
console.log(`Largest deviation : ${worst.toExponential(3)}`);

if (worstCase) {
  console.log(`  worst case #${worstCase.i}  R=${worstCase.r.toFixed(10)}  ` +
              `TS=${worstCase.js.toFixed(10)}`);
}

if (failures.length) {
  console.error(`\nPARITY FAILED on ${failures.length} of ${fixtures.cases.length} patients.`);
  for (const f of failures.slice(0, 5)) {
    console.error(`  #${f.i}  R=${f.r.toFixed(8)}  TS=${f.js.toFixed(8)}  ` +
                  `diff=${f.err.toExponential(3)}`);
    console.error(`      ${JSON.stringify(f.patient)}`);
  }
  process.exit(1);
}

console.log("\nPARITY PASSED. The browser and RStudio agree.");
