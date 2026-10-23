# Development plan, next round (2026-10-23)

The crosswalk review plan (`CROSSWALK-REVIEW.md`, phases 0-5) is done, as are
the full crosswalk review (F-030), the CEO / board label set (F-031), the
step 09 rule fixes (F-032), the CEO thesaurus (F-033) and the chancellor fix
(F-034). This plan collects what is left. Findings go in
`data-raw/partvii-validation/FINDINGS.md` as usual.

## Rule: overlapping CEOs in a transition year

An outgoing and an incoming chief executive reported on the same return
should **both** be the CEO for that filing. That is not a bug; it records the
transition. Labels already do this (`LABELING-GUIDE-ceo.md`, rule 9:
`ceo_note` = `outgoing` on the former). Scorers and rules must:
- treat two CEOs in a filing as correct when one is outgoing;
- not count the outgoing CEO as an "extra CEO" error;
- not add a rule that drops a former CEO while another is named (F-032
  already rejected that rule: it cost 0.1 point).

Revisit the "extra CEO (13)" and "former and interim CEOs (about 15)" error
counts of F-031/F-032 under this rule: some are not errors.

## 1. Settle the label disagreements (first)

- 193 rows in 144 filings of the CEO sample
  (`ceo-sample/disagreements.csv`) and 45 rows in 30 filings of the held-out
  set (`ceo-holdout/disagreements.csv`) are undecided. Every CEO score
  excludes them, and they concentrate in the hard cases.
- **Review page (built 2026-10-23):** https://claude.ai/artifact/GKCuuJThFW1KwGmmy2Lr3M
  (`30-review-queue.R` + `review-page/template.html`). 320 cases ranked by
  model agreement (F-035): 22 models split, 31 likely label errors, 90 lean
  one way, 81 unanimous, 96 weakly confirmed. Decisions are saved in the
  page's store (collection `decisions`, one document per row_id).
- Originally planned: a review page or sheet that shows each case with the filing, both
  labels and the step 09 answer, and records a decision.
  `26-ceo-score.R` applies `decision_*` columns on its next run.

## 2. Remaining CEO errors that need rules (about 80 of 112 wrong filings)

Score each change alone on the F-031 set and the held-out set, as in F-032.
- **Stipend officers named CEO (about 25 filings).** Treasurers, recorders
  and secretaries paid $7-37k at 5+ h/wk, outside lodges and veterans posts.
  Try a pay or hours threshold, possibly by organization type.
- **Unpaid presidents the labels make CEO (about 25).** No officer box or 0
  hours, so rule 7 does not fire. Near the board-chair boundary; decide
  after item 1.
- **Former and interim CEOs (about 15).** Re-count under the overlap rule
  above. What remains (a former CEO paid only severance, an interim listed
  after the new CEO started) needs the tax period's dates, which step 09
  does not see yet.
- **Multi-CEO systems.** A hospital system lists facility CEOs. Test a rule
  that keeps the top-paid CEO-level title as `designated` and makes the
  others `OFFICER`, unless the overlap rule applies (an outgoing CEO).

## 3. Smaller CEO items (F-033 open list, Identifying CEOs article)

- Union abbreviations: BM/FS, BMFINSEC (business manager / financial
  secretary).
- Unpaid presidents of religious colleges (a Benedictine president).
- A minimum gap for pay ties in the imputation rule.
- Two-pastor churches: ASSOCIATE PASTOR maps to the CEO-level standard
  PASTOR, so a senior and an associate pastor are co-CEOs. Map associate,
  assistant and youth pastors to a non-CEO standard.

## 4. Model comparison for labeling (in progress, 2026-10-23)

How well do Claude models of different sizes label the CEO / board panel,
and what is the cheapest one before quality drops? Measured on the 750
filings of the F-031 panel against the consensus labels: board members, the
CEO of each filing, and CEO transitions across years (with the overlap rule
above). Script: `29-model-eval.R`; results in FINDINGS.md.

## 5. Package cleanup (from `TODO.md`)

- Shrink the exported API to the user-facing functions before 1.0.
- Clear the R CMD check warning (non-ASCII glyphs in status messages) and
  the `state.abb` note.
- Confirm the LICENSE copyright holder.

## 6. Leftover from the crosswalk review

- Phase 1 item 3: the August suggestion list goes through the per-title
  review rather than a bulk apply (F-010). Low priority.
