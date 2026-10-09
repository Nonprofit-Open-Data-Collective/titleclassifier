# Reviewing the Part VII title crosswalk

This folder holds the scripts, decisions and findings of the review of the
title classifier on every Form 990 / 990-EZ Part VII Section A title,
TY2009-2024. The data and the review files live outside the repository, in
`~/Documents/PARTVII` (set `PARTVII_DIR` to move it):

```
PARTVII/
  data/
    panel/                 Part VII person-rows, all years (from efile_v2_3)
    titles-raw.parquet     distinct raw titles with counts
    title-steps.parquet    raw title -> split titles and each cleaning step
    title-keys.parquet     (raw title x officer box x paid x 40+ hours) -> TitleTxt7, title.standard
    panel-classified/      the panel, one row per person-title, with TitleTxt7 and title.standard
    title-profiles.parquet one row per TitleTxt7: frequency, holders, mapping, checks, queue
  review/
    index.csv              every TitleTxt7 with its queue, tier and file
    queue-add.csv          titles the crosswalk does not cover
    queue-inspect.csv      mapped titles an automatic check flagged
    queue-confirm.csv      mapped titles with no flag
    titles/<block>/<id>_<slug>.md   one review file per TitleTxt7
  logs/
```

## What is reviewed

The unit of review is the **standardized title**, `TitleTxt7`: the title after
the cleaning steps (01 standardize, 02 dates, 03 conjunctions, 04 split,
05 spelling, 06 status words, 07 c-suite fixes) and before the crosswalk
merge. The crosswalk matches `TitleTxt7` exactly to `title.variant`, so each
`TitleTxt7` lands in one `title.standard`, or none. One file covers every raw
title that becomes that `TitleTxt7`, so one decision fixes all of them.

Two things can go wrong, and each review file shows the evidence for both:

1. **The cleaning steps produce the wrong `TitleTxt7`.** The raw-variants
   table traces each raw title through steps 3, 5 and 6. A split that cuts a
   title in two, a spelling rule that changes the meaning, or a status word
   left in or wrongly removed is a cleaning problem, fixed in the step's code,
   not the crosswalk.
2. **The crosswalk sends `TitleTxt7` to the wrong place,** or nowhere.

## The queues

`03-title-profiles.R` puts every title in one queue and one tier:

| Queue | Meaning | What to do |
|---|---|---|
| **ADD** | `TitleTxt7` is not in the crosswalk; the person gets no category | choose a standard (or decide it is not a title) |
| **INSPECT** | mapped, but an automatic check is raised | check the flagged mapping |
| **CONFIRM** | mapped, no check raised | confirm (tier 1), or spot-check by sample |

| Tier | Titles (round 0) | Rows | Reviewed how |
|---|---|---|---|
| 1 | 1,000+ person-title rows: 923 titles | 92.9% | every file, all three queues |
| 2 | 100-999 rows: 6,046 titles | 2.7% | ADD and INSPECT every file; CONFIRM by a random 5% sample |
| 3 | under 100 rows, 5+ filings: 123,630 titles | 2.9% | by pattern and cleaning fixes; files read as needed |
| 4 | the rest: 487,565 titles | 1.6% | not reviewed one by one; covered by rules and patterns |

Tiers 1 and 2 hold 96% of rows in about 7,000 titles. Tiers 3 and 4 are
mostly unmatched (94% and 99.8% of their rows), and most of them are variants
the cleaning steps should have reduced to a known title (see FINDINGS.md
F-006 to F-008). Fixing a cleaning rule moves many tail titles at once, so
tail work starts from the patterns, not from single files.

Work through the queues in this order: tier 1 ADD, tier 1 INSPECT, tier 1
CONFIRM, then tier 2 ADD and INSPECT, the tier 2 sample, and then tier 3
patterns. The order puts the most person-rows behind each decision first.

The automatic checks (in `auto_checks`) are reasons to look, not verdicts:

| Check | Raised when |
|---|---|
| `MISSING_FROM_CROSSWALK` | no crosswalk row |
| `NO_TAXONOMY_ROW` | the standard has no taxonomy row, so no category |
| `BOARD_BUT_PAID_FT` | a board/member standard, but most holders are paid and work 35+ hours |
| `EXEC_BUT_UNPAID_PT` | a CEO / c-level standard, but holders are unpaid and work under 10 hours (990 filers) |
| `OFFICER_BOX_MISMATCH` | a board standard (not president, VP, secretary or treasurer), but 60%+ tick the officer box |
| `STANDARD_WORDS_DIFFER` | the standard shares no word with the title (fine for abbreviations, worth a look otherwise) |
| `CLEANING_RESIDUE` | digits, `&`, `/`, brackets, a dangling AND/OF/THE, or two letters or fewer left after cleaning |
| `EMPTY_TITLE` | nothing left after cleaning |
| `STEP7_REWRITE` | step 7 rewrote the title for some holders (CHANCELLOR, finance titles); check the rule fits |

## Reviewing one file

Each file starts with a YAML block of facts and an empty `review:` section,
followed by the evidence: who holds the title (checkboxes, pay, hours), leads
for missing titles (suggestions already on file, crosswalk variants the title
contains, the nearest crosswalk variants), the raw titles with their cleaning
trace, and five example filings with links to the XML.

Answer five questions in order and stop at the first that settles it:

1. **Is it a title?** A name, a date, an org name, "SEE SCHEDULE O" or a
   fragment is `not_a_title`. If a fragment comes from a bad split, it is
   `fix_cleaning` instead.
2. **Did cleaning keep the meaning?** Compare the raw titles with `TitleTxt7`.
   If a step changed the meaning, it is `fix_cleaning`; name the step and the
   rule in `cleaning_issue`.
3. **For a mapped title, does the standard fit the holders?** Board titles
   should look like a board: trustee box, unpaid, a few hours a week.
   Executive titles should look like staff: officer or key-employee box, paid,
   full time. If it fits, `confirmed`; if another standard fits better,
   `remap`.
4. **For a missing title, which existing standard fits?** Use the leads, and
   prefer an existing `title.standard`: `add`. Create a new standard only when
   none fits: `add_new_standard`, with its taxonomy row in `new_taxonomy`.
5. **Does the answer depend on the filer?** PRESIDENT is the CEO in some
   organizations and the board chair in others. Record `ambiguous`, say what
   it depends on in `notes`; these become conditional rules (the step 9 stub),
   not crosswalk rows.

Then fill in the `review:` block and save:

```yaml
review:
  status: add               # pending | confirmed | remap | add | add_new_standard | fix_cleaning | not_a_title | ambiguous
  standard: "BOARD MEMBER"   # for remap, add, add_new_standard
  new_taxonomy: ""           # add_new_standard: "domain.category / domain.label / flags"
  cleaning_issue: ""         # fix_cleaning: step and rule
  reviewer: "J. Lecy"
  date: "2026-10-09"
  notes: "committee members of the board; 98% trustee box, unpaid"
```

Rules of thumb:

- One `TitleTxt7`, one decision, whatever raw title it came from.
- Judge by the holders, not just the words: the checkbox, pay and hours
  profile is the strongest evidence of what a title means in practice.
- When a decision is really a pattern (every "<X> COMMITTEE MEMBER" is a
  board member), record it once in `FINDINGS.md` as a candidate rule. Rules
  are proposed as a batch, checked against the titles they would change, and
  then decided like any other change.
- A machine-drafted decision (by Claude or a script) sets `reviewer` to
  "Claude (draft)" or the script name. A person confirms it before it is
  applied.

## From decisions to fixes

```
Rscript data-raw/partvii-validation/05-collect-reviews.R          # review files -> review-decisions.csv and the change lists
Rscript data-raw/partvii-validation/06-apply-decisions.R          # dry run: what would change
Rscript data-raw/partvii-validation/06-apply-decisions.R --apply  # update inst/extdata/crosswalks/ and applied-log.csv
Rscript data-raw/partvii-validation/02-classify-titles.R          # re-classify (reuses the cached steps 1-6)
Rscript data-raw/partvii-validation/03-title-profiles.R           # new profiles and queues
Rscript data-raw/partvii-validation/04-write-review-files.R       # refresh files; reviewed files are kept
```

Recorded in this folder (committed):

| File | Contents |
|---|---|
| `review-decisions.csv` | every decision, one row per `TitleTxt7` (latest wins) |
| `crosswalk-changes.csv` | crosswalk rows to add or change |
| `taxonomy-additions.csv` | new standards and their taxonomy rows |
| `cleaning-issues.csv` | titles a cleaning step gets wrong, for code fixes |
| `applied-log.csv` | what `06-apply-decisions.R` changed, and when |
| `taxonomy-edits-log.csv` | cells of existing taxonomy rows that `07-taxonomy-fixes.R` changed (role flags, SOC) |
| `FINDINGS.md` | the findings log: patterns, rule proposals, code fixes, metrics after each round |

The crosswalk CSVs in `inst/extdata/crosswalks/` are snapshots of the Google
Sheet, and `get_googlesheets_*(refresh = TRUE)` overwrites them. Enter the rows
in `applied-log.csv` and `taxonomy-edits-log.csv` in the sheet as well before
anyone refreshes. `tests/testthat/test-crosswalks.R` checks the snapshots
(unique standards, flags, valid SOC codes) and fails if a refresh brings back
a fixed error.

Code fixes for `fix_cleaning` findings change the step functions in `R/`. Each
one cites its finding in `FINDINGS.md`, and the regression baseline
(`tests/regression/check-regression.R`) is rebuilt with it, since it changes
outputs by design.

## Measuring progress

After each round, rerun 02-03 and record in `FINDINGS.md`:

- the share of person-title rows with no `title.standard`, overall and by tier;
- the number of ADD and INSPECT titles left in tiers 1 and 2;
- for the CONFIRM sample, the share that needed a change. This is the
  estimated error rate of the mapped titles that were not reviewed one by one.
