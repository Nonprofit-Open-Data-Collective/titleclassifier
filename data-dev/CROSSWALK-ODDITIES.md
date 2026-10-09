# Crosswalk oddities (2026-10-09)

Cases noticed while writing the pkgdown step articles (PR "Docs: step
articles..."). None has been changed yet. Each needs a decision: fix the
crosswalk, fix the code, or record why the current mapping is intended.

Sources: `data-raw/crosswalks/title-standardization.csv` (title.xwalk),
`title-patterns.csv` (title.patterns), `title-taxonomy.csv`, `status-codes.csv`,
and the step 02/05/06 code.

## 1. Staff titles mapped to board standards

286 crosswalk variants contain a staff word (COORDINATOR, COACH, ASSISTANT,
MANAGER, ADMINISTRATOR, PROGRAM, YOUTH, ...) but map to a BOARD standard.

| Variant | Maps to | Concern |
|---|---|---|
| VOLUNTEER COORDINATOR | BOARD MEMBER | a staff or volunteer job, not a seat |
| YOUTH DIRECTOR | BOARD MEMBER | a church or youth-program staff job |
| ACTIVITIES / ADOPTION / AWARDS / BASEBALL COORDINATOR | BOARD MEMBER | same pattern |
| ASSISTANT HEAD COACH | BOARD MEMBER | staff (PROFESSIONAL: COACH) |
| BOARD ASSISTANT, BOARD MANAGER, BOARD ATTORNEY | BOARD MEMBER | may be staff who serve the board |

Some of these may be intentional. Volunteer-run clubs (sports leagues, PTOs)
list their volunteer coordinators as directors, and step 09 can still make a
paid coordinator staff from pay and hours. The review should separate:
- **seat titles**: a coordinator who is listed as a director of a volunteer
  club;
- **job titles**: these should map to a staff standard, and let step 09 move
  an unpaid holder to BOARD.

Reproduce:

```r
x <- get_title_xwalk()
x[grepl("^BOARD", x$title.standard) &
  grepl("COORDINATOR|COACH|ASSISTANT|MANAGER|ADMINISTRATOR|PROGRAM|YOUTH", x$title.variant), ]
```

## 2. Assistant and deputy board offices

Assistant and deputy board offices are mapped inconsistently:

| Variant | Maps to |
|---|---|
| ASSISTANT SECRETARY | ASSISTANT SECRETARY (staff, STAFF level) |
| ASSISTANT TREASURER | BOARD TREASURER (strata subordinate / assistant) |
| DEPUTY TREASURER, ASSOCIATE TREASURER | BOARD TREASURER |
| DEPUTY SECRETARY, ASSOCIATE SECRETARY | BOARD SECRETARY |
| ASSISTANT SECRETARY TREASURER | BOARD TREASURER |

Decide one rule for assistant, deputy and associate board officers. For a
board seat, `role.board` then counts the assistant as the treasurer or
secretary, and `strata.label` is the only trace of "assistant".

## 3. Pattern rules that over-reach

Learned head rules (`title.patterns`) apply when the crosswalk has no entry.
- **Prefix `YOUTH` → BOARD MEMBER** (precision 0.875, support 16). YOUTH
  SOCCER COACH becomes BOARD MEMBER, because the suffix COACH has no rule
  and the prefix rule wins. A coach title should win over a sports or
  program prefix.
- **28 rules map to BOARD MEMBER with precision under 0.9.** Their prefixes
  are activity words: BASEBALL, TRAVEL, SOCIAL, NEWSLETTER, EVENTS, PARENT,
  COMMUNITY, LEADERSHIP. They encode volunteer-club conventions (see 1), and
  apply them to titles from any organization.
- **The selection rule:** suffix versus prefix is decided by precision only.
  A job-noun suffix (COACH, COORDINATOR, MANAGER) should probably outrank a
  topic prefix.

## 4. Frequent titles left uncoded

From the 2019-2023 calibration sample (`perf-uncoded-top.csv`):
- **EXECUTIVE OFFICER** (51 rows): no crosswalk entry, and the pattern step
  finds no rule. It is probably a CEO or OFFICER, decided by pay.
- **GENERAL** (47): probably a truncation (GENERAL MANAGER, GENERAL COUNSEL);
  decide whether to map it or leave it.
- **STAFF PHYSIATRIST** (35): should map to DOCTOR (PROFESSIONAL).
- **SENIOR RABBI, PARISH ADMINISTRATOR, CAMPUS ADVOCATE**: seen in examples.
  SENIOR PASTOR maps to PASTOR, but SENIOR RABBI is missing.

## 5. Spelling and status code oddities

These are in code, not the crosswalk:
- **PROGRAM MANAGER is rewritten as PROGRAMS MANAGER** (step 05), and the
  crosswalk standard is PROGRAMS MANAGER. Harmless for matching, but odd in
  the output. Check whether the plural is intended.
- **A title ending in NEW is recoded as FORMER**
  (`R/06-gen-status-codes.R`, `gsub("\\bNEW$", "FORMER", ...)`). NEW usually
  means newly started, which is FUTURE. NEW is also listed under FUTURE in
  `status-codes.csv`, so the two contradict each other.
- **"Trustee since 2019"**: step 02 removes the year but leaves `DATE.X = 0`.
  SINCE then sets `future.x`. The date flag should be set whenever a date is
  removed.
- **PRESENT is an INTERIM variant** in `status-codes.csv`. "2019 - PRESENT"
  means current, not interim.
- **CHAPTER becomes REGIONAL**: ALPHA CHAPTER ADVISOR becomes ALPHA REGIONAL
  ADVISOR, rewriting the title text, not just the flag.

## 6. Task: review the full crosswalk for similar issues

The cases above were found by accident. A systematic pass should cover:

1. **Board standards.** List every variant mapped to a BOARD standard whose
   words name a job (the regex in section 1, extended). Classify each as a
   seat or a job. Use the calibration sample's checkboxes as evidence: the
   share of holders with the trustee box against the key-employee box, and
   the share paid.
2. **Staff and officer standards.** The reverse check: variants mapped to
   job standards whose holders mostly tick only the trustee box and are
   unpaid.
3. **Rank words.** ASSISTANT, DEPUTY, ASSOCIATE, VICE, SENIOR and EXECUTIVE
   should be handled the same way across the board offices and the C-suite
   (`strata`, `strata.label`).
4. **Pattern rules.** Re-score `title.patterns` on the 2019-2023 sample
   (titles with a crosswalk entry as truth). Flag rules under 0.9 precision
   whose head is a topic, not a job noun. Consider letting job-noun suffixes
   win over topic prefixes.
5. **Uncoded titles.** Add crosswalk entries for the most frequent uncoded
   titles (`perf-uncoded-top.csv`, extended to the top 200).
6. **Status codes.** Check each `status-codes.csv` variant against its
   qualifier (NEW, PRESENT, SINCE), and the hard-coded edge cases in
   `R/06-gen-status-codes.R`.
7. **Measure.** Re-run `09-gold-check.R`, `14-panel-calibrate.R` and
   `21-performance.R`. Record the changes in coverage, accuracy and
   checkbox agreement in FINDINGS.md, and rebaseline the regression
   reference.

Tooling: the checkbox and pay evidence per variant can be built from
`PARTVII_DIR/sample/classified-*.rds`, which has `title.v7`,
`title.standard`, the boxes and `tot.comp` for 60,520 filings.
