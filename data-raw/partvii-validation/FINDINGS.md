# Part VII title review: findings log

Findings from running the classifier on every Part VII Section A title,
TY2009-2024, and from the title review. Each finding has an id (F-nnn), the
evidence, and its status: **open** (to decide or fix), **fixed** (the commit
or crosswalk change that fixed it), or **won't fix** (with the reason). The
process is in [REVIEW-PROCESS.md](REVIEW-PROCESS.md).

## Round 0: baseline (2026-10-08)

Run: `01-build-panel.R` through `04-write-review-files.R`, with the crosswalk
snapshots at commit 8ec313f.

- **Source:** efile_v2_3 `F9-P07-T01-COMPENSATION`, TY2009-2024, 990 and
  990-EZ. That's 58,675,305 person-rows from about 6.0 million filings, with
  1,630,208 distinct raw titles.
- **Classified:** 61,281,441 person-title rows after splitting multi-title
  entries. They contain 618,164 distinct standardized titles (`TitleTxt7`,
  the title matched to the crosswalk).
- **Shortcut check:** `00-check-equivalence.R` confirms that running steps
  1-6 once per distinct raw title, and step 7 per (raw title x officer box x
  paid x 40+ hours), gives the same `TitleTxt7` and `title.standard` as the
  full pipeline on the 2023 demo data (4,137 person-title rows).

| Tier | Rule | Titles | Person-title rows | Share of rows | Rows with no `title.standard` |
|---|---|---|---|---|---|
| 1 | 1,000+ rows | 923 | 56,930,624 | 92.9% | 0.61% |
| 2 | 100-999 rows | 6,046 | 1,628,889 | 2.7% | 56.9% |
| 3 | under 100 rows, 5+ filings | 123,630 | 1,769,490 | 2.9% | 94.1% |
| 4 | the rest | 487,565 | 952,438 | 1.6% | 99.8% |
| all | | 618,164 | 61,281,441 | 100% | **6.35%** |

| Queue | Tier 1 | Tier 2 | Tier 3 | Tier 4 |
|---|---|---|---|---|
| ADD (not in crosswalk) | 129 | 3,913 | 121,537 | 487,386 |
| INSPECT (check raised) | 102 | 270 | 189 | 13 |
| CONFIRM | 692 | 1,863 | 1,904 | 166 |

The crosswalk covers the head of the distribution well and the tail barely.
Tiers 1-2 (6,969 titles) are where one-by-one review pays off. Tiers 3-4 need
cleaning fixes and pattern rules more than crosswalk rows.

## Findings

### F-001 Duplicate taxonomy row for COMPTROLLER (fixed)

`xwalk-title-taxonomy.csv` had two rows for `COMPTROLLER` (lines 130-131):
one with SOC codes, one without. `categorize_titles()` merges on
`title.standard`, so every COMPTROLLER and CONTROLLER person-row came out
twice. That's about 42,000 people counted twice in this panel.

**Fixed 2026-10-08:** deleted the row without SOC codes from
`inst/extdata/crosswalks/xwalk-title-taxonomy.csv`. Tests and the regression
check pass.
- The regression baseline uses its own frozen copy (`data-raw/demo/`), which
  keeps the duplicate on purpose, so the baseline did not change.
- The sheet is retired (F-019), so the duplicate cannot come back from it.
  `build-crosswalks.R` refuses to build with a duplicate standard, and the
  tests check for one.

`03-title-profiles.R` also keeps only the first row of any duplicated
standard, as a guard.

### F-002 Standards with no taxonomy row (fixed)

Five `title.standard` values have no taxonomy row, so their holders get no
category:
- `CHIEF _X_ OFFICER` (135 crosswalk variants)
- `CHEIF _X_ OFFICER`
- `MEMBERSHIP AND PUBLIC RELATIONS`
- `MUSEUM GUIDE`
- `SCHED O`

`CHIEF _X_ OFFICER` is a placeholder, and the second spelling is a typo of it.
Together they affect 139 standardized titles, including:
- CHIEF COMPLIANCE OFFICER (2,634 rows)
- CHIEF PEOPLE OFFICER (2,179)
- CCO (1,913)
- CHIEF COMMUNICATIONS OFFICER (1,585)

**Decision needed:** give each chief-officer title its own standard (and
taxonomy row), or add one taxonomy row for a generic chief officer.

**Fixed:**
- The CHIEF _X_ OFFICER variants map to the generic CHIEF OFFICER (P1, round 1).
- On 2026-10-08:
  - SCHED O and ATTACHED (see the attached schedule) map to NO TITLE (P4);
  - MEMBERSHIP AND PR maps to MEMBERSHIP;
  - MUSEUM GUIDE gets a taxonomy row (STAFF, SOC 39-7011).

  `applied-log.csv` records these, with id F-002.
- Every standard now has a taxonomy row, and `test-crosswalks.R` allows no
  exceptions.

### F-003 BOARD MEMBER used as a default for unrelated titles (open)

`BOARD MEMBER` is the standard for 1,531 crosswalk variants. Some of them are
paid, full-time professional titles. The `BOARD_BUT_PAID_FT` check flags 127
titles where more than half the holders are paid and the median is 35+ hours
a week. Examples:

| Title | Rows | Paid | Median hours |
|---|---|---|---|
| PEDIATRICIAN | 2,142 | 100% | 40 |
| CARDIOLOGIST | 1,590 | 99% | 43 |
| LINEMAN | 2,446 | 100% | 45 |
| PROVIDER | 2,359 | 92% | 40 |
| GOLF COURSE SUPERINTENDENT | 1,879 | 100% | 40 |
| SENIOR DIRECTOR | 7,019 | 71% | 40 |
| HEAD OF SCHO (truncated) | 3,063 | 89% | 40 |
| CPO | 2,223 | 92% | 40 |

**Action:** review every `BOARD_BUT_PAID_FT` title, and in tier 1-2 every
mapped title whose standard is BOARD MEMBER.

### F-004 Officer titles of lodges and fraternal orders mapped to BOARD MEMBER (open, policy)

Many tier 1 titles have 60-90% of holders ticking the officer box, but are
mapped to BOARD MEMBER. Examples:
- LEADING KNIGHT, LOYAL KNIGHT, LECTURING KNIGHT
- TILER, INSIDE GUARD, OUTSIDE GUARD
- SENIOR WARDEN, JUNIOR WARDEN, JUNIOR GOVERNOR
- SERGEANT AT ARMS, MASTER, GRAND MASTER, SENIOR VICE COMMITTEE

A few are mapped to executive standards: POTENTATE → CEO and WORSHIPFUL
MASTER → PASTOR. Those holders are unpaid and work a few hours a week, so the
`EXEC_BUT_UNPAID_PT` check flags them.

**Decision needed:** a standard for "officer of a membership organization"
(for example `BOARD OFFICER`, or the taxonomy's `mem` flag with an officer
flag), and a rule for presiding titles (POTENTATE, WORSHIPFUL MASTER, EXALTED
RULER) versus CEO.

### F-005 Mappings that look wrong among frequent titles (open)

These are in the CONFIRM or INSPECT queues and are listed here so they are
decided early:

| Title | Rows | Current standard | Evidence |
|---|---|---|---|
| UNION TRUSTEE | 32,141 | BOARD TREASURER | a Taft-Hartley fund trustee, not a treasurer |
| SECRETARY TREASURER | 50,443 | BOARD MEMBER | 90% officer box; a combined officer title |
| OFFICER | 156,838 | BOARD MEMBER | 64% officer box |
| EXECUTIVE | 19,842 | BOARD MEMBER | 54% paid, 20 hours a week median |
| ADJUTANT | 26,835 | CHIEF ADMINISTRATIVE OFFICER | 7% paid, 3 hours a week (a veterans-post officer) |
| REGIONAL | 22,054 | DIRECTOR | a fragment left by a split (see F-007) |

### F-006 Step 3 joins some multi-title entries instead of splitting them (open, code)

"PRESIDENT/CEO/EX-OFFICIO", "PRESIDENT & CEO - FORMER" and "PRESIDENT/CEO -
PART YEAR" come out of `standardize_conj()` as "PRESIDENT AND CEO AND
EX-OFFICIO". Here AND is treated as part of a title, not as a separator. After
the status words are removed, "PRESIDENT AND CEO" (1,516 rows) is left as one
title, which the crosswalk does not have. A plain "PRESIDENT/CEO" splits
correctly, so it is the extra element (a status word or a third title) that
changes the decision.

**Fix:** in `R/03-standardize-conj.R`, treat `/` and `&` between known titles
as separators regardless of what follows. Alternatively, run the status-word
step before the conjunction step. Check against the regression baseline.

### F-007 Status words and boilerplate left behind as titles (open, code)

Leftover fragments become titles of their own:

- **Date qualifiers after the date is removed:**
  - "DIRECTOR - THRU 6/16" → DIRECTOR + **THRU** (3,776 rows)
  - **UNTIL** (3,682)
  - **PAST** (2,633)
  - **PART YEAR** (2,965)
  - **PARTIAL YEAR** (1,892)

  Step 2 removes the date, step 3 splits on the dash, and the qualifier is
  then a title by itself. Step 6 does not remove it because it is the whole
  title.
- **Single letters:**
  - **S** (1,588 rows): "DIRECTOR & S", "DIRECTOR - S"
  - **P** (2,119)
  - **CO** (12,500)

  Some are abbreviations (S = secretary?), others truncations.
- **Boilerplate:**
  - "SEE SCHEDULE O - O & T TITLES" → **AND O AND TRUSTEE TITLES** (4,755 rows)
  - "DISCLOSURE - PGS 34 & 38" → **DISCLOSURE AND PGS** (1,725 rows)
- **Empty titles:** 102,524 rows have no title after cleaning:
  - 93,801 blank raw titles
  - "NA", "N A", "0", ".", "1" and similar
  - "FOUNDER AND FOUNDER AND", which ends up empty

**Fix:** in step 6 (or a new check after it), drop a split title that is only
a status word, date qualifier or single letter, and set the matching flag on
the person. Recognize "SEE SCHEDULE O" boilerplate as the whole entry
(`SCHED.O.X`) rather than removing only the phrase. Empty titles should stay
unmatched, but as an explicit `title.standard` (for example `NO TITLE`), so
they are not mixed up with unmapped titles.

### F-008 Truncated titles (open, code or crosswalk)

The e-file title field is often cut off. Step 5 completes some truncations
(MANAGEMENT T → MANAGEMENT TRUSTEE), but not others:
- HEAD OF SCHO
- LECTURING KN
- REAR COMMODO
- LEGAL COUNSE
- EXECUTIVE VICE PR
- SERGEANT AT ARM

**Fix:** a step 5 rule that completes a final partial word when exactly one
crosswalk title starts with that prefix. Or add the truncated forms to the
crosswalk where the rule would be ambiguous.

### F-009 Executive-director family missing from the crosswalk (open, crosswalk)

The largest tier 1 ADD titles are deputies and co-holders of the top job:

| Title | Rows |
|---|---|
| CO-EXECUTIVE DIRECTOR | 11,227 |
| EXECUTIVE OFFICER | 7,295 |
| ASSOCIATE EXECUTIVE DIRECTOR | 6,520 |
| ASSISTANT EXECUTIVE DIRECTOR | 6,310 |
| DEPUTY EXECUTIVE DIRECTOR | 5,061 |
| PRESIDENT AND CEO | 1,516 (see F-006) |

CO-EXECUTIVE DIRECTOR keeps its `CO-` prefix because step 6 flags `CO.X`
without removing it. The crosswalk has CO-PRESIDENT and CO-CHAIR, but not the
co- form of every title.

**Decision needed:** remove `CO-` in step 6 and keep `CO.X` (as the notes tab
already asks), or add each co- form to the crosswalk.

### F-010 The pending suggestion lists are not applied (open)

Two lists of proposed additions exist but are not in the snapshot:
- the `to-do` tab of `data-dev/title-taxonomy-map.xlsx` (757 variants)
- `data-dev/board_crosswalk_patch.csv` (37 variants)

The review files show them as `suggested_standard` where they match a
`TitleTxt7`, so they get confirmed or corrected in the normal review rather
than applied in bulk.

### F-011 `missing_title_variants()` cannot run (open, code)

`R/identify-missing-titles.R` refers to an undefined `variant` (lines 114,
125 and others), so the helper for unmatched titles errors out. The review
files and `queue-add.csv` now cover its purpose. Fix it or remove it.

## Taxonomy review (2026-10-08)

A review of the `title-taxonomy` tab against the 2018 SOC table and the role
plans in `synthid/dev/` (`PLAN-title-role-refinement.md`). Counts are from the
2010-12 classified slice (35,930 rows). The full review and the phased plan
are in `data-dev/CROSSWALK-REVIEW.md`.

F-012 to F-015 are fixed by `07-taxonomy-fixes.R`, which logs every changed
cell in `taxonomy-edits-log.csv` (266 cells, 198 rows).
`tests/testthat/test-crosswalks.R` now checks for them. The edits were never
entered in the Google Sheet, which was retired the same day (F-019). The
regression baseline keeps its own frozen crosswalks, so it did not change. On
the 2023 demo data with the fixed taxonomy:
- rows with the ceo flag go from 42 to 137;
- filings with at least one CEO go from 10.6% to 31.9%;
- rows with a `soc.label` go from 3,709 to 560, because board rows no longer
  carry the label "board".

### F-012 CEO flag on the wrong titles (fixed)

`EXECUTIVE DIRECTOR` had `c.level` but not `ceo`, although the instructions
tab maps ED to ceo. ED is the most common title for the top staff job (673
rows in the slice, against 378 for CEO). Meanwhile `ASSISTANT CEO`,
`ASSOCIATE CEO`, `DEPUTY CEO`, `VICE CEO`, `PRESIDENT OF MEDICAL STAFF` and
`POLITICAL DIRECTOR` had `ceo`.

**Fixed:**
- ceo added to EXECUTIVE DIRECTOR and GENERAL DIRECTOR.
- The deputies moved to c.level.
- PRESIDENT OF MEDICAL STAFF moved to spec (a physician).
- POLITICAL DIRECTOR moved to emp + dir.vp.

**Left as is:**
- MANAGING DIRECTOR: holders are mixed (31% trustee box, median pay $21k,
  24 hours a week), so the step-09 cascade should decide.
- ORGANIZATION DIRECTOR: no holders in the slice.

### F-013 Support and editorial titles filed as managers (fixed)

EXECUTIVE ASSISTANT, EXECUTIVE SECRETARY, EDITOR and ORGANIZER had `mgr`.
Moved to `spec`.

### F-014 Employee level without the emp flag (fixed)

BUSINESS AGENT and BUSINESS REPRESENTATIVE had `spec` with `emp` blank, so
they counted in no group. Added `emp`. The test now requires every employee
level to have `emp` and every board level to have `board`.

### F-015 SOC codes and labels (fixed)

Fixed:
- **Codes that contradict the title:**
  - CHIEF CIVIC PROGRAMS OFFICER and CIVIC DIRECTOR had 11-9051 (Food
    Service Managers); changed to 11-9151.
  - ATHLETE, SPORTS, SPORTS DIRECTOR and VICE PRESIDENT OF SOFTBALL had
    27-2023 (Umpires). Now 27-2021, broad group only, 27-2022 and broad group
    only.
  - ARTIST had a detailed code from another broad group.
- **Placeholders and whitespace:**
  - the word "board" in every SOC column of 10 board rows;
  - "xxx" and "missing" labels;
  - trailing spaces in COMPTROLLER and WEBMASTER codes.
- **Labels:** `soc.label` was hand-typed, and 88 of 219 labels did not match
  an official title. It is now the official 2018 title of the most detailed
  code given. Labels on rows without a code are unchanged.

The codes are text. **Opening a crosswalk table in Excel turns them into
dates** (11-2033 becomes 48884), as did the sheet's .xlsx export.

### F-016 Board taxonomy: rows with no board level, and an ambiguous `mem` (fixed, see F-020)

99.8% of board rows land on five standards: BOARD MEMBER, BOARD PRESIDENT,
BOARD TREASURER, BOARD SECRETARY and BOARD VICE PRESIDENT. Of the 32 board
taxonomy rows, 21 have no board level, including BOARD CHAIR, VICE CHAIR,
COUNCIL CHAIR and PRESIDENT TREASURER. `mem` is "committee member" in the
instructions tab, but the sheet uses it for a regular board member.

**Proposal:** one `board.role` column (CHAIR / VICE CHAIR / SECRETARY /
TREASURER / MEMBER). Turn the long-tail board standards into
standardization-tab variants of the five, and move committee, advisory and
ex officio to the status flags.

**Done 2026-10-08 (F-020):**
- `board.role` replaces the board flags.
- 47 board standards are now variants of the five core standards.
- Committee, advisory and ex officio are not yet status flags: those titles
  map to BOARD MEMBER.

### F-017 Context-dependent titles fixed to one role (fixed in step 09, see F-022)

| Title | Rows | Hard-coded mapping |
|---|---|---|
| VICE PRESIDENT | 1,998 | emp / dir.vp, although many holders tick the trustee box |
| SECRETARY | 101 | emp / spec, with SOC "Secretaries and Administrative Assistants" |
| CORPORATE SECRETARY | | same as SECRETARY |
| DIRECTOR | 283 | emp / dir.vp |

The standardization tab also sends PRESIDENT, CHAIR, DIRECTOR and SECRETARY to
board standards. Together with F-005, these titles need an `ambiguous` marker
and a decision in step 09 from the checkboxes, pay and hours, not in the
crosswalk.

### F-018 Domain vocabulary (fixed, Phase 4)

74% of employee rows have the domain `operations / administration`, because
executives are filed there. The overview tab defines a "General Management"
domain that is not used. The labels are not a controlled list:
- 28 rows have no category;
- placeholders `xxx` and `industry-specific (operations?)`;
- both `religion` and `religious`;
- `marketing-pr`, `marketing-sales` and `comms-pr` side by side.

**Fixed 2026-10-08:** `data-raw/crosswalks/03-domains.R` writes
`domains.csv`, a vocabulary of 29 category/label pairs, and moves every row
onto it (115 titles, logged in `domains-log.csv`):
- **New categories:**
  - `executive / general management`: CEOs, plus officers in administration
    with a general-management SOC code or none.
  - `governance / board`: all board titles.
- **Merged labels:** `religious` into `religion`; `comms-pr`, `marketing-pr`
  and `marketing-sales` into `marketing and communications`.
- **New label:** `housing`, for the housing operations titles.
- **Assigned one by one:** the rows with blank or placeholder domains.
- **Checks:** `test-crosswalks.R` and `06-apply-decisions.R` reject a domain
  that is not in `domains.csv`.

Employee rows in the 2010-12 slice:

| Domain | Before | After |
|---|---|---|
| operations / administration | 74% | 12% |
| executive / general management | 0% | 62% |

Most of the executive share is VICE PRESIDENT (about 2,000 rows), the
ambiguous title F-017 will split.

### F-019 The crosswalks move from the Google Sheet to the repository (done)

The pipeline read its three crosswalks from the title-taxonomy-map Google
Sheet. The package bundled snapshots of them in `inst/extdata/crosswalks/`, and
`refresh = TRUE` overwrote the snapshots from the sheet. The sheet and the
snapshots had to be kept in step by hand. F-001 and F-012 to F-015 were fixed
in the snapshots but not in the sheet, so a refresh would have undone them.

**Done 2026-10-08:**
- **Archive:** every tab of the sheet is exported unchanged to
  `data-raw/crosswalks/archive/google-sheet-2026-10-08/` by
  `00-export-google-sheet.R`.
- **Source tables:** `data-raw/crosswalks/` holds `status-codes.csv`,
  `title-standardization.csv` and `title-taxonomy.csv`.
  - Their pipeline columns are identical to the snapshots.
  - The sheet's notes columns are kept.
- **Package data:** `build-crosswalks.R` builds the tables into package data
  (`status.codes`, `title.xwalk`, `title.taxonomy` in `data/`).
- **Loaders:**
  - `get_status_codes()`, `get_title_xwalk()` and `get_title_taxonomy()`
    return the package data.
  - The `get_googlesheets_*()` names still work, and `refresh = TRUE` now
    only warns.
- **Removed:**
  - the googlesheets4 dependency;
  - `inst/extdata/crosswalks/`;
  - the stale 2022 data objects `d.taxonomy`, `df.standard` and
    `status.mapping`.
- **Scripts:** 06 and 07 edit the tables and rebuild `data/`.

The regression check is unchanged.

`fread` misreads two variants that contain literal quote characters
(`DIRECTOR """"`), so 06 would have rewritten them wrongly. The tables are now
read with `read.csv` (`_crosswalk-io.R`).

### F-020 Employee levels and board roles replace the role flags (done)

Decided by the user on 2026-10-08:
- Five employee levels, with PROFESSIONAL kept: the 990's "highest
  compensated employee" group are people reported because of their pay, not
  a managerial role (doctors, lawyers, coaches). Counselors, chefs and
  teachers are STAFF.
- The board president is called CHAIR.

`data-raw/crosswalks/01-role-levels.R` converted `title-taxonomy.csv` from
the 12 flag columns to two columns:
- `emp.level`: CEO / OFFICER / MANAGER / PROFESSIONAL / STAFF;
- `board.role`: CHAIR / VICE CHAIR / SECRETARY / TREASURER / MEMBER.

`role-levels-log.csv` records the rule behind each row:
- `dir.vp` is retired. Vice presidents and deputies go to OFFICER, and
  directors to MANAGER, except four directors the August evidence routed up.
- `spec` splits into PROFESSIONAL and STAFF: the taxonomy now has 31
  PROFESSIONAL and 109 STAFF standards.
- Club, lodge and post officers become board roles (DECISIONS.md P2).
- A new COACH standard (PROFESSIONAL, SOC 27-2022) is added.
- Board standards other than the core five are collapsed into them
  (`board-collapse-log.csv`).

The script also brought PR #9's drafts along:
- Three drafts that targeted UNION REPRESENTATIVE now target BOARD MEMBER.
- `taxonomy-additions.csv` now gives a level name instead of flags, and
  `06-apply-decisions.R` reads that form.

`build-crosswalks.R` derives the legacy flags from the two columns, so the
pipeline's flag and count columns remain. `c.level` now includes every CEO,
`spec` is PROFESSIONAL or STAFF, and `dir.vp` is always blank.
`categorize_titles()` also outputs `emp.level` and `board.role`.

The regression reference was rebuilt on purpose:
`build-demo.R --reference-only` re-pins the crosswalks and keeps the demo
sample. The new reference has md5 `a0e82dd1f1f5a8dc7169759de1ddfe90` and
4,137 rows x 103 columns. Same rows and order; the changes are:

| Change | Rows | Source |
|---|---|---|
| `emp.level` and `board.role` added | all | F-020 |
| ceo on EXECUTIVE DIRECTOR | 97 | F-012, new to the frozen copy |
| board rows lose the `board` placeholder in their SOC columns | about 3,100 | F-015, new to the frozen copy |
| `dir.vp` retired; VICE PRESIDENT and DEPUTY DIRECTOR become c.level, DIRECTOR titles become mgr | 331 | F-020 |
| FINANCE OFFICER becomes mgr; FOUNDER becomes c.level | 8 | F-020 |
| SERGEANT AT ARMS, VICE COMMANDER and PRESIDENT TREASURER fold into the core board standards | 10 | F-016 |

Still open:
- Coach variants. `COACH` maps to BOARD MEMBER, which is right for youth
  leagues but not for paid coaches. The per-title review should decide which
  variants go to COACH.
- VICE PRESIDENT is OFFICER by default and is often a board vice president
  (F-017).
- The level of a few titles is a judgment call worth a look in review:
  - FIRE CHIEF (OFFICER)
  - FOUNDER (OFFICER)
  - LINEMAN (STAFF)
  - MANAGING DIRECTOR (OFFICER)
  - GENERAL MANAGER (MANAGER)

### F-021 SOC coverage (done, Phase 5)

Half the employee titles had no SOC code:
- 232 of 429 had no broad or detailed code;
- 82% of employee rows in the 2010-12 slice were coded;
- every functional chief officer had the generic 11-1021 (General and
  Operations Managers).

`data-raw/crosswalks/02-soc-codes.R` assigns codes by one rule: a SOC code
describes the title's function, not its rank, because `emp.level` carries the
rank.
- **Functional chief officers** take their function's code:
  - CHRO 11-3121
  - CIO, CTO and CDO-digital 11-3021
  - development, advancement and philanthropy 11-2033
  - CMO-marketing 11-2021
  - CLO 23-1011
  - COO 11-1011
- **Health, education, programs, finance, fundraising, HR, legal, museum,
  public safety, facilities and hospitality titles** take their occupation's
  code, at the most specific level that fits. For example, PROFESSOR gets the
  minor group 25-1000 and DOCTOR the broad group 29-1210.
- **Titles left uncoded:** those whose function the words don't settle,
  including DIRECTOR, STAFF, MILITARY, CAPTAIN, CMO (medical or marketing),
  CREATIVE DIRECTOR, PROGRAMMING and fraternal offices.

The script fills the parent groups and the official 2018 label from
`tidy-soc-codes.csv`, and stops on an unknown code. It changed 170 titles,
logged in `soc-codes-log.csv`.

| Coverage of employee titles and rows | Before | After |
|---|---|---|
| Titles with a minor-or-finer code | 46% | 81% |
| Rows with a minor-or-finer code | 82% | 92.5% |
| Rows with a broad-or-finer code | 82% | 92% |

### F-022 Step 09 resolves roles from checkboxes, pay and title (done, Phase 3)

The crosswalk gives a title one role. On Part VII the same title can be a
paid executive or a volunteer board seat (F-017). Step 09
(`conditional_logic()`, now `resolve_roles()`) used to be an unwired stub. It
now carries the August rules from `synthid/dev/06_role_cascade_prototype.R` and
`10_role_passes.R`:
- **Position:** the checkboxes come first, and a paid executive title outranks
  a missing officer box. 990-EZ returns, which have no checkboxes, use the
  title and pay.
- **Person:** split titles resolve to one role per person, and `role.primary`
  marks one row per person.
- **Leadership, per filing:**
  - a designated CEO comes from the title;
  - otherwise an imputed CEO is the highest-paid plausible leader;
  - otherwise the filing is `board_governed`.

Three changes from the prototype came out of testing on the 2023 demo:
- An imputed leader must work 15 or more hours a week or earn $25,000 or more.
  Without this, token pay such as $104 or $599 made a CEO.
- 990-EZ returns are recognized by `formtype`, because their checkbox columns
  hold 0, not NA.
- An unpaid EXECUTIVE DIRECTOR is still the designated CEO. Only unpaid
  OFFICER-level titles on a 990-EZ (VICE PRESIDENT, deputy) are read as board.

Step 09 adds seven columns: `role.final`, `role.board`, `role.ceo`,
`role.position`, `role.source`, `role.primary` and `org.leadership`. It changes
no existing column. `classify_titles()` and the regression pipeline now run it,
and `tests/testthat/test-roles.R` pins the rules on hand-built filings.

On the 2023 demo (4,000 people in 493 filings):

| Result | |
|---|---|
| Full-990 filings with paid staff that have a CEO | 90% |
| Paid 990-EZ filings that have a CEO | 76% |
| BOARD PRESIDENT holders who are the paid chief executive (CEO) | 34 |
| VICE PRESIDENT holders | split between board (vice chair) and officer |

The rest are `board_governed`.

The regression reference was rebuilt with `build-demo.R --reference-only`:
md5 `601c0de314e173b6f72b976a2d9f7caa`, 4,135 rows x 110 columns, same rows.
It adds the seven role columns, and re-pinning the crosswalks brings in F-002,
F-018 and F-021 (domain on 3,643 rows, SOC on 58).

**Not done:** a validation of step 09 against the hand-labeled gold sample.
That sample is keyed to the 2010-12 slice, whose raw input is not in the
repository, so the gold check has to run where the slice's raw rows are.

### F-023 Step 09 scored against the hand-labeled sample (done)

`09-gold-check.R` runs steps 01-09 on the raw 2010-12 rows of the role
sample: `synthid/dev/data/slice_2010_2012_1000eins.rds`, 34,321 rows in
3,027 filings. It matches the 158 labeled people on filing + raw title + pay,
because person ids are hashes that differ between runs, and scores
`role.final` against `gold_role`.

The August labels were drafted by a rule pass, and their planned human check
was never done. Two independent labeling agents relabeled the 22 people where
step 09 and the label disagreed. Each agent saw the guide and the whole filing,
but not the old label or step 09's answer.
- **Inputs and outputs** are in `gold-check/`:
  - `relabel-cases.md` (the cases) and `relabel-key.csv` (the answer key);
  - `relabel-A.csv` and `relabel-B.csv` (each agent's labels);
  - `relabel-consensus.csv` (where they agree).
- **The agents agreed on 20 of 22.** Ten of the agreed labels change the old
  role, and 9 of those now match step 09. Examples:
  - a yacht club's general manager and a trust fund's staff administrator,
    each the only full-time head, are CEOs;
  - a parent system's CEO sitting on this board is a board member;
  - two working-board leaders are DUAL.
- **They split on two cases, left for a person to decide:**
  - C07, an irrigation company president paid $7,140 for 20 hours a week:
    board chair or CEO?
  - C12, a youth orchestra's only paid staffer, the music director: manager or
    key employee?

The relabels showed one rule worth adding: a board title held under 10 hours
a week for under $25,000 is a board seat even with the officer box (a titular
president with a stipend). On the 2023 demo it moves 37 people from OFFICER to
BOARD. The regression reference was rebuilt, md5 `16dc096b...`.

| Agreement with the labels | Original labels | With the 20 relabels |
|---|---|---|
| Step 09 as merged in #18 | 85.8% | |
| Plus the board-seat rule | 87.1% | **92.9%** |

With the relabels and the rule, by role:

| Role | Precision | Recall |
|---|---|---|
| CEO | 0.73 | 0.95 |
| BOARD | 0.98 | 0.95 |
| OFFICER | 0.88 | 0.93 |
| MANAGER | 1.00 | 0.64 |

**Still open:** in a board-run filing, the imputed CEO is sometimes a
functional manager, such as a maintenance manager or a lodge manager, whom the
labelers call MANAGER. That case is 4 of the remaining 11 misses. A possible
next rule would leave a functional manager title (a specific department) out
of imputation.

**Update 2026-10-09.** The user decided the two split cases. Both are CEOs:
- **C07, the irrigation company president** (paid $7,140, 20 hours a week).
  Rule: a paid president is the CEO when no one else in the filing has a CEO
  title.
- **C12, the youth orchestra's music director** (the only paid staffer, 40
  hours a week). Here "director" means the organization's head, not a
  department head.

`relabel-consensus.csv` now records who decided each case: both agents, or
the user. `09-gold-check.R` applies all 22 relabels. Both decisions match
step 09, which raises agreement from 92.9% to **94.2%**:

| Role | Precision | Recall |
|---|---|---|
| CEO | 0.78 | 0.95 |
| BOARD | 0.98 | 0.97 |
| OFFICER | 0.88 | 0.93 |
| MANAGER | 1.00 | 0.69 |

Three CEO false positives remain. Each is a maintenance or lodge manager who is
the top-paid person in a board-run filing, and the agents agreed on MANAGER for
them. The user's rule for C07 (no other CEO title, so CEO) may apply to them too.

### F-024 Role labels v2: refreshed to the current standard and doubled (done)

The August labels used the old vocabulary:
- CFO, COO and C_LEVEL_OTHER as separate roles;
- KEY_EMPLOYEE instead of PROFESSIONAL / STAFF;
- one BOARD_OFFICER covering vice chair, secretary and treasurer.

They also covered full-990, paid filings only, and two people appeared twice.
`gold-check/gold-v2.csv` replaces them with **316 people**, labeled in the role
standard and board roles of `gold-check/LABELING-GUIDE-v2.md`:
- **156 August people,** after removing the two duplicates.
  - 132 mapped mechanically (CEO -> CEO, CFO -> OFFICER,
    BOARD_CHAIR -> BOARD / CHAIR, ...).
  - 24 that needed judgment were relabeled. These were PROFESSIONAL vs STAFF,
    board posts the title doesn't name, and DUAL with no seat given.
  - Title columns are recomputed with the current pipeline.
- **160 new people** in 10 strata covering each part of step 09's decision,
  including 40 from 990-EZ filings. There is at most one person per filing.
  The strata are designated, imputed and board-governed leaders, board titles
  with the officer box, paid ambiguous titles, professional, staff, manager,
  990-EZ paid and 990-EZ unpaid. `10-gold-v2-build.R` builds them, seed 2026.

Labeling:
- Two blind agents labeled the 184 open cases. They agreed on 178 (97%).
- The six splits and the Elks lodge manager were settled at the user's
  request (`v2-user-decisions.csv`). The manager is CEO: a full-time paid role
  running the lodge, with no other staff.
- The guide adds the user's rules for:
  - a paid president with no other CEO title;
  - a "director" who heads the organization;
  - the top-paid person in a board-run organization (part-time with no
    staff, versus full-time with staff, versus head of one function);
  - unpaid full-time leaders, such as a religious order's president.
- `11-gold-v2-assemble.R` builds `gold-v2.csv`, and `09-gold-check.R` scores
  it into `gold-v2-results.csv`.

Step 09 against v2 (312 scored, 4 UNSURE): **86.5%**.

| Set | People | Agreement |
|---|---|---|
| August | 153 | 93.5% |
| New | 159 | 79.9% |
| Full 990 | 272 | 88.6% |
| 990-EZ | 40 | **72.5%** |

| Role | Precision | Recall |
|---|---|---|
| CEO | 0.73 | 0.96 |
| OFFICER | 0.80 | 0.67 |
| MANAGER | 0.95 | 0.64 |
| PROFESSIONAL | 0.86 | 0.95 |
| STAFF | 0.24 | 0.58 |
| BOARD | 0.95 | 0.89 |

Board seats get the right board role 97% of the time.

Weak spots to work on:
- **STAFF is over-assigned.** Eight managers and six board members are called
  STAFF.
- **990-EZ.**
- **No unpaid CEO.** Step 09 never makes an unpaid full-time officer the CEO
  (the Little Sisters of the Poor case, guide rule 7). That rule is pending,
  until the round-2 cleaning work in R/05 and R/06 merges.

The labelers also found gaps in the guide:
- when a paid executive who also ticks the trustee box should be DUAL rather
  than CEO;
- stipends for officers other than the president;
- related-organization executives.

### F-025 A 2019-2023 sample for calibrating step 09 at scale (sample drawn)

The labeled set (F-024) is for measuring accuracy, not for training. The
synthid plan calls for something else: **silver labels** from full-990 filers.
These are the step 09 roles in strata where the checkboxes, pay and title
agree. They come first, by stratum, and the lessons then carry over to 990-EZ
filers, which have no checkboxes. `12-panel-sample.R` draws the sample.

**Source:** NCCS `efile_v2_3` Parquet tables for tax years 2019-2023:
- Part I summary, with total expenses for 990 and 990-EZ returns;
- Part VII compensation.

Files are stored in `~/Documents/PARTVII/efile` (about 880 MB). Tax year 2024
was left out because it is still incomplete: its Part I file is 246 MB,
against 305 MB for 2023.

**Frame:** 2,538,408 filings from 647,098 organizations, keeping one filing
per organization-year (the latest submission). 355,674 organizations (55%)
file in all five years.

**Size strata, by total expenses.** $0 is its own bin.

| Share of filings with any paid person | $0 | $1-150k |
|---|---|---|
| 990 | 2.9% | 16% |
| 990-EZ | 0.9% | 15% |

- $0 filers are 1.2% of filings. Their median board is smaller: 4 people,
  against 6, on the 990.
- 43% of $0 organizations report expenses in another year, so many are
  dormant years rather than dormant organizations.
- The 256 negative-expense filings are put in $0.
- 990-EZ returns with no expense figure (19,037) get a "missing" bin.

**Other strata:**
- paid people on Part VII: 0, 1, 2-4, 5+;
- form: 990 or 990-EZ.

**Sample:** 60,520 filings from 35,396 organizations.
- **Panel:** 6,027 organizations that file all five years, stratified by their
  2019 cell and kept in every year. That gives 30,135 filings and supports the
  cross-time consistency check.
- **Fresh:** 30,385 filings, about 6,000 a year from organizations outside the
  panel.
- **Allocation:** proportional to the square root of each cell's population,
  with a floor of 25 per cell. Each filing carries a weight (cell population /
  sample).
- **Large organizations are well represented:** 2,134 filings over $100m and
  5,679 at $10m-100m. The 990-EZ part has about 17,000 filings.

`panel-sample-cells.csv` gives the counts by cell.

**Next:** run steps 01-09 on the sampled filings, then:
- calibrate by stratum: how often the checkboxes, title and pay agree;
- check roles across time in the panel;
- build the silver label set.
