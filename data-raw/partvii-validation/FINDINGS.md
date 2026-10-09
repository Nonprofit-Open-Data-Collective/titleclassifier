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

### F-001 Duplicate taxonomy row for COMPTROLLER (open, fix ready)

`xwalk-title-taxonomy.csv` has two rows for `COMPTROLLER` (lines 130-131):
one with SOC codes, one without. `categorize_titles()` merges on
`title.standard`, so every COMPTROLLER and CONTROLLER person-row comes out
twice. That's about 42,000 people counted twice in this panel.

**Fix:** delete the row without SOC codes, in the snapshot and in the Google
Sheet. `03-title-profiles.R` keeps the first row until then.

### F-002 Standards with no taxonomy row (open)

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
