# Title crosswalk review + improvement plan (2026-10-08)

Scope: the `title-taxonomy` tab of the title-taxonomy-map Google Sheet
(`title.standard` → domain, SOC, employee hierarchy, board hierarchy), checked
against the 2018 SOC table (`data-raw/standard-occupational-classifications/tidy-soc-codes.csv`)
and weighted by frequency in the 35,930-row classified slice
(`synthid/dev/data/classified_slice.rds`, 2010–2012, 1,000 orgs).

> **Status (2026-10-08, branch `crosswalk-phase0-1`).** Findings are logged as
> F-012 to F-019 in `data-raw/partvii-validation/FINDINGS.md`.
> - **The Google Sheet is retired (F-019).** The crosswalks are now CSV tables
>   in `data-raw/crosswalks/`, built into package data, and every tab is
>   archived there. Mentions of "the sheet" below describe where the
>   crosswalk lived when this review was written.
> - **Done:**
>   - Phase 1 items 1, 2 and 4, through `07-taxonomy-fixes.R`
>     (`taxonomy-edits-log.csv`). The duplicate COMPTROLLER row had already
>     been removed (F-001).
>   - The Phase 0 checks, in `tests/testthat/test-crosswalks.R`, and the
>     archive of the old tabs.
>   - Phase 2 (F-020): `emp.level` with five levels (PROFESSIONAL kept) and
>     `board.role` (president → CHAIR) replace the flags in the table. The
>     legacy flags are derived in the package data. Board standards are
>     collapsed into the core five. The decisions below are settled.
>   - Every standard has a taxonomy row (F-002).
>   - Phase 5, SOC coded by function (F-021): employee rows with a code go
>     from 82% to 92.5%.
>   - Phase 4, a controlled domain vocabulary in `domains.csv`, with executive
>     and governance (F-018).
>   - Phase 3, step 09 `resolve_roles()` (F-022): roles come from the
>     checkboxes, pay and title, settling PRESIDENT, VICE PRESIDENT and
>     DIRECTOR by context, with designated, imputed or board-governed
>     leadership.
> - **Not done:** Phase 1 item 3. The Part VII review process routes pending
>   suggestion lists through the per-title review instead of a bulk apply
>   (F-010), so the August list goes there.

Builds on the August role-refinement work in `synthid/dev/`
(`PLAN-title-role-refinement.md`, `CROSSWALK-UPDATES.md`,
`crosswalk_updates_consolidated.csv`, `dirvp_crosswalk_suggestions.csv`).

---

## 1. What the earlier plans already decided

- **Simplify to a coarse role layer.** The guaranteed output is
  `role_coarse` ∈ {CEO, BOARD, OFFICER, MANAGER, KEY, STAFF}. Fine labels are kept only where reliable.
- **Retire `dir.vp`.** It mixes board and staff (median comp $8,986, 55% paid).
  Route it up to OFFICER (`VICE PRESIDENT OF <function>`, `DEPUTY DIRECTOR`) or down to
  MANAGER (`DIRECTOR`, `DIRECTOR OF <dept>`). The proposed routing is in `dirvp_crosswalk_suggestions.csv`.
- `c.level` and `spec` are coherent, so keep them. `mgr` is small (n≈107). KEY vs MANAGER is
  split by title and comp.
- **Board tiers.** Optimize board *membership* and board *count*. The officer roles
  (pres/sec/treas) are REPORT-only, and multiples are allowed (transition or shared).
- **The crosswalk stays 1:1 and context-free.** Context-dependent titles (PRESIDENT, VP, DIRECTOR,
  CHAIR, SECRETARY, WARDEN) are resolved in the step-09 cascade, not in the sheet.
- **Pending and never applied:** the 650-title consolidated update (ADD_BOARD 13, ROUTE_DIRVP 45,
  FIX_MISMAP 57, ADD_UNMAPPED 376, FAILED_SPLIT 159). The live sheet confirms none of it is in:
  `REGULAR TRUSTEE` and `DELEGATE TRUSTEE` are still unmapped, `LINEMAN`, `OFFICER`, `ADMINISTRATIVE`
  and `SEE SCHEDULE O` still map to BOARD MEMBER, and `BOARD AND DIRECTOR` still maps to DIRECTOR.

---

## 2. Findings from this review

### 2a. Structural / hygiene
| Issue | Impact |
|---|---|
| **`COMPTROLLER` appears twice** (one row with SOC, one without) | The `merge(all.x=TRUE)` in step 08 **duplicates every COMPTROLLER person-row** (50 rows in slice → 100). |
| Rows keyed `NA` / `missing`. Std targets with no taxonomy row: `CHIEF _X_ OFFICER` (template leak, 20 rows), `"NA"`, `SCHED O`, `MUSEUM GUIDE`, `MEMBERSHIP AND PUBLIC RELATIONS` | Silent NAs downstream |
| 90 taxonomy rows are never produced by the standardization tab | Some come from code (e.g. `CFO` via `replace_cfo`). The rest are dead rows. Verify against the full corpus before pruning. |
| `FLAG` column is empty. Parallel tabs (`taxonomy_updated`, `temp`, `temp2`) drift: they use `domainl.label` and `com` where this tab uses `domain.label` and `mem` | Unclear which tab is authoritative |
| **xlsx export turns SOC codes into dates** (e.g. `11-2033` → 48884) | The bundled CSV snapshot is fine. Never round-trip the sheet through Excel. |

### 2b. Employee hierarchy (`emp, ceo, c.level, dir.vp, mgr, spec`)
- **`EXECUTIVE DIRECTOR` is not flagged `ceo`** (c.level only). Neither are `MANAGING DIRECTOR`,
  `GENERAL DIRECTOR` or `ORGANIZATION DIRECTOR`. ED is the most common CEO title in the slice:
  673 rows vs 378 for `CEO`. The `instructions` tab itself says ED → ceo.
  This is the single largest context-free error and a big part of the pre-cascade 64% CEO coverage.
- **Deputies are flagged as CEO:** `ASSISTANT CEO`, `DEPUTY CEO`, `VICE CEO` and `ASSOCIATE CEO`
  inflate CEO counts. `PRESIDENT OF MEDICAL STAFF` (a physician-staff chief), `MUSEUM DIRECTOR` and
  `POLITICAL DIRECTOR` are also flagged ceo. `POLITICAL DIRECTOR` additionally has emp blank.
- **Context-dependent titles are hard-coded:**
  - `VICE PRESIDENT` → emp/dir.vp (1,998 rows, the 5th most common title). The August
    analysis found 584 of these persons had the trustee box.
  - `SECRETARY` → emp/spec with SOC "Secretaries and administrative assistants" (101 rows).
    `CORPORATE SECRETARY` gets the same treatment, although it is an officer role.
  - `DIRECTOR` → dir.vp (283 rows).
  - On the standardization tab, `PRESIDENT`, `CHAIR`, `DIRECTOR` and `SECRETARY` are pre-resolved
    to BOARD PRESIDENT / BOARD MEMBER / BOARD SECRETARY. This is where the phantom board presidents come from.
- **`mgr` holds non-managers:** EXECUTIVE ASSISTANT, EXECUTIVE SECRETARY, EDITOR, ORGANIZER,
  MEDICAL OFFICER.
- 7 emp rows have no level (CAPTAIN, FIRE CHIEF, LIEUTENANT, VICE PRESIDENT AND CIO…).
  3 rows have levels but emp is blank (BUSINESS AGENT, BUSINESS REPRESENTATIVE, POLITICAL DIRECTOR).
- The senior tiers in the `overview`/`instructions` tabs (s-dir-vp, s-mgr, s-spec) were never
  implemented.

### 2c. Board hierarchy (`board, pres, vp, sec, treas, mem`)
- **99.8% of board rows land on just 5 standards:** BOARD MEMBER 19,071; BOARD PRESIDENT 3,671;
  BOARD TREASURER 2,464; BOARD SECRETARY 2,261; BOARD VICE PRESIDENT 578.
  The other 27 board rows are long-tail clutter.
- 21 of 32 board rows carry **no board level**: BOARD CHAIR, VICE CHAIR, COUNCIL CHAIR, PERSONNEL
  CHAIR, PRESIDENT TREASURER, COUNCILMAN… If any of these is ever produced, it gets board=1 with no role.
- **`mem` is ambiguous.** Per `instructions` it is "com = committee member", but in use it marks regular
  BOARD MEMBER (and COMMITTEE CHAIR).
- `REPRESENTATIVE` → board (45 rows, domain `xxx`). This was flagged in August as a staff/other mis-map.

### 2d. Domain
- Executives are filed under `operations / administration`, so **74% of employee rows carry
  the same domain** and it says nothing about leadership. The `overview` tab defines a
  *General Management* category (CEO, COO…) that is never used.
- The vocabulary is uncontrolled: 28 rows have no category. Other problems:
  - `xxx` placeholders
  - `industry-specific (operations?)`
  - `religion` vs `religious`
  - `marketing-pr`, `marketing-sales` and `comms-pr` side by side
  - `board / higher education`
  - `operations / employee`

### 2e. SOC
- 206 of 417 employee titles have a SOC label. Weighted by rows, 82% of employee rows carry a
  broad or detailed code. Much of that 82% is VICE PRESIDENT alone (11-1021).
- **Invalid or inconsistent codes:**
  - `CHIEF CIVIC PROGRAMS OFFICER` and `CIVIC DIRECTOR` → 11-9051 *Food Service Managers*;
    should be 11-9151.
  - `ARTIST` has broad group 27-1010 but detailed occupation 27-2013 (mismatched hierarchy).
  - `COMPTROLLER` has trailing spaces in `13-2090  `.
  - 10 board rows have the literal string `board` in every SOC column.
- **Labels are hand-typed:** 88 of 219 `soc.label` values don't match the official 2018 title
  ("General and operations manager", "financial manager", "Chief medical officer?",
  "artistic director vs. marketing?").
- **Function is coded inconsistently:** every `CHIEF <X> OFFICER` is coded 11-1021 *General & Operations*
  regardless of function. The SOC has functional homes for most of them:
  | Title | Functional SOC |
  |---|---|
  | COO | 11-1011 Chief Executives (SOC lists COO there) |
  | CIO/CTO | 11-3021 |
  | CHRO | 11-3121 |
  | Chief Development/Philanthropy/Advancement | 11-2033 |
  | CMO (marketing) | 11-2021 |
  | CLO | 23-1011 |
- **High-frequency uncoded titles:**
  | Title | Rows | Suggested SOC |
  |---|---|---|
  | DOCTOR | 272 | 29-1210 |
  | DIRECTOR | 283 | — (ambiguous; leave uncoded) |
  | CLERK | 50 | 43-9061 |
  | MEDICAL DIRECTOR | 22 | 11-9111 |
  | PROFESSOR | 22 | 25-1000 |
  | PRINCIPAL | 14 | 11-9032 |
  | NURSE | 6 | 29-1141 |
  | CNO / CMO-medical | — | 11-9111 |
  | INSTRUCTOR | — | — |

---

## 3. Proposed plan

Each phase is gated by the regression harness (`tests/regression/check-regression.R`) and the
role scorecard (`synthid/dev/09_role_scorecard.R`). Phases 1+ intentionally re-baseline.

### Phase 0: Hygiene + guardrails (no semantic change)
1. Delete the duplicate `COMPTROLLER`. Drop the `NA`/`missing` row. Fix the `CHIEF _X_ OFFICER`
   leak on the standardization tab. Map `"NA"` and `SCHED O` to a placeholder.
2. Add **`data-raw/validate-crosswalk.R`** plus a testthat case that fails on:
   - duplicate `title.standard`
   - a std-tab target with no taxonomy row
   - a value outside the controlled vocab (domain, level)
   - a SOC code not in `tidy-soc-codes.csv` or with an inconsistent major/minor/broad/detail prefix
   - a row that is both emp and board, or neither (except `non-role`)

   Also make `get_googlesheets_title_taxonomy()` stop on duplicate keys.
3. Archive `taxonomy_updated`, `temp`, `temp2` into a dated backup and declare `title-taxonomy` authoritative.

### Phase 1: Context-free correctness fixes
1. ceo flag:
   - Set it on `EXECUTIVE DIRECTOR`, `MANAGING DIRECTOR`, `GENERAL DIRECTOR` and `ORGANIZATION DIRECTOR`.
   - Remove it from `ASSISTANT/DEPUTY/VICE/ASSOCIATE CEO` (→ officer), `PRESIDENT OF MEDICAL STAFF`
     (→ officer/physician) and `POLITICAL DIRECTOR` (→ manager).
2. Move EXECUTIVE ASSISTANT, EXECUTIVE SECRETARY, EDITOR and ORGANIZER out of `mgr` into staff.
3. Apply `synthid/dev/crosswalk_updates_consolidated.csv`, after re-running
   `12_crosswalk_sweep.R` against the full `data-dev/*-unique-titles.rds` corpus:
   - ADD_BOARD and FIX_MISMAP first (cheap, evidence-backed)
   - then ADD_UNMAPPED by frequency
4. SOC repairs:
   - Fix 11-9051 → 11-9151 and the ARTIST hierarchy. Trim whitespace.
   - Blank out the SOC columns on board rows.
   - **Derive `soc.label` by joining on the code** instead of typing it by hand. Move the "?" notes
     into a `notes` column.

### Phase 2: Simplify the taxonomies (the agreed direction)
Replace the 12 boolean columns with two categoricals, then derive the legacy booleans inside
`add_features()` so the existing `num.*` counts and downstream code keep working.

- **`emp.level`** ∈ {`CEO`, `OFFICER`, `MANAGER`, `STAFF`}
  - CEO = top executive (CEO, ED, President-as-exec)
  - OFFICER = C-suite, VP-of-function, deputy/assistant CEO, deputy director
  - MANAGER = director-of-dept, manager
  - STAFF = everything else
  - `dir.vp` is retired using `dirvp_crosswalk_suggestions.csv`.
  - Senior tiers are dropped. Capture SENIOR/ASSOCIATE/ASSISTANT as a modifier flag if wanted.
  - KEY vs STAFF stays a step-09 decision (comp + key-employee box). The SOC major group (29 health,
    23 legal, 25 education…) supplies the "professional" signal, so there's no need for a separate `spec` tier.
- **`board.role`** ∈ {`CHAIR`, `VICE CHAIR`, `SECRETARY`, `TREASURER`, `MEMBER`}
  - Use CHAIR rather than "president" to avoid the PRESIDENT collision.
  - Committee, advisory, ex-officio and at-large move to the status/qualifier flags, not levels.
  - The ~27 long-tail board standards (COUNCILMAN, INCORPORATOR, CAUCUS COUNCILOR, BOARD CHAIR,
    VICE CHAIR…) become **standardization-tab variants** of the 5 core standards rather than taxonomy rows.
  - Compound SECRETARY-TREASURER is handled by the splitter. If it survives unsplit, it maps to SECRETARY plus a `multi` flag.
- Optional **coarse column** `role.coarse.default` (CEO/BOARD/OFFICER/MANAGER/STAFF) as the
  title-only prior that the step-09 cascade overrides.

### Phase 3: Make ambiguity explicit (coordinate with porting the cascade to step 09)
1. Add an **`ambiguous`** column (`role-context`) for PRESIDENT, VICE PRESIDENT, SECRETARY,
   TREASURER, DIRECTOR, CHAIR, OFFICER, WARDEN, MARSHAL. The cascade resolves these from
   checkbox + comp + hours, and the crosswalk value is only the fallback default.
2. Stop the standardization tab from pre-resolving them. `PRESIDENT`, `CHAIR`, `DIRECTOR` and
   `SECRETARY` should standardize to neutral forms instead of BOARD *X*, so the cascade sees the
   real title. **Do this only together with the step-09 port.** Otherwise the 990EZ and no-checkbox
   cases lose their current board default.

### Phase 4: Domain cleanup
1. Use a controlled two-level vocabulary of about 12 labels:
   - `executive` (general management)
   - `governance`
   - `finance`, `administration`, `HR`, `technology`, `fundraising/development`,
     `marketing/communications`, `legal`, `facilities`
   - `programs`
   - industry-specific labels (`health`, `education`, `arts/culture`, `religion`, `social services`, `military/fraternal`)
   - `non-role`
2. Executives go to `executive`, which unblocks domain analysis of staff.
3. **Derive the domain from SOC wherever the SOC is functional** (11-2xxx, 11-3xxx, 11-9xxx,
   major groups 13–29), so the two columns aren't maintained twice. Hand-assign only for uncoded
   titles.

### Phase 5: SOC coverage
1. Fill the top uncoded titles by frequency (the list in 2e).
2. Recode the functional C-suite (2e).
3. Set the policy that **SOC codes function, not rank**. Generic leadership titles (VICE PRESIDENT,
   DEPUTY DIRECTOR) keep 11-1021. Ambiguous titles (DIRECTOR, SECRETARY) stay uncoded until the
   cascade resolves them.
4. Target ≥90% of paid employee rows with a broad-or-detailed code.

### Suggested order
Phase 0 → Phase 1 (biggest accuracy gain for least effort; ED→CEO alone moves hundreds of rows)
→ Phase 2 (schema change; plan one regression re-baseline) → Phase 4/5 (data entry, can run in
parallel) → Phase 3 (bundle with the step-09 cascade port, role-plan P3).

### Decisions needed
- Confirm `emp.level` = 4 levels (CEO/OFFICER/MANAGER/STAFF), or keep a 5th `PROFESSIONAL` tier
  in the crosswalk instead of relying on the SOC major group.
- Confirm renaming board "president" → CHAIR in the output vocabulary.
- Should the simplified columns replace the booleans in the sheet, or be added alongside them for one release?
