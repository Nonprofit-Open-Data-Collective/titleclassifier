# titleclassifier — Code-Quality Assessment

Findings from a review of the pipeline source (steps 01–08 + helpers). **Every
issue below was reproduced by running the current code** (a handful of
plausible-looking leads that did *not* reproduce are listed at the end so they
aren't re-investigated). No code was changed — this is the "report" half of the
report-then-fix plan.

Important: several of these bugs are baked into the frozen regression reference
(`data-raw/demo/…-REFERENCE.csv`), because that reference captures *current*
behavior. Fixing any of them will change pipeline output, so each fix should be
paired with an intentional, reviewed re-baseline of the regression fixture.

Severity = impact × frequency on real 990 data.

> **Update:** H1–H3 and M1, M3, M4, M5 are **fixed**; **M2 is accepted as-is**
> (see below). The regression reference was re-baselined (current md5
> `65ea3233…`). Net pipeline changes across all fixes: 3 finance officers now
> coded CFO (H1) and 5 spurious empty-title rows dropped (M4), plus downstream
> per-org feature recomputation. M1/M3/M5 are verified and unit-tested but did not
> alter this particular demo's final output. All fixes have testthat cases (30
> tests total). The **Low** items below are now also resolved: L1, L2, L4, L5, L7
> fixed; L3 and L8 left inert-but-documented; L6 unused params removed. Only 3
> more rows changed (L2 spacing: `DIRECTORTHRU` → `DIRECTOR THRU`), no
> `title.standard` changes. Reference re-baselined again (see manifest).

---

## HIGH

### H1. [FIXED] `replace_cfo()` officer-flag comparison is always false → CFO reclassification is dead code
`R/07-standardize-titles.R:117–131`

`standardize_df()` converts the officer checkbox to **numeric 0/1** (via
`to_boole`), but `replace_cfo()` compares it to the **string `"X"`**
(`officer.flag == "X"`). `1 == "X"` is always `FALSE`, so none of the
finance-role → CFO promotions ever fire.

Reproduced:
```
replace_cfo("ACCOUNTANT", 40, 50000, 1)    # -> "ACCOUNTANT"  (pipeline path)
replace_cfo("ACCOUNTANT", 40, 50000, "X")  # -> "CFO"         (code's assumption)
```
Impact in the demo reference: of 13 officer rows whose cleaned title contained
FINANCE/ACCOUNTANT (incl. `FINANCE OFFICER`, `VICE PRESIDENT OF FINANCE`), **0**
were reclassified to CFO. Fix: compare against the numeric flag (`== 1`) — and
note `replace_ceo`/`replace_cfo` should agree on the flag's type.

### H2. [FIXED] `identify_split_num()` mis-splits ordinary titles (bad regex alternation)
`R/04-split-titles.R:127`

```r
x[ grepl("^\\s*SEC[A-Z]*\\s|-*TREAS[A-Z]*\\b$", x) ] <- "SECRETARY & TREASURER"
```
The top-level `|` creates two loosely-anchored alternatives, so any title
*starting* with a SEC-word or *ending* in a TREAS-word is rewritten to
`SECRETARY & TREASURER` and then counted as 2 titles.

Reproduced:
```
identify_split_num("SECURITY DIRECTOR")   # -> 2   (should be 1)
identify_split_num("ASSISTANT TREASURER") # -> 2   (should be 1)
```
Effect: spurious row duplication and wrong titles for security/assistant-treasurer
staff. The single-expression form at `apply_misc_split_rules()` line 91 is the
intended pattern.

### H3. [FIXED] `fix_chair()` standalone-`C` rule clobbers spaced C-suite titles
`R/05-standardize-spelling.R:424` (runs before `condense_abbreviations`, :443–459)

`gsub("\\bC\\b", "CHAIR", …)` fires before the C-suite condenser, so any lone `C`
becomes `CHAIR`.

Reproduced:
```
fix_spelling("C E O")  # -> "CHAIR E O"   (should be "CEO")
fix_spelling("C F O")  # -> "CHAIR F O"   (should be "CFO")
```
Consequence: the `C E O` / `C F O` / `C O O` branches of
`condense_abbreviations` can never match — that block is effectively dead. Fix:
restrict the chair rule (e.g. require a CHAIR-context) or run condensing first.

---

## MEDIUM

### M1. [FIXED] Region substitution matches inside words ending in "R"
`R/02-remove-dates.R:131–132` — `gsub("R-[[:digit:]]{1,2}\\b", "REGION", …)` (and `R\d` variant) are unanchored.
```
remove_date("DIRECTOR-1")    # -> "DIRECTOREGION"
remove_date("SUPERVISOR-2")  # -> "SUPERVISOREGION"
```

### M2. [ACCEPTED — not fixed] Date detection false-positives on non-date numeric tokens
`R/02-remove-dates.R:85–88, 121–124` — `\d+/\d+…` and `\d+-\d+…` are unanchored.
```
has_date("24/7 SUPPORT MANAGER")  # TRUE  -> stripped to "SUPPORT MANAGER"
remove_date("9-5 COORDINATOR")    # -> "COORDINATOR"
```
**Decision: left as-is.** A tighter pattern (valid-month anchor + full m-d-y) was
tried and reverted: it broke real dates that the broad pattern must keep catching
— `mm/yyyy` forms like `12/2023`, and dates glued to text like `EFF02/15/23` —
leaving stray `/` characters in cleaned titles. Distinguishing `24/7` from a real
date by regex without those regressions isn't feasible. The false positive is rare
and low-harm (the core role text survives: `24/7 SUPPORT MANAGER` → `SUPPORT
MANAGER`), so the broad behavior is retained.

### M3. [FIXED] `fix_governor()` boolean logic is impossible → GOV→GOVERNOR never fires
`R/05-standardize-spelling.R:1145–1149` — condition requires the title to contain
`GOVERNMENT` **and** `GOVERNING` simultaneously. Should be "skip if *any* of
GOVERNANCE/GOVERNMENT/GOVERNING present". Reproduced: `fix_spelling("GOV")` → `"GOV"`.

### M4. [FIXED] FOUNDER split rule injects an empty title
`R/04-split-titles.R:96` — `gsub("\\bFOUNDER\\b", "& FOUNDER & ", x)` prepends a
leading `&`; only trailing `&` is stripped.
```
apply_misc_split_rules("FOUNDER")  # -> "& FOUNDER"  -> strsplit on "&" yields c("", "FOUNDER")
```
→ a spurious empty-string title row. (The FOUNDING rule on the next line avoids this.)

### M5. [FIXED] `VICE PRINCIPAL` mis-normalized to `VICE PRESIDENT`
`R/05-standardize-spelling.R:171` — `gsub("\\bVICE\\s*P[A-Z]*\\b", "VICE PRESIDENT", …)`
collapses any VICE + P-word. Reproduced: `fix_spelling("VICE PRINCIPAL")` →
`"VICE PRESIDENT"` (matters for schools).

---

## LOW

- **L1** [FIXED] `convert_ordinal()` unanchored (`R/02:50–59`): ordinals now `\b`-anchored, so `convert_ordinal("21ST")` stays `"21ST"` instead of `"2FIRST"`.
- **L2** [FIXED] Glued `(` removal merged words (`R/02`): the word-glued `(` is now replaced with a space, so `remove_date("CFO(INTERIM)")` → `"CFO INTERIM"`.
- **L3** [ACCEPTED — inert] `fix_double_and()` matches lowercase against upper-case pipeline text, so it never fires (`R/03`). Left inert on purpose: the `title AND word AND word` case is already handled by `standardize_and()`/`fix_misc_splits()`, and activating it double-processes that path. Documented in the source.
- **L4** [FIXED] `standardize_of()` "FOR"→"OF" (`R/03`): now uses a negative lookahead so each non-terminal `FOR` converts independently and only a *trailing* `FOR` is left.
- **L5** [FIXED] `to_helper()` (`R/03`): now scans every parenthetical and replaces `TO`→`UNTIL` only *inside* the parentheses, not globally.
- **L6** [FIXED] `standardize_titles()` (`R/07`): the unused `title`/`hours`/`pay` parameters were removed from the signature.
- **L7** [PARTIAL] The exact duplicate line at `R/05:222` was removed. The other items (the `:117–120` dedup overlap, non-vectorized/unused `spellcheck()`, and the redundant `ifelse(grepl…)` wrappers) are cosmetic and left as-is to avoid churn in the large spelling file.
- **L8** [ACCEPTED — by design] `gen_taxonomy()` (step 10) is an intentional stub for future work; `conditional_logic()` (step 09) is an opt-in step outside the documented 8-step pipeline. No change; noted for context. (`clean_up_ceos()`'s dedup risk is worth revisiting if step 09 is adopted.)

---

## Documentation accuracy (found via the new test suite)

- `README.md` step-05 examples are **stale**: `fix_spelling("EX DIR")` returns
  `"EXECUTIVE DIRECTOR"`, not `"CEO"` (the ED→CEO mapping now happens in the step-07
  crosswalk); and `fix_spelling("DIRECTOR FACILITIES")` is unchanged, not
  `"DIRECTOR OF FACILITIES"` (FACILITIES isn't in the recognized subject list). The
  step-05 vignette has been corrected; the README still needs the same fix.

---

## Leads that did NOT reproduce (checked, cleared — don't re-investigate)

- `comma_helper()` returning `NULL` and misaligning `standardize_comma()` — output was
  correct on the constructed indeterminate inputs (there is an effective catch-all).
- Greedy `PRESIDENTPR.*` data loss — `fix_spelling("PRESIDENTPRE VICE CHAIR")` kept
  "VICE CHAIR".
- `fix_assistant()` `\sASSO\s` gluing words — `fix_spelling("VP ASSO DIRECTOR")` did not
  glue (though the ASSO/DIRECTOR handling here is worth a closer look separately).
- `split_titles()` crashing on `NA` titles — ran without error on an `NA` `TitleTxt3`.

---

## Suggested fix order

1. **H1** (single-line type fix, restores a whole class of CFO coding) and **H2**
   (row-duplication bug) first — highest impact, smallest diffs.
2. **H3** + the dead-code in L7 together (spelling-order cleanup).
3. Medium regex-anchoring fixes (M1, M2, M4, M5) as a batch.
4. Low items opportunistically.

Each fix pairs with an intentional regression re-baseline
(`data-raw/demo/build-demo.R`) and, where natural, a new testthat case pinning the
corrected behavior.
