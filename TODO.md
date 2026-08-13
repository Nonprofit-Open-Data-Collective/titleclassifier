# titleclassifier — TODO / maintenance notes

## Export surface: keep-public vs. make-internal (deferred)

Right now **every function is exported** — intentional for the beta, since being able to
call and test individual helpers aids debugging and refinement. Before a 1.0 release we
should shrink the public API so `?titleclassifier` and the pkgdown reference show only what
users need. Suggested split below. To demote a function: remove its `@export` tag (add
`@keywords internal` and `@noRd` if we also want to drop the `.Rd`), then re-run
`devtools::document()`. No behavior change — internal functions still work package-wide.

### Keep public (user-facing)

The pipeline verbs, the two trailing steps, and the fetch helpers:

- `standardize_df`, `remove_dates`, `standardize_conj`, `split_titles`,
  `standardize_spelling`, `gen_status_codes`, `standardize_titles`, `categorize_titles`
- `conditional_logic` (step 09), `gen_taxonomy` (step 10)
- `fetch_partvii`, `read_partvii`, `get_partvii`

### Demote to internal (candidates)

Grouped by the step they support:

- **Conjunction (03):** `standardize_and`, `standardize_to`, `standardize_of`,
  `standardize_comma`, `standardize_slash`, `standardize_separator`, `and_helper`,
  `amp_helper`, `to_helper`, `of_title_helper`, `slash_helper`, `comma_helper`,
  `fix_double_and`, `fix_misc_splits`, `apply_misc_split_rules`,
  `remove_trailing_conjunctions`
- **Dates (02):** `convert_ordinal`, `has_date`, `remove_date`
- **Split (04):** `identify_split_num`, `remove_first_split`
- **Spelling (05):** the whole `fix_*` family (~50: `fix_academics` … `fix_vice`),
  `condense_abbreviations`, `simplify_clevels`, `spellcheck`, `fix_of`, `fix_spelling`
- **Status (06):** `flag_and_keep`, `flag_and_remove`, `get_variants`, `add_status_flag`,
  `remove_status`, `standardize_status`
- **Standardize/Categorize (07/08):** `basic_csuite_fixes`, `replace_ceo`, `replace_cfo`,
  `add_features`, `simplify_varnames`, `director_correction`, `clean_up_ceos`
- **Utility/reporting:** `pre_clean`, `get_title_dump`, `gen_helpful_tables`

### Move out of the package (into tests)

The `test_*` helpers are ad-hoc harnesses, not package API — fold them into the testthat
suite (see below) and drop the exports:
`test_fix_spelling`, `test_remove_dates`, `test_standardize_and`, `test_standardize_comma`,
`test_standardize_of`, `test_standardize_separator`, `test_standardize_slash`,
`test_standardize_to`.

## Other follow-ups

- **LICENSE copyright holder** — currently "Nonprofit Open Data Collective"; confirm/adjust.
- **Non-ASCII in source** — the `✔`/`✘` glyphs in `cat()` status messages trip an R CMD
  check WARNING; replace with ASCII or escape as `✔` if we want a clean check.
- **Regression baseline** — re-run `tests/regression/check-regression.R` after any pipeline
  change; re-baseline (`data-raw/demo/build-demo.R`) only for intentional output changes.
