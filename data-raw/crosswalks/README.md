# Crosswalk tables

These CSV tables are the source of the package's three crosswalks. Edit them
here, then rebuild the package data from the package root:

```
Rscript data-raw/crosswalks/build-crosswalks.R
```

| Table | Package data | Used by | One row per |
|---|---|---|---|
| `title-standardization.csv` | `title.xwalk` (`data/title-xwalk.rda`) | step 07, `standardize_titles()` | cleaned title variant (`title.variant`, matched to `TitleTxt7`) |
| `title-taxonomy.csv` | `title.taxonomy` (`data/title-taxonomy.rda`) | step 08, `categorize_titles()` | standard title (`title.standard`) |
| `status-codes.csv` | `status.codes` (`data/status-codes.rda`) | step 06, `gen_status_codes()` | status-word variant (`status.variant`) |

Load them with `get_title_xwalk()`, `get_title_taxonomy()` and
`get_status_codes()`. The build keeps only the columns the pipeline uses; the
`notes` columns stay here. `tests/testthat/test-crosswalks.R` fails if `data/`
is out of date with these tables, and checks the tables themselves (one row
per standard, consistent role flags, valid 2018 SOC codes).

## Editing

- Every value is text, including the SOC codes (`11-2033`). Open the tables in
  a text editor or read them as text. **Excel turns SOC codes into dates**, and
  saving from Excel corrupts the file.
- In R, read the tables with `read.csv(colClasses = "character",
  na.strings = character(0))`, or with `read_xwalk()` from `_crosswalk-io.R`.
  Not `fread`: a few variants contain literal quote characters that `fread`
  does not unescape.
- The scripts in `data-raw/partvii-validation/` edit the tables for you and
  log each change: `06-apply-decisions.R` (reviewed crosswalk rows and new
  standards) and `07-taxonomy-fixes.R` (cells of existing taxonomy rows).
  Both rebuild the package data.
- The regression baseline (`tests/regression/`) uses its own frozen copies of
  the crosswalks in `data-raw/demo/`, so editing these tables does not change
  it.

## History

The crosswalks were kept in the title-taxonomy-map Google Sheet until
2026-10-08. `00-export-google-sheet.R` exported every tab on that date to
`archive/google-sheet-2026-10-08/`, unchanged. The archive includes the tabs
the pipeline never read (overview, instructions, abbreviations, notes, to-do,
taxonomy_updated, temp, temp2), and it is not edited.

The three tables started from that export. `title-taxonomy.csv` also includes
the fixes that had not yet been entered in the sheet (FINDINGS.md F-001,
F-012 to F-015). The sheet is no longer read or updated.
