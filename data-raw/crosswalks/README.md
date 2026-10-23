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
| `ceo-thesaurus.csv` | `ceo.thesaurus` (`data/ceo-thesaurus.rda`) | step 09, `resolve_roles()` | chief-executive title pattern for one organization type (`org_type`, see `org_type_levels()`) |

`ceo-thesaurus.csv` (FINDINGS.md F-033) lists, by organization type, titles
whose holder runs the organization (`effect` "ceo": an adjutant, a club's
general manager, a union's business manager) or never does on stipend-level
pay (`effect` "not_ceo": a lodge secretary). `title_pattern` and `outranks`
are Perl regular expressions on the cleaned title; `min_pay` and `min_hours`
are the thresholds (for "not_ceo", `min_hours` is the hours at which a holder
is no longer on a stipend). `evidence` cites the labeled filings behind each
row. The org type comes from an `org.type` column, or BMF NTEE codes and
501(c) subsections (`ntee_code_clean`, `subsection_code`) joined to the input,
or the name and titles of the filing, in that order (`org_types()`).

`title-taxonomy.csv` gives each standard title an `emp.level` (CEO, OFFICER,
MANAGER, PROFESSIONAL, STAFF) or a `board.role` (CHAIR, VICE CHAIR, SECRETARY,
SECRETARY TREASURER, TREASURER, MEMBER). Board titles use only the six core
standards (BOARD PRESIDENT, BOARD VICE PRESIDENT, BOARD SECRETARY, BOARD
SECRETARY TREASURER, BOARD TREASURER, BOARD MEMBER): add other board titles as
standardization variants of these. The combined secretary-treasurer office is
kept whole (FINDINGS.md F-032); its legacy flags set both `sec` and `treas`.
`build-crosswalks.R` derives the legacy flags (`emp`, `ceo`, ... `mem`) for
the package.

The taxonomy's other columns follow two rules:
- **Domain:** `domain.category` / `domain.label` must be a pair listed in
  `domains.csv`, the controlled vocabulary.
  - `executive / general management` holds the CEO and general-management
    officers.
  - `governance / board` holds every board title.
- **SOC:** a SOC code describes a title's function, not its rank. Leave
  ambiguous titles uncoded.

One-time passes over the taxonomy, each logging its changes:

| Script | What it did | Log |
|---|---|---|
| `01-role-levels.R` | flags to `emp.level` / `board.role` | `role-levels-log.csv`, `board-collapse-log.csv` |
| `02-soc-codes.R` | SOC codes by function | `soc-codes-log.csv` |
| `03-domains.R` | the domain vocabulary | `domains-log.csv` |
| `04-taxonomy-orphans.R` | removed taxonomy rows no variant or pattern maps to | `taxonomy-orphans-log.csv` |

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
