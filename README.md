# titleclassifier

**Turn the free-text titles on IRS Form 990 Part VII into a structured taxonomy
of nonprofit leadership.**

Part VII lists every officer, director, trustee, key employee and highly paid
person at a nonprofit, each with a title the filer typed: "Pres. & CEO",
"Sec/Treas", "Board Member (thru 6/2023)", "Exec. V.P.". `titleclassifier`
cleans and standardizes those titles, places each in a taxonomy (functional
domain, occupation code, employee level or board role), and resolves who each
person really is in the organization: the chief executive, an officer, a
manager, a professional, staff, or a board seat.

| Filed title | Standard title | Domain | Role |
|---|---|---|---|
| Pres. & CEO | CEO | executive / general management | CEO |
| Sec/Treas | BOARD SECRETARY + BOARD TREASURER | governance / board | BOARD (SECRETARY) |
| Board Member (thru 6/2023) | BOARD MEMBER, flagged partial year | governance / board | BOARD (MEMBER) |
| Exec. V.P. | EXECUTIVE VICE PRESIDENT | executive / general management | OFFICER |
| Dir of Development | DIRECTOR OF DEVELOPMENT | operations / fundraising and membership | MANAGER |
| Staff Physician | DOCTOR | industry-specific / medical | PROFESSIONAL |

The role depends on the filing as well as the title: a PRESIDENT is the CEO
when paid to run the organization and the board chair when not. Roles above
are for a typical filer.

## Installation

```r
# install.packages("pak")
pak::pak("Nonprofit-Open-Data-Collective/titleclassifier")
```

To classify 990-EZ returns with the role model, also install xgboost (optional;
without it the rules decide):

```r
pak::pak("xgboost")
```

## Get a Part VII table

Part VII tables are published in the NCCS efile store, one file per tax year.
The [panel990](https://github.com/Nonprofit-Open-Data-Collective/panel990)
package downloads and reads them:

```r
pak::pak("Nonprofit-Open-Data-Collective/panel990")
library(panel990)

dl   <- download_tables(years = 2022, tables = "F9-P07-T01-COMPENSATION")
pvii <- read_tables(dl)$tables[[1]]
dim(pvii)
#> [1] 5073368      37
```

```r
# the fields titleclassifier uses, for one 990-EZ row and one paid officer on a 990
keep <- c("EIN2", "OBJECTID", "RETURN_TYPE", "TAX_YEAR", grep("^F9_07_COMP_DTK_(TITLE|AVE|POS|COMP|EMPL)", names(pvii), value = TRUE))
i <- c(which(pvii$RETURN_TYPE == "990EZ")[1],
       which(pvii$RETURN_TYPE == "990" & pvii$F9_07_COMP_DTK_POS_OFF_X %in% "X" & pvii$F9_07_COMP_DTK_COMP_ORG > 0)[1])
str(pvii[i, keep])
#> 'data.frame':  2 obs. of  17 variables:
#>  $ EIN2                            : chr  "EIN-01-0015091" "EIN-01-0018922"
#>  $ OBJECTID                        : chr  "OID-202301429349201060" "OID-202510359349300206"
#>  $ RETURN_TYPE                     : chr  "990EZ" "990"
#>  $ TAX_YEAR                        : int  2022 2022
#>  $ F9_07_COMP_DTK_TITLE            : chr  "President" "Secretary"
#>  $ F9_07_COMP_DTK_AVE_HOUR_WEEK    : num  6 37.5
#>  $ F9_07_COMP_DTK_AVE_HOUR_WEEK_RL : num  NA 0
#>  $ F9_07_COMP_DTK_POS_INDIV_TRUST_X: chr  NA NA
#>  $ F9_07_COMP_DTK_POS_INST_TRUST_X : chr  NA NA
#>  $ F9_07_COMP_DTK_POS_OFF_X        : chr  NA "X"
#>  $ F9_07_COMP_DTK_POS_KEY_EMPL_X   : chr  NA NA
#>  $ F9_07_COMP_DTK_POS_HIGH_COMP_X  : chr  NA NA
#>  $ F9_07_COMP_DTK_POS_FORMER_X     : chr  NA NA
#>  $ F9_07_COMP_DTK_COMP_ORG         : num  0 39301
#>  $ F9_07_COMP_DTK_COMP_RLTD        : num  NA 0
#>  $ F9_07_COMP_DTK_COMP_OTH         : num  0 0
#>  $ F9_07_COMP_DTK_EMPL_BEN         : num  0 NA
```

Each row is one person on one filing (`OBJECTID`); the 2022 table has 5.07
million rows from 555,197 filings. The fields are the title, hours at the
organization and at related organizations (`_RL`), the six position
checkboxes (trustee, institutional trustee, officer, key employee, highly
compensated, former), and four pay components: from the organization, from
related organizations, other compensation and benefits. A 990-EZ has no
checkboxes. Names and organization fields are also in the table.

`titleclassifier` also ships a demo slice in the same schema, `tinypartvii`,
and `get_partvii()` reads a year directly from the store.

## Classify the titles

The pipeline is nine steps, each a single function:

```r
library(titleclassifier)

df <- pvii |>
  standardize_df() |>         # 01 clean the table: titles, numeric pay and hours, 0/1 boxes
  remove_dates() |>           # 02 strip and flag dates ("thru 6/2023")
  standardize_conj() |>       # 03 one separator between titles ("Sec/Treas" -> SEC & TREAS)
  split_titles() |>           # 04 one row per title
  standardize_spelling() |>   # 05 expand abbreviations ("Exec. V.P." -> EXECUTIVE VICE PRESIDENT)
  gen_status_codes() |>       # 06 flag and remove FORMER, INTERIM, EX OFFICIO, ...
  standardize_titles() |>     # 07 map to a standard title (crosswalk, then pattern rules)
  categorize_titles() |>      # 08 attach the taxonomy and per-filing features
  conditional_logic()         # 09 resolve each person's role
```

The result has one row per person and title, with the title at every step,
`title.standard`, the taxonomy fields, status flags, pay and hours features,
and the resolved role (`role.final`, `role.board`, `role.ceo`).
`role.primary` marks one row per person.

## Learn more

- [Workflow](https://nonprofit-open-data-collective.github.io/titleclassifier/articles/workflow.html):
  an end-to-end run.
- [Pipeline steps](https://nonprofit-open-data-collective.github.io/titleclassifier/articles/step-01-standardize-df.html):
  one article per step, with before-and-after examples and the crosswalks
  each step uses.
- [Rules and models](https://nonprofit-open-data-collective.github.io/titleclassifier/articles/classifiers.html):
  how roles are decided, and when the model is used.
- [Taxonomy](https://nonprofit-open-data-collective.github.io/titleclassifier/articles/taxonomy.html):
  domains, roles, employee levels, board roles, strata and status flags.
- [Performance](https://nonprofit-open-data-collective.github.io/titleclassifier/articles/performance.html):
  coverage and accuracy against hand labels.
