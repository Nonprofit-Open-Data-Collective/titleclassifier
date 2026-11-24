# titleclassifier (development version)

## Baseline performance

This is the first entry in the changelog. It records where the package stands
so that later changes can be measured against it. All numbers come from the
validation scripts in `data-raw/partvii-validation/`, run on the NCCS efile
tables (release `efile_v2_3`), and are reported in full in the Performance
article (`vignettes/articles/performance.Rmd`).

* **Coverage.** On the 2019-2023 calibration sample (60,520 filings, 699,276
  title rows), weighted to the population, 96.1% of title rows reach a
  standard title (95.3% by exact crosswalk match, 0.8% by pattern rule) and
  3.7% remain uncoded. Every person still gets a role, because step 09
  resolves uncoded titles from the checkboxes, pay and hours.
* **Accuracy on the hand-labeled set** (312 people on 2010-2012 filings):
  90.4% get the right role (990: 90.1%; 990-EZ: 92.5%). Recall by labeled
  role: CEO 100%, DUAL 100%, PROFESSIONAL 95.0%, BOARD 94.0%, OFFICER 70.8%,
  MANAGER 64.3%, STAFF 58.3%. Unpaid people get the right role 97.7% of the
  time and paid people 87.5%. Of the board members identified correctly, 98.2%
  also get the right board role.
* **990-EZ returns** (no checkboxes, so step 09 uses a model). On held-out form
  switchers with the same title in both years (10,558 people), the model
  agrees with the full-990-year role 96.7% of the time (rules: 95.1%), and
  74.0% for people not on the board (rules: 63.6%).
* **Chief executive.** On the CEO label set (750 filings from 150
  organizations), step 09 names the right chief executive for 88.1% of
  filings and gives board seats the right board role 98.7% of the time. On
  the held-out set (126 filings from 64 other organizations) it names the
  right chief executive for 81.7%.
* **Stability over time.** For the 6,000 organizations followed from 2019 to
  2023, 95-98% of people keep their role from one year to the next (board
  members: 99.2%), and in most groups 88-92% of organizations keep the same
  CEO.

## Name parsing for the validation scripts: peopleparser 0.2.4

Name parsing is not part of the package pipeline. Only the scripts that link
people across years, `15-panel-synthid.R` and `18-switch-labels.R`, parse
names, using peopleparser before `synthid::link_panel()`.

* peopleparser is now a declared dependency: `peopleparser (>= 0.2.4)` in
  `Suggests`, installed from the `v0.2.4` tag through `Remotes`. Both scripts
  stop with the install command if an older version is installed.
* Both scripts call `parse_names(add_nicknames = TRUE)` and carry the new
  first-name variant columns (`first_name_type`, `first_name_formal`,
  `first_name_nicknames`, `first_name_canonical`, `canonical_rule`) into
  their linked output. synthid already matches nicknames using the same
  variant table, so the links themselves do not change.
* Since peopleparser 0.2.0, `parse_names()` no longer starts its own parallel
  workers. Both scripts now pass `workers = NW`, so parsing still uses the
  script's worker count (all cores but one by default).
* The scripts use none of the options added in 0.2.1 (`classify`,
  `split_run_together`), so their calls are unchanged from 0.2.0.
* Rerunning either script with peopleparser 0.2.4 can change some parses,
  and so some links, compared with earlier outputs:
  * 0.1.0 moved to the 2020 Census name tables and strips more credentials
    (253 of 9,608 Part VII names changed first or last name).
  * 0.2.0 keeps two-word names in their written order (20 more changed).
  * 0.2.2 does the same for three-word names and joins a middle name to the
    surname only when it is a strong surname (`JOHN LEE SMITH` keeps `LEE`).
  * 0.2.3 and 0.2.4 strip trailing credentials that used to become the
    surname (`ANDREA M GORDON NMF`, `CAROL RUSSELL NP`).

  Against the parses stored in synthid's Part VII people tables, peopleparser
  reports that 9% of 10,000 names parse differently, most of them gender
  codes or a corrected name order. The baseline numbers above were produced
  before this update.
