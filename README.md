# titleclassifier

An R package to assign raw nonprofit executive titles from Form 990 Part VII to a well-structured title taxonomy. 


## Installation

```r
devtools::install_github( 'nonprofit-open-data-collective/titleclassifier' )
```

If you get the following error message: 

```
Error in utils::download.file( ...
: download from 'https://api.github.com/repos/nonprofit-open-data-collective/titleclassifier/tarball/HEAD' failed
```

Try changing the default file download options before installing: 

```r
options( download.file.method = "wininet" )    # for windows
options( download.file.method = "libcurl" )    # for all op systems
devtools::install_github( 'nonprofit-open-data-collective/titleclassifier' )
```

## Use

The package works on 990 Part VII compensation tables (F9-P07-T01-COMPENSATION) from [IRS 990 EFILE DATA](https://nccs.urban.org/nccs/catalogs/catalog-efile-v2.html).

- **STEP-01:** standardizes the raw Part VII dataframe into a clean, numeric-friendly format.
- **STEP-02:** removes date fragments and flags them.
- **STEP-03:** normalizes conjunctions, prepositions, and separators.
- **STEP-04:** splits multi-title strings into individual rows.
- **STEP-05:** harmonizes spelling, abbreviations, and short forms.
- **STEP-06:** detects, flags, and cleans status qualifiers (FORMER, INTERIM, etc.).
- **STEP-07:** merges with a crosswalk and applies CEO/CFO fixes.
- **STEP-08:** assigns taxonomy categories, engineers final features, and produces analysis-ready datasets.


```r
library( titleclassifier )
library( dplyr )

url <- "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_0/"
fn  <- "F9-P07-T01-COMPENSATION-2023.CSV"
d   <- read.csv( paste0(url,fn) )

df <- d %>% 
  standardize_df() %>%           # step 01
  remove_dates() %>%             # step 02
  standardize_conj() %>%         # step 03
  split_titles() %>%             # step 04
  standardize_spelling() %>%     # step 05
  gen_status_codes() %>%         # step 06
  standardize_titles() %>%       # step 07
  categorize_titles()            # step 08
```


## Process

The sections below provide a workflow summary of the Title Classifier process:

1. **Standardize the raw data:** Start by cleaning the Part VII table: rename fields, fix titles, convert compensation and hours into numeric values, and ensure checkboxes are consistent.

2. **Remove dates from titles:** Strip out start/end dates, year ranges, and calendar references that sometimes appear in titles. Create a flag so we know which titles contained dates.

3. **Normalize conjunctions and separators:** Clean up words like AND, TO, OF, commas, slashes, and ampersands. Make sure they are used consistently so later splitting won’t misinterpret them.

4. **Split multi-title strings:** Break apart rows where a single individual’s Title field contains multiple roles (e.g., “President & Treasurer”). Each role gets its own row.

5. **Harmonize spelling and abbreviations:** Standardize variations such as “VP”, “Vice Pres.”, “V.P.” → “VICE PRESIDENT”. Apply a large dictionary of fixes for common nonprofit role terms.

6. **Detect and flag status qualifiers:** Identify titles with qualifiers like FORMER, INTERIM, FOUNDER, EX OFFICIO. Remove or standardize them, and add parallel indicator flags.

7. **Standardize titles to canonical forms:** Use a curated crosswalk to map raw titles into a controlled vocabulary. Apply logic to resolve tricky executive roles (e.g., when PRESIDENT = CEO).

8. **Categorize roles and add features:** Assign each standardized title into categories (CEO, C-level, Board, Manager, Specialist, etc.). Compute organizational summaries like counts of leaders, pay ranks, and hours distributions.

In short: the workflow takes raw nonprofit officer/employee titles from IRS Form 990 data, cleans them step by step, and produces an analysis-ready dataset where each role is standardized, categorized, and enriched with organizational metrics.

### Details

#### 1.	Standardize Input Data

```
STEP-01: standardize_df()
|
+- check_names(df)
|    - Ensures the expected compensation and checkbox columns exist in the input dataframe.  
|    - If any are missing, adds them with NA values so later processing won’t break.  
|
+- pre_clean(x)           ← used on title field
|    - Standardizes raw titles: converts to uppercase, strips periods and extra characters, 
|      creating a clean version ready for later parsing and matching.  
|
+- to_numeric(x)          ← used on hours and compensation fields
|    - Converts messy character/numeric fields into numeric form.  
|    - Removes non-digit characters, coerces blanks to 0, and ensures numeric type.  
|    - Applied to: x_hours, x_hours_rltd, x_comp_base, x_benefits, x_related, x_other.  
|
+- to_boole(x, form.type)  ← used on several checkbox columns
|    - Standardizes yes/no checkbox fields into 1 (TRUE) or 0 (FALSE).  
|    - Handles cases like “X”, “YES”, “NO”, blanks.  
|    - Special case: if form.type == "990EZ", sets values to NA since those checkboxes 
|      aren’t collected on that form.  
|    - Applied to: x_trustee_ind, trustee_inst, officer, key.employee, 
|      high.comp.ind, former.  
```

#### 2. Remove Dates

```
STEP-02: remove_dates()
|
+- convert_ordinal(TitleTxt)
|    - Converts numeric ordinals (1ST, 2ND, 3RD, …) into their spelled-out
|      word equivalents (FIRST, SECOND, THIRD, …).
|    - Ensures ordinals in titles are standardized before date detection.  
|
+- has_date(TitleTxt)
|    - Detects whether a string contains a date.  
|    - Looks for patterns like: 
|        * 'YY (e.g., '19)
|        * YY-YY (e.g., 17-18)
|        * mm/dd/yyyy or mm-dd-yyyy
|        * Full/abbreviated month names (JAN, FEBRUARY, etc.)
|    - Returns TRUE/FALSE flag.  
|
+- remove_date(TitleTxt)
|    - Strips dates and related fragments from raw title text.  
|    - Handles multiple formats: 'YY, YY-YY, mm/dd/yyyy, mm-dd-yyyy,
|      explicit month words.  
|    - Also removes trailing numbers, stray parentheses, and cleans up
|      whitespace.  
|    - Special substitution: “R-1”, “R12”, etc. → “REGION”.  
|
+- remove_dates(df, title = "F9_07_COMP_DTK_TITLE")
|    - Main wrapper applied to a dataframe column.  
|    - Steps:
|        1. Applies convert_ordinal() to normalize ordinals.
|        2. Creates DATE.X flag = 1 if has_date() detects a date, else 0.
|        3. Creates cleaned title field (TitleTxt2) by applying remove_date().  
|    - Returns the dataframe with new fields.  
```


```r
# example date cleaning

input <- "TREASURER BEGINNING MAY 30 2018"
isDatePresent <- identify_dates(input)  # flag for date present
if(isDatePresent) {
  output <- remove_date(input)
  print(output)
  # "TREASURER BEGINNING"
}

# the remove_dates wrapper function
# applies this vectorized function
# process over all titles in the data
```


#### 3. Standardize Conjunctions

Sometimes titles can include conjunctions, prepositions, and punctuation (like "and", "to", ",", "/"), but these symbols don't always refer to the same thing. For example, a title field could be "CEO AND BOARD PRESIDENT", and another could be "VP OF FINANCE AND ADMINISTRATION". In the first instance, the "and" serves to separate two different titles, but in the second it's part of a compound subject. There are many more cases like these with such symbols, so in this step, they are standardized. It is also important to note that we are operating on each individual title at this stage (as opposed to working with the entire compensation table).

```
STEP-03: standardize_conj()
|
+- standardize_and(TitleTxt)
|    - Detects whether “AND” or “&” is acting as a title separator.  
|    - Converts “AND” to “&” when both sides look like valid titles.  
|    - Converts lone “&” back to “AND” if not acting as a separator.  
|    - Also calls fix_double_and() and fix_misc_splits() to clean tricky cases.  
|
|    +- and_helper(x)
|    |    - Checks if “AND” splits a string into valid titles.  
|    |    - Returns TRUE only if both/all sides match likely titles.  
|    |
|    +- amp_helper(x)
|         - Checks if “&” splits into valid titles.  
|         - Returns TRUE if both/all sides are recognized titles.  
|
|    +- fix_double_and(x)
|         - Handles patterns like “title AND word AND word”  
|           → substitutes first AND with “&” to avoid double-splits.  
|
+- standardize_to(TitleTxt)
|    - Detects “TO” usage, mainly in dates (already flagged in Step-02).  
|    - If “TO” is inside parentheses, calls to_helper() to swap with “UNTIL”.  
|    - Also replaces trailing “TO” at end of a title with “UNTIL”.  
|
|    +- to_helper(x)
|         - Checks parentheses text for “TO” and replaces with “UNTIL”.  
|
+- standardize_of(TitleTxt)
|    - Cleans up “OF” usage.  
|    - Replaces “AS OF” → “SINCE”.  
|    - Drops dangling “OF” at string ends.  
|    - Converts “FOR” → “OF” in some contexts.  
|    - Cleans up cases like “VP-” → “VP,”.  
|
+- standardize_comma(TitleTxt)
|    - Standardizes comma usage.  
|    - Distinguishes commas used as separators vs “of” vs redundant.  
|    - Ensures commas are followed by spaces.  
|    - Calls helpers to detect state abbreviations and replace with REGION.  
|
|    +- check_if_state(x)
|    |    - Detects U.S. state abbreviations at string ends.  
|    |    - Replaces with “OF REGION”.  
|    |
|    +- replace_w_of_except(x)
|         - Handles common cases like “VP, Sales” → “VP of Sales”.  
|
+- standardize_slash(TitleTxt)
|    - Not fully shown in this snippet but intended to standardize “/” usage.  
|    - Likely decides whether “/” means separator (“&”) or part of a compound.  
|
+- standardize_separator(TitleTxt)
|    - Cleans up other generic separators (not fully visible in snippet).  
|
+- txt_cleanup(TitleTxt)
|    - Final generic text cleanup: trims extra spaces, fixes artifacts.  
```

With the **standardize_and** function, the method checks the substrings on each side of the "and", and if there is a recognizable title on each side, then the "and" is converted into "&", which is our delineator for fields with multiple titles. Otherwise, the "and" is left alone. 

```r
# "and" examples
title1 <- "CEO AND BOARD PRESIDENT"
title2 <- "VP OF FINANCE AND ADMINISTRATION"

standardize_and(title1) # "CEO & BOARD PRESIDENT"
standardize_and(title2) # "VP OF FINANCE AND ADMINISTRATION"
```

With the **standardize_to()** function, the method checks if "to" is at the end of the title (in which case it was part of a date description) in which case it converts the "to" to "until". If the "to" is not at the end of the title, the method then checks if the substring on the left is a recognizable title, in which case it leaves it alone. In all other cases also the "to" is left alone. 


```r
# "to" examples
title1 <- "DIRECTOR TO"
title2 <- "LIAISON TO BOARD OF TRUSTEES"

standardize_to(title1) # "DIRECTOR UNTIL"
standardize_to(title2) # "LIAISON TO BOARD OF TRUSTEES"
```

With the **standardize_of()** function, the method checks if "of" is part of an "as of" phrasing, in which case it converts "as of" to "since". If that is not the case, yet "of" is at the end of the title, the "of" is removed. Otherwise, the "of" is treated the same way as "to" by checking the left side substring. Additionally, all instances of "for" are replaced with "of", and "VP-" is replaced with "VP OF"


```r
# "of" examples
title1 <- "VICE PRESIDENT AS OF"
title2 <- "DIRECTOR OF"
title3 <- "VICE PRESIDENT FOR MARKETING"
title3 <- "VP-ADMINISTRATION"

standardize_of(title1) # "VICE PRESIDENT SINCE"
standardize_of(title2) # "DIRECTOR "
standardize_of(title3) # "VICE PRESIDENT OF MARKETING"
standardize_of(title4) # "VICE PRESIDENT OF ADMINISTRATION"
```

With the **standardize_comma()** function, the method checks for the different meanings that the comma can represent. If a comma is used as a title delineator, it is replaced with "&" (same procedure as standardize_and). If it is used in place of "of" (same procedure as standardize_to), the comma is replaced with "of". For titles with multiple commas, this only applies to the first instance of the symbol. Any extraneous commas are replaced with "and". 

```r
# comma examples
title1 <- "SECRETARY, TREASURER"
title2 <- "VP, MARKETING"
title3 <- "SENIOR VICE PRESIDENT, MARKETING, SALES, AND PUBLICITY"
title4 <- "FINANCE, ADMINISTRATION"

standardize_comma(title1) # "SECRETARY & TREASURER"
standardize_comma(title2) # "VP OF MARKETING"
standardize_comma(title3) # "SENIOR VICE PRESIDENT OF MARKETING, SALES, AND PUBLICITY"
standardize_comma(title4) # "FINANCE AND ADMINISTRATION"
```

With the **standardize_slash()** function, the method checks for the different meanings that the slash can represent. The slash is treated the same way as the comma; thus, when it is used as a delineator it is replaced with "&", when used like "of" it is replaced with "of", and when used extraneously it is replaced with "and". 

```r
# slash examples
title1 <- "SECRETARY/TREASURER"
title2 <- "VP/SALES"
title3 <- "SENIOR VICE PRESIDENT OF FINANCE/ADMINISTRATION"

standardize_slash(title1) # "SECRETARY & TREASURER"
standardize_slash(title2) # "VP OF MARKETING"
standardize_slash(title3) # "SENIOR VICE PRESIDENT OF FINANCE AND ADMINISTRATION"
```

Finally, the **standardize_separator()** function standardizes all other miscellaneous symbols that are used as a separator. As previously mentioned, the "&" is the main symbol used to delineate titles, but ";" and "\" can also be used. As such, those symbols are replaced by "&" to make title splitting more straightforward. The other common title separators ("," and "/") have already been dealt with. 

```r
# separator examples
title1 <- "CEO & CFO"
title2 <- "DIRECTOR; CHAIR"
title3 <- "CHAPLAIN\TRUSTEE"

standardize_separator(title1) # "CEO & CFO"
standardize_separator(title2) # "DIRECTOR& CHAIR"
standardize_separator(title3) # "CHAPLAIN&TRUSTEE"
```

It goes without saying that there are and will be instances of false positives, but using these techniques, the titles are generally in good shape for splitting and categorization in later steps. Browsing through [lists of titles with these symbols apparent (and their standardizations)](https://github.com/Nonprofit-Open-Data-Collective/titleclassifier/tree/main/test-tables/title-comparison/2022-07-19) can be helpful for coming up with more complicated yet accurate heuristics. 

You may also notice that spacing can sometimes look inconsistent. That is true, but since the goal of this step is only to standardize conjunction/transition symbols, weird spacing is not dealt with here. In later steps, the excess spacing, or lackthereof, is dealt with. 

As a note for code consistency, `standardize_and` is actually applied twice after its initial application (after `standardize_slash` and `standardize_separator`). The purpose of this is to catch any conjunction issues that may have arised as a result of the other standardizations. The "and" standardization is ultimately most important as "&" is the main separator, and thus we want it to be as refined as possible. 

Additionally, as with the `remove_dates` wrapper function, the major aspects of standardizing conjunctions in this step are vectorized for efficiency.

#### 4. Split Multi-Titles

Step 04 is the engine that expands multi-role strings into multiple rows, using helper functions for edge cases, counting, and iterative removal. After this, each row represents a single, normalized title.

For instance, a row with the title field "SECRETARY & TREASURER" would be duplicated into two rows - one with title "SECRETARY" and the other with "TREASURER". Having standardized all of our separators, we theoretically need only split on "&". However, sometimes there are titles that are mingled together, as an error of data entry. For example "SECRETARYTREASURER" as one word would not be split into two titles when it should be. Previously, these were dealt with an unmingle() function, but for efficiency issues, this has since been removed. 


```
STEP-04: split_titles()
|
+- apply_misc_split_rules(x)
|    - Applies a set of regex substitutions to catch tricky edge cases.  
|    - Examples:
|        * “CEO TRUSTEE” → “CEO & TRUSTEE”  
|        * “DIRECTOR AND AD HOC” → “DIRECTOR & AD HOC”  
|        * “FOUNDER” → “& FOUNDER &” (forces splits on founder roles)  
|    - Cleans lingering digits and stray trailing ampersands.  
|
+- identify_split_num(x)
|    - Estimates how many distinct titles a string contains.  
|    - Counts “&” separators after cleaning known patterns.  
|    - Special handling:
|        * “EX DIR” variants → “EXECUTIVE DIRECTOR”  
|        * “SECRETARY TREASURER” → “SECRETARY & TREASURER”  
|    - Returns integer count of titles.  
|
+- remove_first_split(x)
|    - Drops everything before the first “&” in a string.  
|    - Used to iteratively peel off titles one by one.  
|
+- split_titles(df, title = "TitleTxt3")
|    - Main wrapper. Steps:
|        1. Pulls the target title column.  
|        2. Applies apply_misc_split_rules() to normalize edge cases.  
|        3. Splits titles into lists on “&”.  
|        4. Duplicates rows of df for people with multiple titles.  
|        5. Assigns each title back into new rows (TitleTxt4).  
|        6. Adds Num.Titles field = within-person title index.  
|    - Returns expanded dataframe with one row per title.  
```

**Examples:**

```r
row.title  <- "CFO & TREASURER & DIRECTOR"
num.titles <- identify_split_num( row.title ) # identifies 3 titles

# in split_titles, if num.titles > 1,
# the row is duplicated and all text after "&" is removed;
# thus in the original row we have the title as "CFO "

row.title  <- remove_first_split(row.title)   # row.title is now " TREASURER & DIRECTOR"
num.titles <- identify_split_num(row.title)   # will now be 2

# since num.titles is still > 1, the row is duplicated again

row.title  <- remove_first_split(row.title)   # row.title is now " DIRECTOR"
num.titles <- identify_split_num(row.title)   # now 1
```

The one row with title "CFO & TREASURER & DIRECTOR" has been converted into three separate rows in the data frame with the titles "CFO", "TREASURER", and "DIRECTOR" respectively. All other information in the rows is duplicated. 

#### 5. Standardize Spelling

This step is the dictionary-driven clean-up layer. It consolidates spelling and abbreviation variants into canonical forms, handles repeated tokens, abbreviates C-levels, and prepares strings for final coding.

All abbreviations are substituted with their full title versions. The main `fix_spelling` function is a wrapper for many smaller `fix_x` functions. In the cases where we find "...VICE PRESIDENT *SUBJECT*" or "...DIRECTOR *SUBJECT*", we substitute an "of" between the main title and the subject. This also applies to "CHAIR", "DEAN", "MANAGER" as the main title. In total, there are nearly 50 `fix_x` subroutines.

```
STEP-05: standardize_spelling()
|
+- fix_spelling(TitleTxt)
|    - Main helper applied to each title string.  
|    - Calls ~40 specialized fix_* functions, each targeting a family of words 
|      (e.g., VICE, EXECUTIVE, DIRECTOR, OPERATIONS, PRESIDENT, TREASURER, etc.).  
|    - Also does final cleanup:
|        * Deduplicates repeated titles (e.g., “PRESIDENT PRESIDENT” → “PRESIDENT”)  
|        * Removes trailing conjunctions  
|        * Abbreviates C-level titles (“CHIEF EXECUTIVE OFFICER” → “CEO”)  
|        * Strips extra whitespace  
|
|    +- fix_vice()
|    |    - Normalizes all “Vice President” variants (VP, V.P., EVP, SVP, etc.).  
|    |
|    +- fix_executive()
|    |    - Standardizes “EXEC” prefixes (EXE, EXDIR, EXECDIR, etc.) → EXECUTIVE.  
|    |
|    +- fix_director()
|    |    - Expands abbreviations and smushed forms into “DIRECTOR”.  
|    |
|    +- fix_operations()
|    |    - Standardizes OPERATIONS / OPERATING.  
|    |
|    +- fix_assistant()
|    |    - Distinguishes ASSISTANT vs ASSOCIATE.  
|    |
|    +- fix_president()
|    |    - Standardizes PRESIDENT forms.  
|    |
|    +- fix_secretary()
|    |    - Standardizes SECRETARY forms.  
|    |
|    +- fix_treasurer()
|    |    - Standardizes TREASURER forms.  
|    |
|    +- fix_finance()
|    |    - Standardizes FINANCE vs FINANCIAL.  
|    |
|    +- fix_senior()
|    |    - Standardizes SENIOR vs JUNIOR.  
|    |
|    +- fix_development(), fix_chair(), fix_officer(), fix_admin(), fix_coordinator() …  
|    |    - Each function targets a specific term family, ensuring consistency.  
|    |
|    +- fix_strategy(), fix_hr(), fix_manage(), fix_programs(), fix_projects() …  
|    |    - Normalize common department/role keywords.  
|    |
|    +- fix_public(), fix_business(), fix_comm(), fix_information(), fix_technology() …  
|    |    - Handle communication, information, technology, institutional forms.  
|    |
|    +- fix_marketing(), fix_advancement(), fix_philanthropy(), fix_systems(), fix_general() …  
|    |    - Normalize nonprofit-specific or administrative vocabulary.  
|    |
|    +- fix_planning(), fix_compliance(), fix_enrollment(), fix_admissions(), fix_deputy(), etc.  
|    |    - Handle niche or role-specific words.  
|    |
|    +- fix_miscellaneous(), condense_abbreviations()  
|    |    - Catch-all cleanup (removes duplicates, short-hands).  
|    |
|    +- remove_trailing_conjunctions(), simplify_clevels(), fix_of()  
|         - Final polish: trims conjunctions, shortens C-suite terms, cleans “OF” usage.  
|
+- standardize_spelling(df, title = "TitleTxt4")
     - Wrapper function at dataframe level.  
     - Applies fix_spelling() to each row.  
     - Stores results in new column TitleTxt5.  
     - Prints confirmation message.  
```

```r
#examples

fix_spelling("VP") # "VICE PRESIDENT"
fix_spelling("DIRECTOR FACILITIES") # "DIRECTOR OF FACILITIES"
fix_spelling("SR D MARKETING") # "SENIOR DIRECTOR OF MARKETING"
fix_spelling("VICE PRESID") # "VICE PRESIDENT"
fix_spelling("SGT AT ARMS") # "SERGEANT AT ARMS"
fix_spelling("TRTEE") # "TRUSTEE"
fix_spelling("SECY") # "SECRETARY"
```

Some title standardization was also completed at this step, and namely any version of "EXECUTIVE DIRECTOR" was mapped onto "CEO". In addition, unambiguous c-suite positions were condensed to their abbreviations. Not all c-suite positions fall into this category however.

```r
fix_spelling("EX DIR") # "CEO"
fix_spelling("CHIEF EXEC OFFICER") # "CEO"
fix_spelling("CHIEF FINANCIAL OFFICER") # "CFO"
fix_spelling("CHIEF ADVANCEMENT OFFICER") # "CHIEF ADVANCEMENT OFFICER"
```


There are many cases of ambiguity especially with abbreviations, so sometimes a heuristic approach is taken. For example, a singular "T" is substituted for "TRUSTEE", even though it could represent "TREASURER" as well. Or, as another example, "COMM" is converted to "COMMUNICATIONS" if in the middle of a title, but converted to "COMMITTEE" if at the end. A final example would be with "ADMIN" which can refer to "ADMINISTRATOR", "ADMINISTRATIVE", or "ADMINISTRATION". There are nearly a dozen of these cases, and the specific design choices are documented in the code. 

These decisions induce some false positives. Additional work on edge cases may be helpful, but there are rapid diminishing returns to deterministic rules.

#### 6. Generate Status Codes

Step 06 is where status qualifiers (FORMER, FUTURE, INTERIM, EX OFFICIO, etc.) are detected, flagged, and either removed or standardized. This produces clean role titles (TitleTxt6) and parallel boolean flags for downstream modeling.

Some modifiers are kept (like "EX-OFFICIO" and "REGIONAL") as they can provide extra information. 

```
STEP-06: gen_status_codes()
|
+- gen_status_codes(comp.data, title="TitleTxt5")
|    - Main wrapper.  
|    - Cleans edge cases (“EX” → “FORMER”, “NEW” → “FORMER”, etc.).  
|    - Creates CO.X flag for co-titles.  
|    - Removes ordinal numbers from text.  
|    - Produces TitleTxt6 for downstream use.  
|    - Calls flag_and_keep() and flag_and_remove() for a set of status codes:  
|        * EXOFFICIO → keep  
|        * FORMER, FOUNDER, FUTURE, INTERIM, OUTGOING, PARTIAL, SCHED O, AT LARGE → remove  
|        * REGIONAL → keep  
|    - Runs a set of cleanup regexes (remove NON-VOTING, empty parentheses, trailing ANDs, etc.).  
|    - Returns the updated dataframe.  
|
+- flag_and_remove(df, title="TitleTxt6", s.code="FORMER")
|    - Retrieves regex variants of a given status code (via get_variants()).  
|    - Adds a boolean status flag column (e.g., FORMER.X).  
|    - Removes the status string from titles (via remove_status()).  
|
+- flag_and_keep(df, title="TitleTxt6", s.code="REGIONAL")
|    - Same as flag_and_remove() but instead of deleting the status code,  
|      it replaces it with the standardized form and keeps it in the title.  
|
+- get_variants(s.code)
|    - Pulls all known variants for a given status code from a Google Sheet (“status-codes”).  
|    - Collapses variants into a single regex OR search string.  
|
+- add_status_flag(df, title, s.code, variants)
|    - Creates a new boolean indicator column (e.g., FUTURE.X).  
|    - TRUE (1) if the title matches any variant, else FALSE (0).  
|
+- remove_status(x, variants)
|    - Removes status qualifiers from titles.  
|    - If the entire title is the qualifier (e.g., “ex officio”), replaces with the standardized version.  
|
+- standardize_status(x, s.code, variants)
|    - Replaces any variant in the text with the standardized version of the status code.  
|    - Used when status is to be preserved (flag_and_keep).  
```

The specific variants that are removed are documented in [this spreadsheet](https://docs.google.com/spreadsheets/d/1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M/edit#gid=145854139), which the code pulls from and makes it dynamic. When additional variants are identified, they can be added to that document and the code will be updated accordingly.


#### 7. Standardize Titles

Step 07 consolidates role titles into their canonical forms using a Google Sheets crosswalk. It also applies “executive fixes” for ambiguous roles (like PRESIDENT, CHANCELLOR, FINANCE DIRECTOR) to assign them consistently to CEO or CFO when conditions warrant.

For example "ADMINISTRATIVE ASSISTANT" and "ADMINISTRATION ASSISTANT" are both mapped to "ADMINISTRATIVE ASSISTANT". A full list of the mappings can be found [here](https://docs.google.com/spreadsheets/d/1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M/edit#gid=1464446536). 

```
STEP-07: standardize_titles()
|
+- standardize_titles(comp.data, title="TitleTxt6", hours="TOT.HOURS", pay="TOT.COMP", officer="F9_07_COMP_DTK_POS_OFF_X")
|    - Main wrapper.  
|    - Reads the "title-standardization" crosswalk from Google Sheets.  
|    - Deduplicates entries and removes duplicate variants.  
|    - Applies basic_csuite_fixes() to handle CEO/CFO adjustments.  
|    - Merges comp.data with standardized titles crosswalk (TitleTxt7).  
|    - Produces final standardized column.  
|
+- basic_csuite_fixes(comp.data, title="TitleTxt6", hours="TOT.HOURS", pay="TOT.COMP", officer="F9_07_COMP_DTK_POS_OFF_X")
|    - Applies conditional logic for executive titles.  
|    - Flags records with multiple titles (& separator).  
|    - Calls replace_ceo() and replace_cfo() to resolve ambiguous cases.  
|    - Creates new field TitleTxt7.  
|
|    +- replace_ceo(TitleTxt, weekly.hours, total.pay)
|    |    - Reclassifies CHANCELLOR to CEO if paid.  
|    |    - Reclassifies CHANCELLOR to BOARD PRESIDENT if unpaid.  
|    |    - (Has commented-out option to reclassify PRESIDENT → CEO based on hours).  
|    |
|    +- replace_cfo(TitleTxt, weekly.hours, total.pay, officer.flag)
|         - Reclassifies finance-related roles to CFO under certain conditions.  
|         - Examples:
|             * FINANCE DIRECTOR / HEAD OF FINANCE → CFO if officer flag = "X".  
|             * FINANCE OFFICER → CFO if officer, paid, and full-time.  
|             * VP OF FINANCE → CFO if officer flag = "X".  
|             * ACCOUNTANT → CFO if officer flag = "X".  
```


#### 8. Categorize Titles

Step 08 merges standardized titles with the final taxonomy, adds engineered features (role flags, pay/hours stats, counts, rankings), and outputs the finished dataset. This is the last stage where titles are linked to categories and organizational metrics are prepared for analysis.

```
STEP-08: categorize_titles()
|
+- categorize_titles(comp.data)
|    - Main wrapper.  
|    - Reads the "title-taxonomy" sheet from Google Sheets.  
|    - Merges taxonomy with comp.data on standardized titles.  
|    - Calls add_features() to engineer new variables.  
|    - Calls simplify_varnames() (defined elsewhere, likely utilities.R).  
|    - Produces final dataset with standardized, categorized titles and
|      engineered features.  
|
+- add_features(df)
|    - Engineers new features and summary fields:  
|        * Converts multiple status flags (FORMER.X, INTERIM.X, REGIONAL.X, etc.) to numeric.  
|        * Sets PARTIAL.X = 1 if DATE.X = 1.  
|        * Converts role category dummies (emp, board, ceo, c.level, etc.) to boolean.  
|        * Weights employees with multiple titles when calculating role counts.  
|    - Recalculates hours and compensation:  
|        * TOT.HOURS and TOT.HOURS.TOT (with related org hours included).  
|        * TOT.COMP and TOT.COMP.TOT (with/without related orgs).  
|        * Adjusts totals so people with multiple titles aren’t double-counted.  
|    - Creates secondary totals (tot.comp2, tot.hours2) excluding multi-title individuals.  
|    - Groups by EIN to calculate organizational summaries:  
|        * num.dtk (distinct individuals), num.titles, num.emp, num.board, etc.  
|        * Leadership counts (num.ceos, num.clevel, num.dirvp, num.mgr, num.spec).  
|        * Officer counts (num.pres, num.vp, num.treas, num.sec, num.mem).  
|        * Employment composition (num.paid, num.fte, num.pte).  
|        * Pay ranks and shares (pay.rank, pay.max, pay.tot, pay.pct.of.max, etc.).  
|        * Hours ranks and shares (hours.rank, pct.of.max, etc.).  
|    - Returns enriched dataframe with final structure.  
|
+- simplify_varnames(df)
|    - Not defined in this file (likely in utilities.R).  
|    - Simplifies and shortens variable names for output dataset.  
```

#### Additional Steps

Many titles are ambiguous in nature: "director" can mean executive director, or board member, and "president" can mean executive director or president of the board. All of the steps up until Step 08 are meant to clean and standardize the text. The outputs from Step 08 are in a format that is appropriate for further modeling to deal with disambiguation. 



