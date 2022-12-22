# titleclassifier

An R package to assign raw nonprofit executive titles from Form 990 Part VII to a well-structured title taxonomy. 


## Use

```r
devtools::install_github( 'nonprofit-open-data-collective/titleclassifier' )
```


```r
library( titleclassifier )
library( dplyr )

data( tinypartvii )       # sample of form 990 part vii for 10,000 orgs

start_time <- Sys.time()  # benchmark runtime

tinypartvii %>% 
  standardize_df() %>% 
  remove_dates() %>% 
  standardize_conj() %>% 
  split_titles() %>% 
  standardize_spelling() %>% 
  gen_status_codes() %>% 
  standardize_titles() %>%
  categorize_titles() %>%
  conditional_logic() ->     df.coded
  

end_time <- Sys.time()    # runtime
end_time - start_time

# since all major steps have a wrapper function that operates on a data frame, they can be piped together
```


## Process

<p><b>   High level overview of procedural flow    </b></p>

<br>
<img width="1000" alt="image" src="https://user-images.githubusercontent.com/40209975/208956035-db4c3a19-6575-477e-bfae-9f38de66685b.png">

<br>

The expected input comes from the [rdb_build_function scripts](https://github.com/Nonprofit-Open-Data-Collective/irs990efile), specifically applied to the Form 990 Part VII section of XML-formatted e-filings. What is generated from that build function is a relational table with raw title, compensation, and other information.

The output is another relational table with cleaned titles and specific flags for title classification. Individuals with multiple titles are split onto separate rows, and the table can be filtered to find specific positions. 



### Steps in detail:

#### 1.	Standardize Data Frame

The purpose of this first step is to make basic data formatting improvements to our raw table. Some details include:
<!-- Note: "&#8594;" is code for a right arrow -->
* Renaming column titles to be more user friendly and descriptive (e.g. "F9_07_PZ_DTK_NAME" &#8594; "PersonNm" or "F9_07_PZ_DTK_TITLE" &#8594; "TitleTxt")
* Adding a "TotalComp" column that with an individual's total compensation calculated as the sum of all compensation fields 
* Converting empty/NA fields to 0 where applicable
* Capitalizing all titles and individual names
* Removing duplicate lines
<p align = "center">
<img width="819" alt="image" src="https://user-images.githubusercontent.com/40209975/182909652-eda95fb2-864a-4921-b49d-521f605acafd.png">
</p>
<p align = "center">
<b>
Example transformation of about 30 entries after initial table formatting step
</b>
</p>
</br>

#### 2. Remove dates

In this step, all titles with dates in them are processed. We want to capture date information in titles as it can be indicative of individual's only working for part of the year, which likely affects their pay. However, having dates in titles makes them less standardized. 

Thus, the date cleaning step works by using regular expressions to detect if a date is present, creating a binary flag for titles that have a date, and then removing the date from the title. As a note, since dates often include numbers, before date identification, all instances of ordinal numbering with numeric characters are converted to their alphabetic counterpart (i.e. "1st" &#8594; "first", "2nd" &#8594; "second, etc.).

<p align = "center">
<img width="365" alt="image" src="https://user-images.githubusercontent.com/40209975/182912204-d586f98c-53fa-4f27-94ed-34f718380d23.png">
</p>
<p align = "center">
<b>
Date cleaning methodology
</b>
</p>
</br>


```r
#example date cleaning

input <- "TREASURER BEGINNING MAY 30 2018"
isDatePresent <- identify_dates(input) #flag for date present
if(isDatePresent) {
  output <- remove_date(input)
  print(output)
  #"TREASURER BEGINNING"
}

#the remove_dates wrapper function applies this vectorized function process over all titles in the data frame
```


#### 3. Standardize conjunctions

Sometimes titles can include conjunctions, prepositions, and punctuation (like "and", "to", ",", "/"), but these symbols don't always refer to the same thing. For example, a title field could be "CEO AND BOARD PRESIDENT", and another could be "VP OF FINANCE AND ADMINISTRATION". In the first instance, the "and" serves to separate two different titles, but in the second it's part of a compound subject. There are many more cases like these with such symbols, so in this step, they are standardized. It is also important to note that we are operating on each individual title at this stage (as opposed to working with the entire compensation table).

<p align = "center">
<img width="305" alt="image" src="https://user-images.githubusercontent.com/40209975/182918546-5539df3c-c42e-4f52-848f-f80288c2d8a4.png">
</p>
<p align = "center">
<b>
Conjunction cleaning order of operations
</b>
</p>
</br>

With the **standardize_and** function, the method checks the substrings on each side of the "and", and if there is a recognizable title on each side, then the "and" is converted into "&", which is our delineator for fields with multiple titles. Otherwise, the "and" is left alone. 


```r
# "and" examples
title1 <- "CEO AND BOARD PRESIDENT"
title2 <- "VP OF FINANCE AND ADMINISTRATION"

standardize_and(title1) # "CEO & BOARD PRESIDENT"
standardize_and(title2) # "VP OF FINANCE AND ADMINISTRATION"
```

With the **standardize_to** function, the method checks if "to" is at the end of the title (in which case it was part of a date description) in which case it converts the "to" to "until". If the "to" is not at the end of the title, the method then checks if the substring on the left is a recognizable title, in which case it leaves it alone. In all other cases also the "to" is left alone. 


```r
# "to" examples
title1 <- "DIRECTOR TO"
title2 <- "LIAISON TO BOARD OF TRUSTEES"

standardize_to(title1) # "DIRECTOR UNTIL"
standardize_to(title2) # "LIAISON TO BOARD OF TRUSTEES"
```

With the **standardize_of** function, the method checks if "of" is part of an "as of" phrasing, in which case it converts "as of" to "since". If that is not the case, yet "of" is at the end of the title, the "of" is removed. Otherwise, the "of" is treated the same way as "to" by checking the left side substring. Additionally, all instances of "for" are replaced with "of", and "VP-" is replaced with "VP OF"


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

With the **standardize_comma** function, the method checks for the different meanings that the comma can represent. If a comma is used as a title delineator, it is replaced with "&" (same procedure as standardize_and). If it is used in place of "of" (same procedure as standardize_to), the comma is replaced with "of". For titles with multiple commas, this only applies to the first instance of the symbol. Any extraneous commas are replaced with "and". 

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

With the **standardize_slash** function, the method checks for the different meanings that the slash can represent. The slash is treated the same way as the comma; thus, when it is used as a delineator it is replaced with "&", when used like "of" it is replaced with "of", and when used extraneously it is replaced with "and". 

```r
# slash examples
title1 <- "SECRETARY/TREASURER"
title2 <- "VP/SALES"
title3 <- "SENIOR VICE PRESIDENT OF FINANCE/ADMINISTRATION"

standardize_slash(title1) # "SECRETARY & TREASURER"
standardize_slash(title2) # "VP OF MARKETING"
standardize_slash(title3) # "SENIOR VICE PRESIDENT OF FINANCE AND ADMINISTRATION"
```

Finally, the **standardize_separator** function standardizes all other miscellaneous symbols that are used as a separator. As previously mentioned, the "&" is the main symbol used to delineate titles, but ";" and "\" can also be used. As such, those symbols are replaced by "&" to make title splitting more straightforward. The other common title separators ("," and "/") have already been dealt with. 

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

#### 4. Split titles

This step separates rows with multiple titles into multiple rows. For instance, a row with the title field "SECRETARY & TREASURER" would be duplicated into two rows - one with title "SECRETARY" and the other with "TREASURER". Having standardized all of our separators, we theoretically need only split on "&". However, sometimes there are titles that are mingled together, as an error of data entry. For example "SECRETARYTREASURER" as one word would not be split into two titles when it should be. Previously, these were dealt with an unmingle() function, but for efficiency issues, this has since been removed. 

Unlike previous iterations of `split_titles` which operated on a singular title, this step is now fully vectorized and much more efficient. The process wrapper function operates on a data frame and checks all titles for an ampersand, and for those that do, it duplicates the row and removes all characters leading up to and including the symbol. This is then iteratively repeated five times to split entries with at most five different titles. Below is an example of how an entry with multiple titles would be treated.

```r
row.title <- "CFO & TREASURER & DIRECTOR"
num.titles <- identify_split_num(row.title) #will be 3

#in split_titles, if num.titles > 1, the row is duplicated and all text after "&" is removed
#thus in the original row we have the title as "CFO "

row.title <- remove_first_split(row.title) #row.title is now " TREASURER & DIRECTOR"
num.titles <- identify_split_num(row.title) #will now be 2

#since num.titles is still > 1, the row is duplicated again

row.title <- remove_first_split(row.title) #row.title is now " DIRECTOR"
num.titles <- identify_split_num(row.title) #now 1

#since num.titles is now 1, the row duplication stops and split_titles job is now done
#this process is vectorized, so all rows are simultaneously going through this process
#ultimately, the one row with title "CFO & TREASURER & DIRECTOR" has been duplicated into three rows with the titles
#"CFO", "TREASURER", and "DIRECTOR" respectively. 
```

To clarify, in the `split_titles` process, extra white space and other irregularities are appropriately dealt with. 


#### 5. Standardize spelling
In this step we substitute all abbreviations with their full title versions. Having gone through all the previous steps, we can assume that we are only dealing with one title, but we want to ensure that it is standardized. The main `fix_spelling` function is a wrapper for many smaller `fix_x` functions. The code is generalized so that in addition to common abbrevations, misspellings or words that are cut off are also standardized. Further, in the cases where we find "...VICE PRESIDENT *SUBJECT*" or "...DIRECTOR *SUBJECT*", we substitute an "of" between the main title and the subject. This also applies to "CHAIR", "DEAN", "MANAGER" as the main title. In total, there are nearly 50 `fix_x` subroutines.

Additionally, in this step, we remove any weird spacing/formatting that have trickled through our initial rounds of title cleaning.


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


There are many cases of ambiguity especially with abbreviations, so sometimes a heuristic approach is taken. For example, a singular "T" is substituted for "TRUSTEE", even though it could represent "TREASURER" as well. Or, as another example, "COMM" is converted to "COMMUNICATIONS" if in the middle of a title, but converted to "COMMITTEE" if at the end. A final example would be with "ADMIN" which can refer to "ADMINISTRATOR", "ADMINISTRATIVE", or "ADMINISTRATION". There are nearly a dozen of these cases, and the specific design choices are documented in the code. They are not without false positives, so additional work detecting and fixing more edge cases is needed (it is an issue of diminishing returns however).

#### 6. Generate status codes

Having substitute full length words for our abbreviations, the titles are nearly standardized. Some may still include remnants from date cleaning indicating temporal words (like "EMERITUS", "INTERIM", or "STARTING"), and some titles may still include extraneous modifiers. This step removes most modifiers and groups them with appropriate flags. Some modifiers are kept (like "EX-OFFICIO" and "REGIONAL") as they can provide extra information. Below is a code snippet of the flag and keep/remove process.
```
  comp.data <- 
    comp.data %>% 
    flag_and_keep(    s.code="EXOFFICIO"  )  %>% 
    flag_and_remove(  s.code="FORMER"     )  %>% 
    flag_and_remove(  s.code="FOUNDER"    )  %>%
    flag_and_remove(  s.code="FUTURE"     )  %>%
    flag_and_remove(  s.code="INTERIM"    )  %>%
    flag_and_remove(  s.code="OUTGOING"   )  %>%
    flag_and_remove(  s.code="PARTIAL"    )  %>% 
    flag_and_remove(  s.code="SCHED O"    )  %>%  
    flag_and_remove(  s.code="AT LARGE"   )  %>%  
    flag_and_keep(    s.code="REGIONAL"   )    
```

The specific variants that are removed are documented in [this spreadsheet](https://docs.google.com/spreadsheets/d/1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M/edit#gid=145854139), which the code pulls from and makes it dynamic. When additional variants are identified, they can be added to that document and the code will be updated accordingly.


#### 7. Standardize titles

Like step 6 with generating status codes, this step also pulls from the Google Sheet. More specifically, it converts variants of titles to a canonical form. For example "ADMINISTRATIVE ASSISTANT" and "ADMINISTRATION ASSISTANT" are both mapped to "ADMINISTRATIVE ASSISTANT" in this step. A full list of the mappings can be found [here](https://docs.google.com/spreadsheets/d/1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M/edit#gid=1464446536). There are several thousand titles to be coded and mapped, and the team is actively working on updating and increasing the dataset for better results.

In addition to the mappings, there are some basic heuristics applied in this step to improve overall title classification accuracy. Most notably, some C-Suite positions like CFO and CEO are corrected based on information within an organization. For example, depending on pay hierarchy, hours worked, and officer positionship, a "FINANCE DIRECTOR" can be corrected to "CFO". More of these conditional logic processes are applied in step 9 where it is specialized. Unlike step 9, the heuristics applied here are mostly with titles that were not successfully standardized (i.e. non-coded titles).


#### 8. Categorize titles

With the standardized titles, they are next classified, again using the Google Sheets document. This classification is a title taxonomy and includes helpful information about domain category, domain label, and SOC code amongst other things. This is also a living and breathing document, and the current work being done can be found [here](https://docs.google.com/spreadsheets/d/1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M/edit#gid=756762038).


#### 9. Conditional Logic

Step 9 is basically a data refinement step. The majority of titles have been standardized and categorized, but some are still not. Part of this issue lies with the great variance in filers having different standards and understandings when filling out the 990 forms, but some are fundamental issues. Conditional logic tries to look broad strokes at non-coded or potentially miscoded titles and use organization information (like pay rank, hours worked, board role, etc.) to try to recategorize if needed. This step is not completely fleshed and could always use more refinement, but the two major conditional logic processes applied are `clean_up_ceos` and `director_correction`.

With `clean_up_ceos`, organizations with multiple CEOs are inspected and adjusted. Sometimes, the multiple CEOs makes sense as it's a transition and there are individuals with "interim" or "past" flags checked, but other times it's an issue with CEOs with multiple titles being split into multiple rows and then double counted. In those instances, they are filtered out based on equivalence checking on other fields. 
With `director_correction', the issue of "DIRECTOR" being used as a term for board member is addressed. In step 7, all rows with "DIRECTOR" are converted to the standardized "BOARD MEMBER" and in this step we check to make sure that that was the correct change. If an individual has a raw director title, but is paid and not on the board, then their title is corrected to "DIRECTOR" instead of the rough conversion to "BOARD MEMBER".

In the future, it would be helpful to further build out the `conditional_logic` with more cases where situational information can be helpful in correcting an individual's title. To determine these rules, however, requires painstaking time sifting through filtered results and trying to draw patterns. As of now, the package is in working condition and functions as intended in filtering and categorizing over 90% of all compensation data from the 990 Part VII form (a dataset of several million entries).


