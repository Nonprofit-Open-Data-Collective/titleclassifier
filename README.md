# titleclassifier

An R package to assign raw nonprofit executive titles from Form 990 Part VII to a well-structured title taxonomy. 

## Process

<p align = "center">
<b>
High level overview of procedural flow
</b>
</p>
</br>

<p align = "center">
<img width="357" alt="image" src="https://user-images.githubusercontent.com/40209975/182947784-f13ee7ba-d622-477a-9ce2-43534807fd1f.png">
</p>




```r
df %>% 
  standardize_df() %>% 
  remove_dates() %>% 
  standardize_conj() %>% 
  split_titles() %>% 
  standardize_spelling() %>% 
  gen_status_codes() %>% 
  standardize_titles() %>%
  categorize_titles() ->     df.coded
```



The expected input comes from the [rdb_build_function scripts](https://github.com/Nonprofit-Open-Data-Collective/irs990efile), specifically applied to the Form 990 Part VII section of XML-formatted e-filings. What is generated from that build function is a relational table with raw title, compensation, and other information.

The output is another relational table with cleaned titles and specific flags for title classification. Individuals with multiple titles are split onto separate rows, and the table can be filtered to find specific positions. 

### Steps in detail:

#### 1.	Format raw table

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

#### 2. Clean dates

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
isDatePresent <- identify_date(input) #flag for date present
if(isDatePresent) {
  output <- remove_date(input)
  print(output)
  #"TREASURER BEGINNING"
}

#this is basically how the clean_dates function works but on the relational table as a whole
```

#### 3. Clean conjunctions

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

#### 4. Split titles

This step separates rows with multiple titles into multiple rows. For instance, a row with the title field "SECRETARY & TREASURER" would be duplicated into two rows - one with title "SECRETARY" and the other with "TREASURER". Having standardized all of our separators, we theoretically need only split on "&". 

However, sometimes there are titles that are mingled together, as an error of data entry. For example "SECRETARYTREASURER" as one word would not be split into two titles when it should be. So accordingly, for all titles with at least 10 characters (heuristic) and no spaces, a simple unmingling check is performed to determine if two words/titles have been smushed together. If they have, a **standardize_space** function is called which converts the space to "&" if the two substrings are both recognizable titles (similar to how standardize_and works). This substep is completed before the titles are split on "&" and serves as one last standardization before we fully split into separate rows.

```r
title1 <- "CFO & TREASURER"
title2 <- "SECRETARYTREASURER"

split_titles(title1) # c("CFO ", " TREASURER")
split_titles(title2) # c("SECRETARY", "TREASURER")

#for each entry in the output vector from split_titles we duplicate a row in the table, 
#with "TitleTxt2" being the singularized title
```


#### 5. Apply substitutions
In this step we substitute all abbreviations with their full title versions. Having gone through all the previous steps, we can assume that we are only dealing with one title, but we want to ensure that it is standardized. The main **apply_substitution** function is a wrapper for many smaller **substitute_x** functions. The code is generalized so that in addition to common abbrevations, misspellings or words that are cut off are also standardized. Further, in the cases where we find "...VICE PRESIDENT *SUBJECT*" or "...DIRECTOR *SUBJECT*", we substitute an "of" between the main title and the subject. This also applies to "CHAIR", "DEAN", "MANAGER" as the main title. 

Additionally, in this step, we remove any weird spacing/formatting that have trickled through our initial rounds of title cleaning.


```r
#examples

apply_substitute("VP") # "VICE PRESIDENT"
apply_substitute("DIRECTOR FACILITIES") # "DIRECTOR OF FACILITIES"
apply_substitute("SR D MARKETING") # "SENIOR DIRECTOR OF MARKETING"
apply_substitute("VICE PRESID") # "VICE PRESIDENT"
apply_substitute("SGT AT ARMS") # "SERGEANT AT ARMS"
apply_substitute("TRTEE") # "TRUSTEE"
apply_substitute("SECY") # "SECRETARY"
```

Some title standardization was also completed at this step, and namely any version of "EXECUTIVE DIRECTOR" was mapped onto "CEO". In addition, unambiguous c-suite positions were condensed to their abbreviations. Not all c-suite positions fall into this category however.
```r
apply_substitute("EX DIR") # "CEO"
apply_substitute("CHIEF EXEC OFFICER") # "CEO"
apply_substitute("CHIEF FINANCIAL OFFICER") # "CFO"
apply_substitute("CHIEF ADVANCEMENT OFFICER") # "CHIEF ADVANCEMENT OFFICER"
```


There are many cases of ambiguity especially with abbreviations, so sometimes a heuristic approach is taken. For example, a singular "T" is substituted for "TRUSTEE", even though it could represent "TREASURER" as well. Or, as another example, "COMM" is converted to "COMMUNICATIONS" if in the middle of a title, but converted to "COMMITTEE" if at the end. A final example would be with "ADMIN" which can refer to "ADMINISTRATOR", "ADMINISTRATIVE", or "ADMINISTRATION". There are nearly a dozen of these cases, and the specific design choices are documented in the code. They are not without false positives, so additional work detecting and fixing more edge cases is needed (it is an issue of diminishing returns however).

#### 6. Generate status codes

Having substitute full length words for our abbreviations, the titles are nearly standardized. Some may still include remnants from date cleaning indicating temporal words (like "EMERITUS", "INTERIM", or "STARTING"), and some titles may still include extraneous modifiers (like "EX-OFFICIO", "CO-", or "AT LARGE"). This step removes these modifiers and groups them with appropriate flags. 

<p align = "center">
<img width="580" alt="image" src="https://user-images.githubusercontent.com/40209975/182951683-c23a4cef-0f97-49e8-b8ee-62b348e9b9c6.png">
</p>
<p align = "center">
<b>
Overview of status code generation
</b>
</p>

With the **categorize_miscellaneous** wrapper function, instances of the literal strings (and their derivatives) "SCHEDULE O", "AT LARGE", "AS NEEDED", "EX-OFFICIO", and "CO-" were identified, flagged, then removed. For example, a title of "TRUSTEE EX-OFFICIO" will have a flag in the "EX-OFFICIO" column, and have the filtered title end up as simply "TRUSTEE". "Quantifier" refers to ordinal numbers like "FIRST" and "SECOND". They were removed because while they do add hierarchical value to a title, they are not part of any standardized titleset.

With the **categorize_qualifiers** wrapper function, if present versions of the literal strings "FORMER", "FUTURE", "INTERIM", and "REGIONAL" were generalized, flagged, and then removed. What this means is that the titles "PRESIDENT EMERITUS" and "PAST PRESIDENT" both end up as "PRESIDENT" with a flag in the "FORMER" column. There are significantly more variants of these qualifiers as opposed to the miscellaneous ones, and [this sheet](https://docs.google.com/spreadsheets/d/1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M/edit#gid=145854139) provides a dynamic view of the mappings. 

As additional codes are added to the spreadsheet, the code dynamically changes.


#### 7. Standardize titles

With our pseudo-standardized titles at this point, some additional standardization substitutions may still be necessary. This step is not fully implemented yet, but the variants of titles will be mapped to a canonical standardized version for improved classification as shown in [this sheet](https://docs.google.com/spreadsheets/d/1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M/edit#gid=1464446536).

#### 8. Categorize titles

We have an old implementation of title categorization in place. It has achieved nearly 98% title classification in testing with a hundred thousand indvidual relational table. Still it is currently being reworked to act like a wrapper function with individual categorize_x functions. Some examples of completely categorized sets can be found [here](https://github.com/Nonprofit-Open-Data-Collective/titleclassifier/tree/main/test-tables/split).

*Steps 7 and 8 are not completely finished.*



## Improvements

* Looking at the flow of our code, in retrospect it seems more effective generate status codes before the "apply substitutes" and "standardize titles" steps.
* As we comb through more examples, we will find more places where the transition word heuristics fail. With those discoveries, we can improve our probabilistic cleaning.


