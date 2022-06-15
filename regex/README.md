Helpful files for regular expression functions. 

We need to make sure to use regular expressions instead of hacking together solutions (current code has a lot of hacks). 

For example:

```r
# BAD
TitleTxt <- gsub( " EXC ", " EXECUTIVE ", TitleTxt )

# GOOD
TitleTxt <- gsub( " EXC ", " EXECUTIVE ", TitleTxt )

#### EXAMPLE using regex for word boundary 
x <- c("EX DIRECTOR", "EXECUTIVE DIRECTOR")

# WRONG
gsub( " EX ", "EXECUTIVE", x )
[1] "EX DIRECTOR"        "EXECUTIVE DIRECTOR"
gsub( "EX", "EXECUTIVE", x )
[1] "EXECUTIVE DIRECTOR"        "EXECUTIVEECUTIVE DIRECTOR"

# CORRECT
gsub( "\\bEX\\b", "EXECUTIVE", x )
[1] "EXECUTIVE DIRECTOR" "EXECUTIVE DIRECTOR"
```

Another example, replacing extra spaces: 

```r
# BAD HACK 

  ## Remove Spaces 
	## ------------------------------------------------------------------------
	TitleTxt <- gsub( "  ", " ", TitleTxt )
	TitleTxt <- gsub( "  ", " ", TitleTxt )
	TitleTxt <- gsub( "  ", " ", TitleTxt )
	TitleTxt <- gsub( "  ", " ", TitleTxt )
	TitleTxt <- gsub( "  ", " ", TitleTxt )
	TitleTxt <- gsub( "  ", " ", TitleTxt )
	TitleTxt <- gsub( "  ", " ", TitleTxt )
	TitleTxt <- gsub( "  ", " ", TitleTxt )

# CORRECT WITH REGEX QUANTIFIERS  [ ]{2,} = 2 or more spaces

  TitleTxt <- gsub( "[ ]{2,}", " ", TitleTxt )
```
