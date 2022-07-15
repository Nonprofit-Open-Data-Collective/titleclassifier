#' # Creating Catagories
#' 
#' 
#' 
#' ## Create Catagories  
#' 
#' Creating Catagoies for the titles of Non-profit employees listed on 501c3s was accomplished in the following steps.
#' 
#' 1. Identify the **Most Common Titles** in the Data.
#' 2. Identify and Define **Catagogies** for sorting titles based off this frame
#' 3. **Random Sample** Titles and organizations to check the validity of Catagories, and see if new catagories need to be created.
#' 4. **Descriptive Statistics** For Catagories
#' 
#' 
#' 
#' ### Most Common Titles 
#' 
#' When developing catagories and hand coding titles its important to understand the costs and benefits of this process. Initial exploration of the dataset found that the 100 Most common titles account for roughly 53.05% of all titles. The next 1000 most common titles accounts for 67% of all titles. This suggests there is some diminishing returns for 
#' increasing the number of titles we use to form our initial catagories. As it takes about an hour to assign 100 titles to catagories, its important to optimize the cost and benefit of including more catagories in our initial conceptualization.
#' 
#' 1. Cost Benefit analysis
#' 
#' 
#' 
## ------------------------------------------------------------------------
#

titles <- d2$TitleTxt2
Total_Unique_Titles <- length( unique( titles ) )


# Move by 100 to check diminishing returns on Top Titles coding
test <- c( 1:10 )
for ( i in 1:10 ){
ii <- i*100
x <- table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( )
test[i] <-  x$Freq[1:ii] %>% sum
}

dim.return.100 <- cbind( ( 1:10 )*100,test )
dim.return.100 <- cbind( dim.return.100, round( dim.return.100[,2] / nrow( d2 ), digits = 4 )*100 )
dim.return.100 <- cbind( dim.return.100, dim.return.100[,2] - lag( dim.return.100[,2] ) )
dim.return.100 <- cbind( dim.return.100, dim.return.100[,3] - lag( dim.return.100[,3] ) )
colnames( dim.return.100 ) <- c( "Titles", "Obs", "%", "Gain","% Gain" )

dim.return.100[1,4] <- dim.return.100[1,2]
dim.return.100[1,5] <- dim.return.100[1,3]

Titles_1000 <- table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 1000 )

Titles_1000_sum  <- Titles_1000$Freq %>% sum

# Move by 1000 to check diminishing returns on Top Titles coding
test <- c( 1:10 )
for ( i in 1:10 ){
ii <- i*1000
x <- table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( )
test[i] <-  x$Freq[1:ii] %>% sum
}

dim.return.1000 <- cbind( ( 1:10 )*1000,test )
dim.return.1000 <- cbind( dim.return.1000, round( dim.return.1000[,2] / nrow( d2 ), digits = 4 )*100 )
dim.return.1000 <- cbind( dim.return.1000, dim.return.1000[,2] - lag( dim.return.1000[,2] ) )
dim.return.1000 <- cbind( dim.return.1000, dim.return.1000[,3] - lag( dim.return.1000[,3] ) )
colnames( dim.return.1000 ) <- c( "Titles", "Obs", "%", "Gain","% Gain" )
dim.return <- dim.return.100 %>% rbind( dim.return.1000 )
dim.return[-11,]%>%pander


#' 
#' This suggests that that getting the first 100 most common Titles by far 
#  allows us to capture 53% of all titles observations. While Coding The 
#  next 100 observations only gets us 4.65% more observations. 
#  And clearly coding more than 1000 observations is a losing proposition.
#' 
## ------------------------------------------------------------------------

Titles_100 <- table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 100 )

Titles_100_sum  <- Titles_100$Freq %>% sum

cat( "The top 100 titles include ", 
    round( Titles_100_sum / nrow( d2 ), digits = 4 )*100,"% of all titles" )
write.csv( Titles_100, "inspection/Top-100-Titles.csv", row.names=F )

#' 
#' 2. 100 Most Common Titles
#' 
## ---- fig.cap = "100 Most Common Titles"---------------------------------
Titles_name_100 <- Titles_100$titles %>% as.vector( )

res <- cbind.data.frame( split( Titles_name_100, rep( 1:20, times=5 ) ), stringsAsFactors=F )
res <- t( res ) %>% as.data.frame( )
colnames( res ) <-  c( "Top 1-20","Top 21-40","Top 41-60","Top 61-80","Top 81-100" )
knitr::kable( res )

#' 
#' 3. Create List of Titles for 101 - 1000 most common titles [By 100]
#' 
## ------------------------------------------------------------------------

Titles_1000 <- table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 1000 )


Titles_200 <- Titles_1000[101:200,]
Titles_300 <- Titles_1000[201:300,]
Titles_400 <- Titles_1000[301:400,]
Titles_500 <- Titles_1000[401:500,]
Titles_600 <- Titles_1000[501:600,]
Titles_700 <- Titles_1000[601:700,]
Titles_800 <- Titles_1000[701:800,]
Titles_900 <- Titles_1000[801:900,]
Titles_1000 <- Titles_1000[901:1000,]

write.csv( Titles_200,  "inspection/Most Common 101-200.csv", row.names=F )
write.csv( Titles_300,  "inspection/Most Common 201-300.csv", row.names=F )
write.csv( Titles_400,  "inspection/Most Common 301-400.csv", row.names=F )
write.csv( Titles_500,  "inspection/Most Common 401-500.csv", row.names=F )
write.csv( Titles_600,  "inspection/Most Common 501-600.csv", row.names=F )
write.csv( Titles_700,  "inspection/Most Common 601-700.csv", row.names=F )
write.csv( Titles_800,  "inspection/Most Common 701-800.csv", row.names=F )
write.csv( Titles_900,  "inspection/Most Common 801-900.csv", row.names=F )
write.csv( Titles_1000, "inspection/Most Common 901-1000.csv", row.names=F )


#' ### Title Catagories
#' 
#' By law every organization is required to have at least 4 positions filled:
#' 
#' Top Management Official
#' Top Financial Official
#' Secretary
#' 
#' Our current catagories are as follows:
#' 
#' * CEO	
#' 
#' * CFO
#' 
#' * Treasurer
#' 
#' * Deputy CEO	
#' 
#' * Secretary	
#' 
#' * COO	
#' 
#' * Director/Trustee	
#' 
#' * Human Resources
#' 
#' * Communications
#' 
#' * Department Head	
#' 
#' * Management	
#' 
#' * Technology/Information
#' 
#' * Development	
#' 
#' * Other	Individual Positions
#' 
## ------------------------------------------------------------------------
Titles_200 <- Titles_1000[101:200,]
Titles_300 <- Titles_1000[201:300,]
Titles_400 <- Titles_1000[301:400,]
Titles_500 <- Titles_1000[401:500,]
Titles_600 <- Titles_1000[501:600,]
Titles_700 <- Titles_1000[601:700,]
Titles_800 <- Titles_1000[701:800,]
Titles_900 <- Titles_1000[801:900,]
Titles_1000 <- Titles_1000[901:1000,]



#' 
#' 
#' 
#' 
#' ### Samples of Titles
#' 
#' We took 2 approaches to sampling random titles, the first approach was to take 100 random organizations. The second approach was to take a sample of 1000 random titles regardless of the organization. The goal is to get a good mix of common titles and uncommon titles, as this lets us test the flexibility and robustness of the categories.
#' 
#' 1. Sample of Titles of 100 Organizations
#' 
## ------------------------------------------------------------------------
set.seed( 99 )
unique.ein <- d2$FilerEIN %>% unique 
i <- length( unique.ein )
ii <- sample( unique.ein, size = 100, replace = FALSE )

ein.sample <- d2[d2$FilerEIN %in% ii, ]
cat( "\nDimensions of the sample:", dim( ein.sample ), "[Rows X Columns] \n" )

ein.sample$top100 <- 0
ein.sample$top100[ein.sample$TitleTxt %in% Titles_name_100] <- 1
cat( "Number of titles in the sample in the top 100 Titles: ", sum( ein.sample$top100 ), "\n" )
cat( "Percentage of titles in the sample in the top 100 Titles: ", signif( sum( ein.sample$top100 )/nrow( ein.sample ), 3 ), "%\n" )
ein.sample.no100 <- ein.sample %>% filter ( top100 == 0 )
titles.2 <- ein.sample.no100$TitleTxt %>% unique

write.csv( titles.2, "inspection/Organization Sample.csv", row.names = F )

Org100.gen.Table <- d2$TitleTxt[d2$TitleTxt %in% as.character( titles.2 )] %>% table( ) %>% sort( decreasing=T ) %>% as.data.frame( )  %>% filter( Freq >20 )
colnames( Org100.gen.Table )[1] <- "Titles"
Org100.gen.Table %>% head(20) %>% pander
Org100.gen.Table[,2] %>% sum



#' 
#' 
#' 
#' 2. Sample of 1000 Titles
#' 
## ------------------------------------------------------------------------
set.seed( 12 )

d2.sample <- sample_n( d2, size = 1000, replace = FALSE )

cat( "\nDimensions of the sample: ", dim( d2.sample ), "[Rows X Columns] \n" )

d2.sample$top100 <- 0
d2.sample$top100[d2.sample$TitleTxt %in% Titles_name_100] <- 1
d2.sample$Org100 <- 0
d2.sample$Org100[d2.sample$TitleTxt %in% titles.2] <- 1

cat( "Number of titles in the sample in the top 100 Titles: ", 
     sum( d2.sample$top100 ), "\n" )
cat( "Percentage of titles in the sample in the top 100 Titles: ", 
     sum( d2.sample$top100 )/nrow( d2.sample ), "%\n" )

d2.sample.no100 <- d2.sample %>% filter ( top100 == 0 & Org100 == 0 )

#Checks the generalizablity of the sample.
Top.Titles.sample <- 
  table( d2.sample.no100$TitleTxt ) %>% 
  sort( decreasing=T ) %>% 
  as.data.frame( )

sample.gen.Table <- 
  d2$TitleTxt[d2$TitleTxt %in% as.character( Top.Titles.sample[,1] )] %>% 
  table( ) %>% sort( decreasing=T ) %>% 
  as.data.frame( )  %>% 
  filter( Freq > 1 )

colnames( sample.gen.Table )[1] <- "Titles"

sample.gen.Table %>% head(20) %>% pander

sample.gen.Table[,2] %>% sum

titles.3 <- d2.sample.no100$TitleTxt %>% unique
write.csv( titles.3, "cleaned-titles/title-sample-2016.csv", row.names = F )

#' 
#' 
#' 
#' 
#' 