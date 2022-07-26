library( dplyr )

d <- read.csv( "soc-temp.csv" )

d$soc.code <- paste0( d$MajorGroup, d$MinorGroup, d$BroadGroup, d$DetailedOccupation )

d$group.1 <- grepl( "-0000", d$soc.code ) %>% as.numeric()
d$group.2 <- grepl( "-[^0].00", d$soc.code ) %>% as.numeric()
d$group.3 <- grepl( "-[^0].[^0]0", d$soc.code ) %>% as.numeric()
d$group.4 <- grepl( "-...[^0]", d$soc.code ) %>% as.numeric()

d$soc.code1 <- paste0( substr( d$soc.code, 1, 3 ), "0000" )
d$soc.code2 <- paste0( substr( d$soc.code, 1, 5 ), "00" )
d$soc.code2[ d$group.1 == 1 ] <- ""
d$soc.code3 <- paste0( substr( d$soc.code, 1, 6 ), "0" )



# SANITY CHECKS 

setdiff( d$soc.code1, d$MajorGroup )
setdiff( d$MajorGroup, d$soc.code1 )

setdiff( d$soc.code2, d$MinorGroup )
setdiff( d$MinorGroup, d$soc.code2 )

setdiff( d$soc.code3, d$BroadGroup )
setdiff( d$BroadGroup, d$soc.code3 )

# DON'T EXIST:
#  "11-3100" "11-9100" "13-1100", ...
# SHOULD BE: 
#  "11-3000" "11-9000" "13-1000"
recode.these <- setdiff( d$soc.code2, d$MinorGroup )
code2.temp <- paste0( substr( d$soc.code, 1, 4 ), "000" )
d$soc.code2[ d$soc.code2 %in% recode.these ] <- code2.temp[ d$soc.code2 %in% recode.these ]

setdiff( d$soc.code2, d$MinorGroup )
setdiff( d$MinorGroup, d$soc.code2 )
# that fixed it


d.lev1 <- filter( d, group.1 == 1 ) %>% select( Label, soc.code )
names(d.lev1) <- c("LEV1","soc.code") 
d.lev2 <- filter( d, group.2 == 1 ) %>% select( Label, soc.code )
names(d.lev2) <- c("LEV2","soc.code") 
d.lev3 <- filter( d, group.3 == 1 ) %>% select( Label, soc.code )
names(d.lev3) <- c("LEV3","soc.code") 



# SHOULD BE THE SAME
# plus 1 for empty titles
nrow( d.lev1 )
length( unique( d$MajorGroup ) )

nrow( d.lev2 )
length( unique( d$MinorGroup ) )

nrow( d.lev3 )
length( unique( d$BroadGroup ) )

 
d <- merge( d, d.lev1, by.x="soc.code1", by.y="soc.code", all.x=T )
d <- merge( d, d.lev2, by.x="soc.code2", by.y="soc.code", all.x=T )
d <- merge( d, d.lev3, by.x="soc.code3", by.y="soc.code", all.x=T )

dd <- filter( d, group.4 == 1 )

dd$soc.code1 <- paste0( "SOC-", dd$soc.code1 )
dd$soc.code2 <- paste0( "SOC-", dd$soc.code2 )
dd$soc.code3 <- paste0( "SOC-", dd$soc.code3 )
dd$soc.code <- paste0( "SOC-", dd$soc.code )

head( dd, 25 )

# should be the same
nrow(dd)
length( unique( d$DetailedOccupation ) )

keep.these <- c( "soc.code1", "soc.code2", "soc.code3", "soc.code",
                 "LEV1", "LEV2", "LEV3", "Label" )
dd <- dd[ keep.these ]

names(dd) <- c( "MajorGroup","MinorGroup","BroadGroup","DetailedOccupation", 
                "LEV1", "LEV2", "LEV3", "LEV4" )


write.csv( dd, "tidy-soc-codes.csv", row.names=F )



