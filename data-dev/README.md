These files contain a subset of Form 990 Part VII that contain only unique (non-duplicated) title strings. 

```r
# load data files directly from github by: 
URL <- "https://github.com/Nonprofit-Open-Data-Collective/titleclassifier/blob/main/data-dev/2018-part-vii-unique-titles.rds?raw=true"
d <- readRDS(gzcon(url( URL )))
```

There are a little over 900,000 unique title strings across across all files. 

```r
> head( d$F9_07_PZ_DTK_TITLE, 100 )
  [1] "PRES"                           "SEC/TREAS"                     
  [3] "PROGRAM ADMINISTRATOR"          "HOUSE PARENT"                  
  [5] "DIRECTOR"                       "PRESIDENT"                     
  [7] "Teacher/Trustee"                "TRustee"                       
  [9] "Trustee"                        "Head of School"                
 [11] "CFO"                            "Director of Development"       
 [13] "Director of Operations"         "Director of Admissions"        
 [15] "Lower School Director"          "Dir of Technology"             
 [17] "PRESIDENT/DIRECTOR"             "VICE PRESIDENT/DIRECTOR"       
 [19] "TREASURER/DIRECTOR"             "SECRETARY/DIRECTOR"            
 [21] "ASSISTANT SECRETARY/DIR."       "LIFE-DIRECTOR"                 
 [23] "CHIEF EXECUTIVE OFFICER"        "FORMER CHIEF FINANCIAL OFFICER"
 [25] "CHIEF FINANCIAL OFFICER"        "VP STRATEGIC DEVELOPMENT"      
 [27] "CHIEF CLINICAL OFFICER"         "FORMER VP HUMAN RESOURCES"     
 [29] "CHIEF MEDICAL DIRECTOR"         "CHIEF INFORMATION OFFICER"     
 [31] "FORMER DIRECTOR OF RESEARCH"    "President"                     
 [33] "Vice-President"                 "Secretary"                     
 [35] "Treasurer"                      "Immediate Past President"      
 [37] "director"                       "PRES-ELECT"                    
 [39] "EX-OFICIO PR"                   "TREASURER"                     
 [41] "SECRETARY"                      "EXEC DIRECTO"                  
 [43] "CHAIR-GALA"                     "CHAIR-LITERA"                  
 [45] "MEMBER"                         "PROGRAM COOR"                  
 [47] "BOOKKEEPER"                     "PENDING"                       
 [49] "VICE PRESIDE"                   "SUPERINTENDE"                  
 [51] "PENDING/WEB"                    "BOARD MEMBER"                  
 [53] "CHAIR-STUFF"                    "EX-OFFICIO"                    
 [55] "CHAIR-PRINCI"                   "EX. DIRECTOR"                  
 [57] "TRUSTEE"                        "CHAIR"                         
 [59] "CHIAR EMERIT"                   "PRES/ART DIR"                  
 [61] "CHAIRMAN"                       "VICE CHAIRMAN"                 
 [63] "FINANCE DIRE"                   "GAMING MANAG"                  
 [65] "QUARTERMASTER"                  "COMMANDER"                     
 [67] "PRESIDENT - GIORGIO FOODS,"     "EXEC/ART. DI"                  
 [69] "VICE-PRES."                     "VICE PRESIDENT"                
 [71] "SERGEANT IN ARMS"               "Chairman"                      
 [73] "Vice Chairman"                  "Secretary and Treasurer"       
 [75] NA                               "Executive Director"            
 [77] "Vice President"                 "Director"                      
 [79] "Director, Chief of Staff"       "CEO"                           
 [81] "Physician"                      "CRNA"                          
 [83] "SECRETARY/TREASURER"            "EXECUTIVE DIRECTOR"            
 [85] "RYL PROGRAM DIRECTOR"           "Board Member"                  
 [87] "Board Chairman"                 "Fiscal Director"               
 [89] "VICE-CHAIRMAN"                  "Chair"                         
 [91] "Chair-Elect"                    "Treasurer/Secretary"           
 [93] "Past Chair"                     "Board of Directors"            
 [95] "IT HEAD"                        "PrINCIPAL"                     
 [97] "Fundraising Chair"              "Meet Coordinator"              
 [99] "EXECUTIVE DI"                   "DIRECTOR through nov 09"
 
> head( d )
            OBJECT_ID       EIN                           NAME TAXYR FORMTYPE
1  201103169349300325 591971002                    ANGELUS INC  2009      990
2  201103169349300325 591971002                    ANGELUS INC  2009      990
3  201103169349300325 591971002                    ANGELUS INC  2009      990
4  201103169349300325 591971002                    ANGELUS INC  2009      990
5  201103169349300325 591971002                    ANGELUS INC  2009      990
26 201123129349301012 381969044 GRADUATE MEDICAL EDUCATION INC  2009      990
                                                                   URL
1  https://s3.amazonaws.com/irs-form-990/201103169349300325_public.xml
2  https://s3.amazonaws.com/irs-form-990/201103169349300325_public.xml
3  https://s3.amazonaws.com/irs-form-990/201103169349300325_public.xml
4  https://s3.amazonaws.com/irs-form-990/201103169349300325_public.xml
5  https://s3.amazonaws.com/irs-form-990/201103169349300325_public.xml
26 https://s3.amazonaws.com/irs-form-990/201123129349301012_public.xml
   F9_07_PZ_DTK_NAME    F9_07_PZ_DTK_TITLE F9_07_PZ_DTK_AVE_HOURS_WEEK
1     PAULINE SHAVER                  PRES                      040.00
2       DAVID SHAVER             SEC/TREAS                      040.00
3        JOSEPH NERI PROGRAM ADMINISTRATOR                      050.00
4        JOSEPH NERI          HOUSE PARENT                      060.00
5    STEPHEN C BOOTH              DIRECTOR                      001.00
26    REISDORFF EARL             PRESIDENT                        1.00
   F9_07_PC_DTK_POS_TRUSTEE_INDIV F9_07_PC_DTK_POS_OFFICER F9_07_PZ_COMP_DIRECT
1                               X                        X                15600
2                               X                        X                27300
3                               X                     <NA>                69315
4                            <NA>                     <NA>                28282
5                               X                     <NA>                    0
26                              X                        X                    0
   F9_07_PZ_COMP_RELATED F9_07_PZ_COMP_OTHER F9_07_PC_DTK_POS_KEY_EMPLOYEE
1                      0                   0                          <NA>
2                      0                   0                          <NA>
3                      0                   0                             X
4                      0                   0                             X
5                      0                   0                          <NA>
26                     0                   0                          <NA>
   F9_07_PC_DTK_POS_HIGH_COMP_EMP F9_07_PC_DTK_POS_FORMER
1                            <NA>                    <NA>
2                            <NA>                    <NA>
3                            <NA>                    <NA>
4                            <NA>                    <NA>
5                            <NA>                    <NA>
26                           <NA>                    <NA>
   ContriToEmplBenefitPlansEtc ExpenseAccountOtherAllowances
1                         <NA>                          <NA>
2                         <NA>                          <NA>
3                         <NA>                          <NA>
4                         <NA>                          <NA>
5                         <NA>                          <NA>
26                        <NA>                          <NA>
   F9_07_PC_DTK_POS_TRUSTEE_INST F9_07_PZ_DTK_AVE_HOURS_WEEK_RLTD
1                           <NA>                             <NA>
2                           <NA>                             <NA>
3                           <NA>                             <NA>
4                           <NA>                             <NA>
5                           <NA>                             <NA>
26                          <NA>                             <NA>
   F9_07_EZ_COMP_BENF
1                <NA>
2                <NA>
3                <NA>
4                <NA>
5                <NA>
26               <NA>

```
