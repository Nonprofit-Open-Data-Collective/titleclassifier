# titleclassifier

An R package to assign raw nonprofit executive titles from Form 990 Part VII to a well-structured title taxonomy. 


Steps: 

1. Split compound titles into separate strings 
2. Apply cleaning steps (capitalization, punctuation, remove numbers, etc) 
3. Standardize titles (e.g. PRES -> PRESIDENT, DIR -> DIRECTOR) 
4. Assign roles 
5. Assign status 
6. Assign hierarchy 
7. Assign functional domains of responsibility 
8. If they are a board member, assign governance hierarchy 


Example functions from peopleparser: 

* https://github.com/Nonprofit-Open-Data-Collective/peopleparser/blob/master/R/prep.name.R
* https://github.com/Nonprofit-Open-Data-Collective/peopleparser/blob/master/R/remove.punctuation.R
* https://github.com/Nonprofit-Open-Data-Collective/peopleparser/blob/master/R/trim.spaces.R
* https://github.com/Nonprofit-Open-Data-Collective/peopleparser/blob/master/R/prep.name.R 
