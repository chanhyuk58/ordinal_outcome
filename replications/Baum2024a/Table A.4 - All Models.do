***TO RUN REMOVE THE "*" BEFORE "CD" AND INSERT THE NAME OF THE DIRECTORY WITH THE RECODED DATA.

*cd "..."


*****JANARY*****

use "January Panel Data File (for replication)_recoded.dta",clear

*****TABLE A.4, MODEL 1 (FIGURE A.2; TOP RIGHT-HAND PANEL): CONSPIRACY BELIEFS INTERACTION MODELS*******

estsimp regress supportstormers01 phq9med phq9hi  phq9low_fn12 phq9med_fn12 phq9hi_fn12 anypolactive  dem rep ind libcon interest black white asian hispanic income_r edu_r age  socialmediaelecnewssum10 el_conf male trumpsupporter [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 male .48 trumpsupporter .34 anypolactive .27

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

***note that "hi" for depression in this set of simulations is actually medium depression (just didn't change the nomenclature for the simulations)***

***lolo***
setx  phq9med 0 phq9hi 0 phq9low_fn12 0 phq9med_fn12 0 phq9hi_fn12 0 

simqi, ev genev(phqlofnlo)

***hilo***
setx  phq9med 1 phq9hi 0 phq9low_fn12 0 phq9med_fn12 0 phq9hi_fn12 0 
simqi, ev genev(phqhifnlo)

***lohi***
 setx  phq9med 0 phq9hi 0 phq9low_fn12 .42 phq9med_fn12 0 phq9hi_fn12 0  

simqi, ev genev(phqlofnhi)

***hihi***
 setx  phq9med 1 phq9hi 0 phq9low_fn12 0 phq9med_fn12 .42 phq9hi_fn12 0 
 
simqi, ev genev(phqhifnhi)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo* 


*****TABLE A.4, MODEL 2 (FIGURE A.2; BOTTOM RIGHT-HAND PANEL): PARTICIPATORY INCLINATION INTERACTION MODELS*******

estsimp regress supportstormers01 phq9med phq9hi misperceptionssum12 phq9low_anypolactive phq9med_anypolactive phq9hi_anypolactive  dem rep ind libcon interest black white asian hispanic income_r edu_r  age socialmediaelecnewssum10 el_conf trumpsupporter male  [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 male .48 trumpsupporter .34 misperceptionssum12 .19

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

***note that "hi" for depression in this set of simulations is actually medium depression (just didn't change the nomenclature for the simulations)***

***lolo***
setx phq9med 0 phq9hi 0 phq9low_anypolactive 0  phq9med_anypolactive 0 phq9hi_anypolactive 0

simqi, ev genev(phqloanypollo)

***hilo***
setx phq9med 1 phq9hi 0 phq9low_anypolactive 0  phq9med_anypolactive 0 phq9hi_anypolactive 0
simqi, ev genev(phqhianypollo)

***lohi***
setx phq9med 0 phq9hi 0 phq9low_anypolactive 1  phq9med_anypolactive 0 phq9hi_anypolactive 0
simqi, ev genev(phqloanypolhi)

***hihi***
setx phq9med 1 phq9hi 0 phq9low_anypolactive 0  phq9med_anypolactive 1 phq9hi_anypolactive 0
simqi, ev genev(phqhianypolhi)



drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo* 
 

*****3-WAY INTERACTION MODELS*******

****TABLE A.4, MODEL 3 (FIGURE A.3; RIGHT-HAND PANEL): SUPPORT CAPITOL RIOT DV (JANUARY)***

estsimp regress supportstormers01 phq9med phq9hi anypolactive misperceptionssum12 phq9med_anypolactive phq9hi_anypolactive phq9med_fn12 phq9hi_fn12 phq9low_anypolactive_fn12 phq9med_anypolactive_fn12 phq9hi_anypolactive_fn12 dem rep ind libcon interest black white asian hispanic income_r edu_r male age socialmediaelecnewssum10 el_conf trumpsupporter [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r  3.29 interest 3.29 male .48 trumpsupporter .34

***NOTE: sequence is phq9 (depression) - anypolactive (participatory inclination) - fn12 (conspiracy beliefs); so, for example, "lolohi" = low depression, low participatory inclination, high conspiracy beliefs***

***note that "hi" for depression in this set of simulations is actually medium depression (just didn't change the nomenclature for the simulations)***

***lololo***
setx  phq9med 0 phq9hi 0 anypolactive 0 misperceptionssum12 0 phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0

simqi, ev genev(lololo)

***lolohi***
setx  phq9med 0 phq9hi 0 anypolactive 0 misperceptionssum12 .42  phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0

simqi, ev genev(lolohi)

***lohilo***
setx  phq9med 0 phq9hi 0 anypolactive 1 misperceptionssum12 0 phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0 

simqi, ev genev(lohilo) 

***hilolo***
setx  phq9med 1 phq9hi 0 anypolactive 0 misperceptionssum12 0 phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0
 
simqi, ev genev(hilolo)

***hihilo******
setx  phq9med 1 phq9hi 0 anypolactive 1 misperceptionssum12 0 phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 1 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0

simqi, ev genev(hihilo)

 ***hilohi***
setx  phq9med 1 phq9hi 0 anypolactive 0 misperceptionssum12 .42 phq9med_fn12 .42 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0
 
simqi, ev genev(hilohi)

***lohihi***
setx  phq9med 0 phq9hi 0 anypolactive 1 misperceptionssum12 .42 phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 .42 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0

simqi, ev genev(lohihi)

***hihihi***
setx  phq9med 1 phq9hi 0 anypolactive 1 misperceptionssum12 .42 phq9med_fn12 .42 phq9hi_fn12 0 phq9med_anypolactive 1 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 .42 phq9hi_anypolactive_fn12 0

simqi, ev genev(hihihi)

drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop lolo* lohi* hilo* hihi* 
 

*********NOVEMBER DATA*********

use "November Data File (for replication)_recoded.dta",clear

************HYPOTHETICAL ELECTION VIOLENCE (NOVEMBER) DV*************

*****TABLE A.4, MODEL 4 (FIGURE A.2; TOP LEFT-HAND PANEL): CONSPIRACY BELIEFS INTERACTION MODELS*******

estsimp ologit violence051 phq9med phq9hi phq9low_fn phq9med_fn phq9hi_fn  anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r age el_conf trumpsupporter male  [pw=weight_nat]

setx dem .37 rep .29 ind .28 interest 3.35 black .12 white .65 asian  .06 hispanic .14 income_r 4.02 el_conf  2.8 libcon 3.99 age  46.76 edu_r 2.99 male .47 trumpsupporter .36 anypolactive .31

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

***note that "hi" for depression in this set of simulations is actually medium depression (just didn't change the nomenclature for the simulations)***


***lolo***
setx  phq9med 0 phq9hi 0 phq9low_fn 0 phq9med_fn 0 phq9hi_fn 0 

simqi, prval(1) genpr(phqlofnlo1)

***hilo***
setx  phq9med 1 phq9hi 0 phq9low_fn 0 phq9med_fn 0 phq9hi_fn 0 
simqi, prval(1) genpr(phqhifnlo1)

***lohi***
 setx  phq9med 0 phq9hi 0 phq9low_fn .42 phq9med_fn 0 phq9hi_fn 0  

simqi, prval(1) genpr(phqlofnhi1)

***hihi***
 setx  phq9med 1 phq9hi 0 phq9low_fn 0 phq9med_fn .42 phq9hi_fn 0 
 
simqi, prval(1) genpr(phqhifnhi1)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo* 





*****TABLE A.4, MODEL 5 (FIGURE A.2; BOTTOM LEFT-HAND PANEL): PARTICIPATORY INCLINATION INTERACTION MODELS*******

estsimp ologit violence051 phq9med phq9hi phq9low_anypolactive phq9med_anypolactive phq9hi_anypolactive misperceptionssum12 dem rep ind libcon interest black white asian hispanic income_r edu_r age el_conf trumpsupporter male  [pw=weight_nat]

setx dem .37 rep .29 ind .28 interest 3.35 black .12 white .65 asian  .06 hispanic .14 income_r 4.02 el_conf  2.8 libcon 3.99 age  46.76 edu_r 2.99 male .47 trumpsupporter .36 misperceptionssum12 .21

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

***note that "hi" for depression in this set of simulations is actually medium depression (just didn't change the nomenclature for the simulations)***

***lolo***
setx phq9med 0 phq9hi 0 phq9low_anypolactive 0  phq9med_anypolactive 0 phq9hi_anypolactive 0

simqi, prval(1) genpr(phqloanypollo1)

***hilo***
setx phq9med 0 phq9hi 1 phq9low_anypolactive 0  phq9med_anypolactive 0 phq9hi_anypolactive 0
simqi, prval(1) genpr(phqhianypollo1)

***lohi***
setx phq9med 0 phq9hi 0 phq9low_anypolactive 1  phq9med_anypolactive 0 phq9hi_anypolactive 0
simqi, prval(1) genpr(phqloanypolhi1)

***hihi***
setx phq9med 0 phq9hi 1 phq9low_anypolactive 0  phq9med_anypolactive 0 phq9hi_anypolactive 1
simqi, prval(1) genpr(phqhianypolhi1)

drop phqhi* phqlo* 
drop b1* b2* b3* b4* b5* b6* b7* b8* b9*




***NOVEMBER***

use "November Data File (for replication)_recoded.dta",clear


****TABLE A.4, MODEL 6 (FIGURE A.3; LEFT-HAND PANEL): HYPOTHETICAL ELECTION VIOLENCE (NOVEMBER))***

estsimp ologit violence051 phq9med phq9hi anypolactive misperceptionssum12 phq9med_fn phq9hi_fn phq9med_anypolactive phq9hi_anypolactive phq9low_anypolactive_fn phq9med_anypolactive_fn phq9hi_anypolactive_fn  libcon interest black white asian hispanic income_r age male edu_r el_conf trumpsupporter dem rep ind  [pw=weight_nat]

setx libcon 4.0 interest 3.34 black .12 white .65 asian .06 hispanic .14 income_r 4.01 age 46.6 edu_r 2.99 el_conf 2.79 male .47 trumpsupporter .36 dem .37 rep .29 ind .28

***NOTE: sequence is phq9 (depression) - anypolactive (participatory inclination) - fn12 (conspiracy beliefs); so, for example, "lolohi" = low depression, low participatory inclination, high conspiracy beliefs***

***note that "hi" for depression in this set of simulations is actually medium depression (just didn't change the nomenclature for the simulations)***

***lololo***
setx  phq9med 0 phq9hi 0 anypolactive 0 misperceptionssum12 0 phq9med_fn 0 phq9hi_fn 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn 0 phq9med_anypolactive_fn 0 phq9hi_anypolactive_fn 0

simqi, prval(1) genpr(lololo1)


***lolohi***
setx  phq9med 0 phq9hi 0 anypolactive 0 misperceptionssum12 .45  phq9med_fn 0 phq9hi_fn 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn 0 phq9med_anypolactive_fn 0 phq9hi_anypolactive_fn 0

simqi, prval(1) genpr(lolohi1)

***lohilo***
setx  phq9med 0 phq9hi 0 anypolactive 1 misperceptionssum12 0 phq9med_fn 0 phq9hi_fn 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn 0 phq9med_anypolactive_fn 0 phq9hi_anypolactive_fn 0 

simqi, prval(1) genpr(lohilo1)

***hilolo***
setx  phq9med 1 phq9hi 0 anypolactive 0 misperceptionssum12 0 phq9med_fn 0 phq9hi_fn 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn 0 phq9med_anypolactive_fn 0 phq9hi_anypolactive_fn 0
 
simqi, prval(1) genpr(hilolo1)

***hihilo******
setx  phq9med 1 phq9hi 0 anypolactive 1 misperceptionssum12 0 phq9med_fn 0 phq9hi_fn 0 phq9med_anypolactive 1 phq9hi_anypolactive 0 phq9low_anypolactive_fn 0 phq9med_anypolactive_fn 0 phq9hi_anypolactive_fn 0

simqi, prval(1) genpr(hihilo1)

 ***hilohi***
setx  phq9med 1 phq9hi 0 anypolactive 0 misperceptionssum12 .45 phq9med_fn .45 phq9hi_fn 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn 0 phq9med_anypolactive_fn 0 phq9hi_anypolactive_fn 0
 
simqi, prval(1) genpr(hilohi1)

***lohihi***
setx  phq9med 0 phq9hi 0 anypolactive 1 misperceptionssum12 .45 phq9med_fn 0 phq9hi_fn 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn .45 phq9med_anypolactive_fn 0 phq9hi_anypolactive_fn 0

simqi, prval(1) genpr(lohihi1)

***hihihi***
setx  phq9med 1 phq9hi 0 anypolactive 1 misperceptionssum12 .45 phq9med_fn .45 phq9hi_fn 0 phq9med_anypolactive 1 phq9hi_anypolactive 0 phq9low_anypolactive_fn 0 phq9med_anypolactive_fn .45 phq9hi_anypolactive_fn 0

simqi, prval(1) genpr(hihihi1)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop lolo* lohi* hilo* hihi* 

 


***********JANUARY PANEL******************

use "January Panel Data File (for replication)_recoded.dta",clear


************HYPOTHETICAL ELECTION VIOLENCE (JANUARY) DV MODELS*************

*****TABLE A.4, MODEL 7 (FIGURE A.2; TOP CENTER PANEL): CONSPIRACY BELIEFS INTERACTION MODELS*******

estsimp ologit violence051 phq9med phq9hi phq9low_fn12 phq9med_fn12 phq9hi_fn12  anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r age el_conf trumpsupporter male socialmediaelecnewssum10 [pw=weight_nat]

setx interest 3.29 black .12 white .63 asian .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r  3.29 male .48 trumpsupporter .34 dem .39 rep .30 ind .27

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

***note that "hi" for depression in this set of simulations is actually medium depression (just didn't change the nomenclature for the simulations)***

***lolo***
setx  phq9med 0 phq9hi 0 phq9low_fn12 0 phq9med_fn12 0 phq9hi_fn12 0 

simqi, prval(1) genpr(phqlofnlo1)

***hilo***
setx  phq9med 1 phq9hi 0 phq9low_fn12 0 phq9med_fn12 0 phq9hi_fn12 0 
simqi, prval(1) genpr(phqhifnlo1)

***lohi***
 setx  phq9med 0 phq9hi 0 phq9low_fn12 .42 phq9med_fn12 0 phq9hi_fn12 0  

simqi, prval(1) genpr(phqlofnhi1)

***hihi***
 setx  phq9med 1 phq9hi 0 phq9low_fn12 0 phq9med_fn12 .42 phq9hi_fn12 0 
 
simqi, prval(1) genpr(phqhifnhi1)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo* 



*****TABLE A.4, MODEL 8 (FIGURE A.2; BOTTOM CENTER PANEL): PARTICIPATORY INCLINATION INTERACTION MODELS*******

estsimp ologit violence051 phq9med phq9hi phq9low_anypolactive phq9med_anypolactive phq9hi_anypolactive misperceptionssum12 dem rep ind libcon interest black white asian hispanic income_r edu_r age el_conf trumpsupporter male socialmediaelecnewssum10 [pw=weight_nat]

setx interest 3.29 black .12 white .63 asian .06 hispanic .16 income_r 3.92 el_conf  2.84 libcon 4.01 age  47.92 edu_r  3.29 male .48 trumpsupporter .34 dem .39 rep .30 ind .27

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

***note that "hi" for depression in this set of simulations is actually medium depression (just didn't change the nomenclature for the simulations)***

***lolo***
setx phq9med 0 phq9hi 0 phq9low_anypolactive 0  phq9med_anypolactive 0 phq9hi_anypolactive 0

simqi, prval(1) genpr(phqloanypollo1)

***hilo***
setx phq9med 0 phq9hi 1 phq9low_anypolactive 0  phq9med_anypolactive 0 phq9hi_anypolactive 0
simqi, prval(1) genpr(phqhianypollo1)

***lohi***
setx phq9med 0 phq9hi 0 phq9low_anypolactive 1  phq9med_anypolactive 0 phq9hi_anypolactive 0
simqi, prval(1) genpr(phqloanypolhi1)

***hihi***
setx phq9med 0 phq9hi 1 phq9low_anypolactive 0  phq9med_anypolactive 0 phq9hi_anypolactive 1
simqi, prval(1) genpr(phqhianypolhi1)


drop phqhi* phqlo* 
drop b1* b2* b3* b4* b5* b6* b7* b8* b9*



****TABLE A.4, MODEL 9 (FIGURE A.3; CENTER PANEL): HYPOTHETICAL ELECTION VIOLENCE (JANUARY)***

estsimp ologit violence051 phq9med phq9hi anypolactive misperceptionssum12 phq9med_anypolactive phq9hi_anypolactive phq9med_fn12 phq9hi_fn12 phq9low_anypolactive_fn12 phq9med_anypolactive_fn12 phq9hi_anypolactive_fn12  libcon interest black white asian hispanic income_r edu_r male age socialmediaelecnewssum10 el_conf trumpsupporter dem rep ind [pw=weight_nat]

setx interest 3.29 black .12 white .63 asian .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r  3.29 male .48 trumpsupporter .34 dem .39 rep .30 ind .27

***NOTE: sequence is phq9 (depression) - anypolactive (participatory inclination) - fn12 (conspiracy beliefs); so, for example, "lolohi" = low depression, low participatory inclination, high conspiracy beliefs***

***note that "hi" for depression in this set of simulations is actually medium depression (just didn't change the nomenclature for the simulations)***

***lololo***
setx  phq9med 0 phq9hi 0 anypolactive 0 misperceptionssum12 0 phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0

simqi, prval(1) genpr(lololo1)


***lolohi***
setx  phq9med 0 phq9hi 0 anypolactive 0 misperceptionssum12 .42  phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0

simqi, prval(1) genpr(lolohi1)

***lohilo***
setx  phq9med 0 phq9hi 0 anypolactive 1 misperceptionssum12 0 phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0 

simqi, prval(1) genpr(lohilo1)

***hilolo***
setx  phq9med 1 phq9hi 0 anypolactive 0 misperceptionssum12 0 phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0
 
simqi, prval(1) genpr(hilolo1)

***hihilo******
setx  phq9med 1 phq9hi 0 anypolactive 1 misperceptionssum12 0 phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 1 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0

simqi, prval(1) genpr(hihilo1)

 ***hilohi***
setx  phq9med 1 phq9hi 0 anypolactive 0 misperceptionssum12 .42 phq9med_fn12 .42 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0
 
simqi, prval(1) genpr(hilohi1)

***lohihi***
setx  phq9med 0 phq9hi 0 anypolactive 1 misperceptionssum12 .42 phq9med_fn12 0 phq9hi_fn12 0 phq9med_anypolactive 0 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 .42 phq9med_anypolactive_fn12 0 phq9hi_anypolactive_fn12 0

simqi, prval(1) genpr(lohihi1)

***hihihi***
setx  phq9med 1 phq9hi 0 anypolactive 1 misperceptionssum12 .42 phq9med_fn12 .42 phq9hi_fn12 0 phq9med_anypolactive 1 phq9hi_anypolactive 0 phq9low_anypolactive_fn12 0 phq9med_anypolactive_fn12 .42 phq9hi_anypolactive_fn12 0

simqi, prval(1) genpr(hihihi1)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop lolo* lohi* hilo* hihi* 
