
***TO RUN REMOVE THE "*" BEFORE "CD" AND INSERT THE NAME OF THE DIRECTORY WITH THE RECODED DATA.

*cd "..."

*********************NOVEMBER DATA**********************

use "November Data File (for replication)_recoded.dta",clear

************HYPOTHETICAL ELECTION VIOLENCE (NOVEMBER) DV*************

*****TABLE A.6, MODEL 1 (FIGURE 3; TOP RIGHT-HAND PANEL): CONSPIRACY BELIEFS INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 phq9_fn anypolactive   libcon interest black white asian hispanic income_r edu_r age el_conf male trumpsupporter if rep==1 [pw=weight_nat]

setx  interest 3.35 black .12 white .65 asian  .06 hispanic .14 income_r 4.02 el_conf  2.8 libcon 3.99 age  46.76 edu_r 2.99 male .47 trumpsupporter .36 anypolactive .31

***NOTE: sequence is phq9 (depression) - fn12 (conspiracy beliefs); so, for example, "lohi" = lo depression, high conspiracy beliefs***

***lolo***
setx  phq9 0 misperceptionssum12 0 phq9_fn 0 

simqi, prval(1) genpr(phqlofnlo)

***hilo***
setx  phq9 18 misperceptionssum12 0 phq9_fn 0 
simqi, prval(1) genpr(phqhifnlo)

***lohi***
 setx  phq9 0 misperceptionssum12 .45 phq9_fn 0  

simqi, prval(1) genpr(phqlofnhi)

***hihi***
setx  phq9 18 misperceptionssum12 .45 phq9_fn .45*18
 
simqi, prval(1) genpr(phqhifnhi)

drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo*




*****TABLE A.6, MODEL 2 (FIGURE 3; BOTTOM RIGHT-HAND PANEL): PARTICIPATORY INCLINATION INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 anypolactive phq9_anypolactive  libcon interest black white asian hispanic income_r edu_r  age el_conf trumpsupporter male if rep==1  [pw=weight_nat]

setx  interest 3.35 black .12 white .65 asian  .06 hispanic .14 income_r 4.02 el_conf  2.8 libcon 3.99 age  46.76 edu_r 2.99 male .47 trumpsupporter .36 misperceptionssum12 .21

***NOTE: sequence is phq9 (depression) - fn12 (conspiracy beliefs); so, for example, "lohi" = lo depression, high conspiracy beliefs***

***lolo***
setx  phq9 0 anypolactive 0 phq9_anypolactive 0

simqi, prval(1) genpr(phqloanypollo)

***hilo***
setx  phq9 18 anypolactive 0 phq9_anypolactive 0
simqi, prval(1) genpr(phqhianypollo)

***lohi***
setx  phq9 0 anypolactive 1 phq9_anypolactive 0

simqi, prval(1) genpr(phqloanypolhi)

***hihi***
setx  phq9 18 anypolactive 1 phq9_anypolactive 18
 
simqi, prval(1) genpr(phqhianypolhi)

drop phqhi* phqlo* 
drop b1* b2* b3* b4* b5* b6* b7* b8* b9*



 
****TABLE A.6, MODEL 3 (FIGURE 4; RIGHT-HAND PANEL): HYPOTHETICAL ELECTION VIOLENCE (NOVEMBER) DV***

estsimp ologit violence051  phq9 anypolactive misperceptionssum12 phq9_anypolactive phq9_fn phq9_fn_anypolactive libcon interest black white asian hispanic income_r edu_r male age  el_conf trumpsupporter if rep==1 [pw=weight_nat]


setx libcon 4.0 interest 3.34 black .12 white .65 asian .06 hispanic .14 income_r 4.01 age 46.6 edu_r 2.99 el_conf 2.79 male .47 trumpsupporter .36

***NOTE: sequence is phq9 (depression) - anypolactive (participatory inclination) - fn12 (conspiracy beliefs); so, for example, "lolohi" = low depression, low participatory inclination, high conspiracy beliefs***


***lololo***
setx  phq9 0 anypolactive 0 misperceptionssum12 0 phq9_anypolactive 0 phq9_fn 0  phq9_fn_anypolactive 0

simqi, prval(1) genpr(lololo1)


***lolohi***
setx  phq9 0 anypolactive 0 misperceptionssum12 .45 phq9_anypolactive 0 phq9_fn 0  phq9_fn_anypolactive 0

simqi, prval(1) genpr(lolohi1)

***lohilo***
setx  phq9 0 anypolactive 1 misperceptionssum12 0 phq9_anypolactive 0 phq9_fn 0  phq9_fn_anypolactive 0

simqi, prval(1) genpr(lohilo1)

***hilolo***
setx  phq9 18 anypolactive 0 misperceptionssum12 0 phq9_anypolactive 0 phq9_fn 0  phq9_fn_anypolactive 0
 
simqi, prval(1) genpr(hilolo1)

***hihilo******
setx  phq9 18 anypolactive 1 misperceptionssum12 0 phq9_anypolactive 18 phq9_fn 0  phq9_fn_anypolactive 0

simqi, prval(1) genpr(hihilo1)

 ***hilohi***
setx  phq9 18 anypolactive 0 misperceptionssum12 .45 phq9_anypolactive 0 phq9_fn 18*.45  phq9_fn_anypolactive 0
 
simqi, prval(1) genpr(hilohi1)

***lohihi***
setx  phq9 0 anypolactive 1 misperceptionssum12 .45 phq9_anypolactive 0 phq9_fn 0  phq9_fn_anypolactive 0

simqi, prval(1) genpr(lohihi1)


***hihihi***
setx  phq9 18 anypolactive 1 misperceptionssum12 .45 phq9_anypolactive 18 phq9_fn .45*18  phq9_fn_anypolactive .45*18

simqi, prval(1) genpr(hihihi1)

drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop lolo* lohi* hilo* hihi*

 
