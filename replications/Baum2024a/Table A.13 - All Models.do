
***TO RUN REMOVE THE "*" BEFORE "CD" AND INSERT THE NAME OF THE DIRECTORY WITH THE RECODED DATA.

*cd "..."

*************************************TRUMPSUPPORTERS ONLY********************************




***************JANUARY DATA****************

use "January Panel Data File (for replication)_recoded.dta",clear



*****TABLE A.13, MODEL 1 (FIGURE A.8; BOTTOM RIGHT-HAND PANEL):PARTICIPATORY INCLINATION INTERACTION MODELS*******

estsimp regress supportstormers01 phq9 misperceptionssum12 anypolactive phq9_anypolactive  libcon interest black white asian hispanic income_r edu_r  age socialmediaelecnewssum10 el_conf dem rep ind male if trumpsupporter==1  [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 male .48 dem .39 rep .30 ind .27 misperceptionssum12 .19

***lolo***
setx  phq9 0 anypolactive 0 phq9_anypolactive 0

simqi, ev genev(phqloanypollo)

***hilo***
setx  phq9 16 anypolactive 0 phq9_anypolactive 0
simqi, ev genev(phqhianypollo)

***lohi***
setx  phq9 0 anypolactive 1 phq9_anypolactive 0

simqi, ev genev(phqloanypolhi)

***hihi***
setx  phq9 16 anypolactive 1 phq9_anypolactive 16
 
simqi, ev genev(phqhianypolhi)

drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo* 



*****TABLE A.13, MODEL 2 (FIGURE A.8; TOP RIGHT-HAND PANEL): CONSPIRACY BELIEFS INTERACTION MODELS*******

estsimp regress supportstormers01 phq9 misperceptionssum12 phq9_fn12 anypolactive   libcon interest black white asian hispanic income_r edu_r age  socialmediaelecnewssum10 el_conf male dem rep ind if trumpsupporter==1 [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 male .48 dem .39 rep .30 ind .27 anypolactive .27

***lolo***
setx  phq9 0 misperceptionssum12 0 phq9_fn12 0 

simqi, ev genev(phqlofnlo)

***hilo***
setx  phq9 16 misperceptionssum12 0 phq9_fn12 0 
simqi, ev genev(phqhifnlo)

***lohi***
 setx  phq9 0 misperceptionssum12 .42 phq9_fn12 0  

simqi, ev genev(phqlofnhi)

***hihi***
setx  phq9 16 misperceptionssum12 .42 phq9_fn12 .42*16
 
simqi, ev genev(phqhifnhi)

drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo*



****TABLE A.13, MODEL 3 (FIGURE A.9; RIGHT-HAND PANEL):SUPPORT CAPITOL RIOT***

estsimp regress supportstormers01 phq9 anypolactive misperceptionssum12 phq9_anypolactive phq9_fn12 phq9_fn12_anypolactive libcon interest black white asian hispanic income_r edu_r male age socialmediaelecnewssum10 el_conf dem rep ind if trumpsupporter==1 [pw=weight_nat]

setx interest 3.29 black .12 white .63 asian .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r  3.29 interest 3.29 male .48 dem .39 rep .30 ind .27 

***sequence=phq9 (depression) -anypolactive (participatory inclination) -fn12 (conspiracy beliefs); so, for example, hilolo = high depression, low participatory inclination, low conspiracy beliefs***

***lololo***
setx  phq9 0 anypolactive 0 misperceptionssum12 0 phq9_anypolactive 0 phq9_fn12 0  phq9_fn12_anypolactive 0

simqi, ev genev(lololo)

***lolohi***
setx  phq9 0 anypolactive 0 misperceptionssum12 .42 phq9_anypolactive 0 phq9_fn12 0  phq9_fn12_anypolactive 0

simqi, ev genev(lolohi)

***lohilo***
setx  phq9 0 anypolactive 1 misperceptionssum12 0 phq9_anypolactive 0 phq9_fn12 0  phq9_fn12_anypolactive 0

simqi, ev genev(lohilo)

***hilolo***
setx  phq9 16 anypolactive 0 misperceptionssum12 0 phq9_anypolactive 0 phq9_fn12 0  phq9_fn12_anypolactive 0
 
simqi, ev genev(hilolo)

***hihilo******
setx  phq9 16 anypolactive 1 misperceptionssum12 0 phq9_anypolactive 16 phq9_fn12 0  phq9_fn12_anypolactive 0

simqi, ev genev(hihilo)

 ***hilohi***
setx  phq9 16 anypolactive 0 misperceptionssum12 .42 phq9_anypolactive 0 phq9_fn12 16*.42  phq9_fn12_anypolactive 0
 
simqi, ev genev(hilohi)

***lohihi***
setx  phq9 0 anypolactive 1 misperceptionssum12 .42 phq9_anypolactive 0 phq9_fn12 0  phq9_fn12_anypolactive 0

simqi, ev genev(lohihi)

***hihihi***
setx  phq9 16 anypolactive 1 misperceptionssum12 .42 phq9_anypolactive 16 phq9_fn12 16*.42  phq9_fn12_anypolactive .42*16


simqi, ev genev(hihihi)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop lolo* lohi* hilo* hihi*



************HYPOTHETICAL ELECTION VIOLENCE MODELS*************

*****TABLE A.13, MODEL 4 (FIGURE A.10; TOP RIGHT-HAND PANEL):CONSPIRACY BELIEFS INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 phq9_fn12 anypolactive   libcon interest black white asian hispanic income_r edu_r age  socialmediaelecnewssum10 el_conf male dem rep ind if trumpsupporter==1 [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 male .48 dem .39 rep .30 ind .27 anypolactive .27

***lolo***
setx  phq9 0 misperceptionssum12 0 phq9_fn12 0 

simqi, prval(1) genpr(phqlofnlo)

***hilo***
setx  phq9 16 misperceptionssum12 0 phq9_fn12 0 
simqi, prval(1) genpr(phqhifnlo)

***lohi***
 setx  phq9 0 misperceptionssum12 .42 phq9_fn12 0  

simqi, prval(1) genpr(phqlofnhi)

***hihi***
setx  phq9 16 misperceptionssum12 .42 phq9_fn12 .42*16
 
simqi, prval(1) genpr(phqhifnhi)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo*





*****TABLE A.13, MODEL 5 (FIGURE A.10; BOTTOM RIGHT-HAND PANEL):PARTICIPATORY INCLINATION INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 anypolactive phq9_anypolactive  libcon interest black white asian hispanic income_r edu_r  age socialmediaelecnewssum10 el_conf dem rep ind male if trumpsupporter==1  [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 male .48 dem .39 rep .30 ind .27 misperceptionssum12 .19

***lolo***
setx  phq9 0 anypolactive 0 phq9_anypolactive 0

simqi, prval(1) genpr(phqloanypollo)

***hilo***
setx  phq9 16 anypolactive 0 phq9_anypolactive 0
simqi, prval(1) genpr(phqhianypollo)

***lohi***
setx  phq9 0 anypolactive 1 phq9_anypolactive 0

simqi, prval(1) genpr(phqloanypolhi)

***hihi***
setx  phq9 16 anypolactive 1 phq9_anypolactive 16
 
simqi, prval(1) genpr(phqhianypolhi)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo* 




****TABLE A.13, MODEL 6 (FIGURE A.11; RIGHT-HAND PANEL):HYPOTHETICAL ELECTION VIOLENCE (JANUARY PANEL)***

estsimp ologit violence051 phq9 anypolactive misperceptionssum12 phq9_anypolactive phq9_fn12 phq9_fn12_anypolactive libcon interest black white asian hispanic income_r edu_r male age socialmediaelecnewssum10 el_conf dem rep ind if trumpsupporter==1 [pw=weight_nat]

setx interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r  3.29 interest 3.29 male .48 dem .39 rep .30 ind .27 

***sequence=phq9 (depression) -anypolactive (participatory inclination) -fn12 (conspiracy beliefs); so, for example, hilolo = high depression, low participatory inclination, low conspiracy beliefs***

***lololo***
setx  phq9 0 anypolactive 0 misperceptionssum12 0 phq9_anypolactive 0 phq9_fn12 0  phq9_fn12_anypolactive 0

simqi, prval(1) genpr(lololo1)


***lolohi***
setx  phq9 0 anypolactive 0 misperceptionssum12 .42 phq9_anypolactive 0 phq9_fn12 0  phq9_fn12_anypolactive 0

simqi, prval(1) genpr(lolohi1)

***lohilo***
setx  phq9 0 anypolactive 1 misperceptionssum12 0 phq9_anypolactive 0 phq9_fn12 0  phq9_fn12_anypolactive 0

simqi, prval(1) genpr(lohilo1)

***hilolo***
setx  phq9 16 anypolactive 0 misperceptionssum12 0 phq9_anypolactive 0 phq9_fn12 0  phq9_fn12_anypolactive 0
 
simqi, prval(1) genpr(hilolo1)

***hihilo******
setx  phq9 16 anypolactive 1 misperceptionssum12 0 phq9_anypolactive 16 phq9_fn12 0  phq9_fn12_anypolactive 0

simqi, prval(1) genpr(hihilo1)

 ***hilohi***
setx  phq9 16 anypolactive 0 misperceptionssum12 .42 phq9_anypolactive 0 phq9_fn12 16*.42  phq9_fn12_anypolactive 0
 
simqi, prval(1) genpr(hilohi1)

***lohihi***
setx  phq9 0 anypolactive 1 misperceptionssum12 .42 phq9_anypolactive 0 phq9_fn12 0  phq9_fn12_anypolactive 0

simqi, prval(1) genpr(lohihi1)


***hihihi***
setx  phq9 16 anypolactive 1 misperceptionssum12 .42 phq9_anypolactive 16 phq9_fn12 16*.42  phq9_fn12_anypolactive 16*.42

simqi, prval(1) genpr(hihihi1)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop lolo* lohi* hilo* hihi* 
