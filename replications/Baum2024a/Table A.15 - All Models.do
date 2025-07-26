 
***TO RUN REMOVE THE "*" BEFORE "CD" AND INSERT THE NAME OF THE DIRECTORY WITH THE RECODED DATA.

*cd "..."

********************FEMALE ONLY*************************

********JANAURY PANEL MODELS WITH SUPPORT CAPITOL RIOT DV********

use "January Panel Data File (for replication)_recoded.dta",clear


*****TABLE A.15, MODEL 1 (FIGURE A.12; BOTTOM RIGHT-HAND PANEL): PARTICIPATORY INCLINATION INTERACTION MODELS (JANUARY)*******

estsimp regress supportstormers01 phq9 misperceptionssum12 anypolactive phq9_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r  age socialmediaelecnewssum10 el_conf trumpsupporter if male==0 [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 trumpsupporter .34 misperceptionssum12 .19

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


***hihi vs. hilo*
gen diffhilovshihi=phqhianypolhi-phqhianypollo
sumqi diffhilovshihi, level(99)
sumqi diffhilovshihi, level(95)
sumqi diffhilovshihi, level(90)

***hihi vs. lohi***
gen difflohivshihi=phqhianypolhi-phqloanypolhi
sumqi difflohivshihi, level(99)
sumqi difflohivshihi, level(95)
sumqi difflohivshihi, level(90)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo* 
drop diff*



*****TABLE A.15, MODEL 2 (FIGURE A.12; TOP RIGHT-HAND PANEL): CONSPIRACY BELIEFS INTERACTION MODELS (JANUARY)*******

estsimp regress supportstormers01 phq9 misperceptionssum12 phq9_fn12 anypolactive  dem rep ind libcon interest black white asian hispanic income_r edu_r age  socialmediaelecnewssum10 el_conf trumpsupporter if male==0 [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 trumpsupporter .34 anypolactive .27

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

***hihi vs. hilo***
gen diffhilovshihi=phqhifnhi-phqhifnlo
sumqi diffhilovshihi, level(99)
sumqi diffhilovshihi, level(95)
sumqi diffhilovshihi, level(90)

***hihi vs. lohi***
gen difflohivshihi=phqhifnhi-phqlofnhi
sumqi difflohivshihi, level(99)
sumqi difflohivshihi, level(95)
sumqi difflohivshihi, level(90)

drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo*
drop diff*



****TABLE A.15, MODEL 3 (FIGURE A.13; RIGHT-HAND PANEL): SUPPORT CAPITOL RIOT (JANUARY PANEL)***

estsimp regress supportstormers01 phq9 anypolactive misperceptionssum12 phq9_anypolactive phq9_fn12 phq9_fn12_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r age socialmediaelecnewssum10 el_conf trumpsupporter if male==0 [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r  3.29 interest 3.29  trumpsupporter .34

***sequence=phq9-anypolactive-fn12***

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

***hilohi vs. hihihi***
gen dfhilohiVShihihi=hihihi-hilohi
sumqi dfhilohiVShihihi, level(99)
sumqi dfhilohiVShihihi, level(95)
sumqi dfhilohiVShihihi, level(90)

***hihilo vs. hihihi***
gen dfhihiloVShihihi=hihihi-hihilo
sumqi dfhihiloVShihihi, level(99)
sumqi dfhihiloVShihihi, level(95)
sumqi dfhihiloVShihihi, level(90)

drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop dfhi*
drop lolo* lohi* hilo* hihi*




*********************NOVEMBER DATA SET**********************

use "November Data File (for replication)_recoded.dta",clear

************HYPOTHETICAL ELECTION VIOLENCE MODELS*************


*****TABLE A.15, MODEL 4 (FIGURE A.14; TOP RIGHT-HAND PANEL): CONSPIRACY BELIEFS INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 phq9_fn anypolactive  dem rep ind libcon interest black white asian hispanic income_r edu_r age el_conf trumpsupporter if male==0 [pw=weight_nat]

setx dem .37 rep .29 ind .28 interest 3.35 black .12 white .65 asian  .06 hispanic .14 income_r 4.02 el_conf  2.8 libcon 3.99 age  46.76 edu_r 2.99  trumpsupporter .36 anypolactive .31


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


***hihi vs. hilo***
gen diffhilovshihi=phqhifnhi-phqhifnlo
sumqi diffhilovshihi, level(99)
sumqi diffhilovshihi, level(95)
sumqi diffhilovshihi, level(90)

***hihi vs. lohi***
gen difflohivshihi=phqhifnhi-phqlofnhi
sumqi difflohivshihi, level(99)
sumqi difflohivshihi, level(95)
sumqi difflohivshihi, level(90)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo*
drop diff*



*****TABLE A.15, MODEL 5 (FIGURE A.14; BOTTOM RIGHT-HAND PANEL): PARTICIPATORY INCLINATION INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 anypolactive phq9_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r  age el_conf trumpsupporter if male==0 [pw=weight_nat]

setx dem .37 rep .29 ind .28 interest 3.35 black .12 white .65 asian  .06 hispanic .14 income_r 4.02 el_conf  2.8 libcon 3.99 age  46.76 edu_r 2.99  trumpsupporter .36 misperceptionssum12 .21

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

sumqi phqloanypollo, level(99)
sumqi phqloanypolhi, level(99)
sumqi phqhianypollo, level(99)
sumqi phqhianypolhi, level(99) 

***hihi vs. hilo***
gen diffhilovshihi=phqhianypolhi-phqhianypollo
sumqi diffhilovshihi, level(99)
sumqi diffhilovshihi, level(95)
sumqi diffhilovshihi, level(90)

***hihi vs. lohi***
gen difflohivshihi=phqhianypolhi-phqloanypolhi
sumqi difflohivshihi, level(99)
sumqi difflohivshihi, level(95)
sumqi difflohivshihi, level(90)

drop phqhi* phqlo* 
drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop diff*




****TABLE A.15, MODEL 6 (FIGURE A.15; RIGHT-HAND PANEL): HYPOTHETICAL ELECTION VIOLENCE (NOVEMBER)***

estsimp ologit violence051  phq9 anypolactive misperceptionssum12 phq9_anypolactive phq9_fn phq9_fn_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r age  el_conf trumpsupporter if male==0 [pw=weight_nat]


setx dem .37 rep .29 ind .28 libcon 4.0 interest 3.34 black .12 white .65 asian .06 hispanic .14 income_r 4.01 age 46.6 edu_r 2.99 el_conf 2.79  trumpsupporter .36

***sequence=phq9-anypolactiVe-fn***


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

***hihilo vs. hihihi***
gen dfhihiloVShihihi1=hihihi1-hihilo1
sumqi dfhihiloVShihihi1, level(99)
sumqi dfhihiloVShihihi1, level(95)
sumqi dfhihiloVShihihi1, level(90)

***hilohi vs. hihihi***
gen dfhilohiVShihihi1=hihihi1-hilohi1
sumqi dfhilohiVShihihi1, level(99)
sumqi dfhilohiVShihihi1, level(95)
sumqi dfhilohiVShihihi1, level(90)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop dfhi*
drop lolo* lohi* hilo* hihi*

 