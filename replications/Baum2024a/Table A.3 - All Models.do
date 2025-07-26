***TO RUN REMOVE THE "*" BEFORE "CD" AND INSERT THE NAME OF THE DIRECTORY WITH THE RECODED DATA.

*cd "..."


**********************JANUARY PANEL************************

use "January Panel Data File (for replication)_recoded.dta",clear

*****TABLE A.3, MODEL 1 (FIGURE 1; TOP RIGHT-HAND PANEL): CONSPIRACY BELIEFS INTERACTION MODELS*******

estsimp regress supportstormers01 phq9 misperceptionssum12 phq9_fn12 anypolactive  dem rep ind libcon interest black white asian hispanic income_r edu_r age  socialmediaelecnewssum10 el_conf male trumpsupporter [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 male .48 trumpsupporter .34 anypolactive .27

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

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




********TABLE A.3, MODEL 2 (FIGURE 1; BOTTOM RIGHT-HAND PANEL): PARTICIPATORY INCLINATION INTERACTION MODELS*******

estsimp regress supportstormers01 phq9 misperceptionssum12 anypolactive phq9_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r  age socialmediaelecnewssum10 el_conf trumpsupporter male  [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 male .48 trumpsupporter .34 misperceptionssum12 .19

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

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


***TABLE A.3, MODEL 3 (FIGURE 2; RIGHT-HAND PANEL): *SUPPORT CAPITOL RIOT***

estsimp regress supportstormers01 phq9 anypolactive misperceptionssum12 phq9_anypolactive phq9_fn12 phq9_fn12_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r male age socialmediaelecnewssum10 el_conf trumpsupporter [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r  3.29 interest 3.29 male .48 trumpsupporter .34

***sequence = phq9, then anypolactive, then fn12; so, for example, lohilo = low depression, high participatory inclination, low conspiracy beliefs***

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






*********************NOVEMBER DATA**********************

use "November Data File (for replication)_recoded.dta",clear


************HYPOTHETICAL VIOLENCE MODELS*************

*****TABLE A.3, MODEL 4 (FIGURE 1; TOP LEFT-HAND PANEL): CONSPIRACY BELIEFS INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 phq9_fn anypolactive  dem rep ind libcon interest black white asian hispanic income_r edu_r age el_conf male trumpsupporter [pw=weight_nat]

setx dem .37 rep .29 ind .28 interest 3.35 black .12 white .65 asian  .06 hispanic .14 income_r 4.02 el_conf  2.8 libcon 3.99 age  46.76 edu_r 2.99 male .47 trumpsupporter .36 anypolactive .31

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

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


sumqi phqlofnlo, level(99)
sumqi phqlofnhi, level(99)
sumqi phqhifnlo, level(99)
sumqi phqhifnhi, level(99) 


***effect of misperceptions WITHOUT depression*
gen difflolovslohi=phqlofnhi-phqlofnlo
sumqi difflolovslohi, level(99)
sumqi difflolovslohi, level(95)
sumqi difflolovslohi, level(90)

***effect of misperceptions WITH depression*
gen diffhilovshihi=phqhifnhi-phqhifnlo
sumqi diffhilovshihi, level(99)
sumqi diffhilovshihi, level(95)
sumqi diffhilovshihi, level(90)



drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo*
drop diff*




*****TABLE A.3, MODEL 5 (FIGURE 1; BOTTOM LEFT-HAND PANEL): PARTICIPATORY INCLINATION INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 anypolactive phq9_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r  age el_conf trumpsupporter male  [pw=weight_nat]

setx dem .37 rep .29 ind .28 interest 3.35 black .12 white .65 asian  .06 hispanic .14 income_r 4.02 el_conf  2.8 libcon 3.99 age  46.76 edu_r 2.99 male .47 trumpsupporter .36 misperceptionssum12 .21

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

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



****TABLE A.3, MODEL 6 (FIGURE 2; LEFT-HAND PANEL): HYPOTHETICAL ELECTION VIOLENCE (NOVEMBER)***

estsimp ologit violence051  phq9 anypolactive misperceptionssum12 phq9_anypolactive phq9_fn phq9_fn_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r male age  el_conf trumpsupporter [pw=weight_nat]


setx dem .37 rep .29 ind .28 libcon 4.0 interest 3.34 black .12 white .65 asian .06 hispanic .14 income_r 4.01 age 46.6 edu_r 2.99 el_conf 2.79 male .47 trumpsupporter .36

***sequence = phq9, then anypolactive, then fn12; so, for example, lohilo = low depression, high participatory inclination, low conspiracy beliefs***


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
 

 

**************JANUARY PANEL DATA**********************

use "January Panel Data File (for replication)_recoded.dta",clear


************HYPOTHETICAL VIOLENCE (JANUARY) MODELS*************

*****TABLE A.3, MODEL 7 (FIGURE 1; TOP CENTER PANEL): CONSPIRACY BELIEFS INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 phq9_fn12 anypolactive  dem rep ind libcon interest black white asian hispanic income_r edu_r age  socialmediaelecnewssum10 el_conf male trumpsupporter [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 male .48 trumpsupporter .34 anypolactive .27

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

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


sumqi phqlofnlo, level(99)
sumqi phqlofnhi, level(99)
sumqi phqhifnlo, level(99)
sumqi phqhifnhi, level(99) 


***effect of misperceptions WITHOUT depression*
gen difflolovslohi=phqlofnhi-phqlofnlo
sumqi difflolovslohi, level(99)
sumqi difflolovslohi, level(95)
sumqi difflolovslohi, level(90)

***effect of misperceptions WITH depression*
gen diffhilovshihi=phqhifnhi-phqhifnlo
sumqi diffhilovshihi, level(99)
sumqi diffhilovshihi, level(95)
sumqi diffhilovshihi, level(90)



drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo*
drop diff*






*****TABLE A.3, MODEL 8 (FIGURE 1; BOTTOM CENTER PANEL): PARTICIPATORY INCLINATION INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 anypolactive phq9_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r  age socialmediaelecnewssum10 el_conf trumpsupporter male  [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 male .48 trumpsupporter .34 misperceptionssum12 .19

***NOTE: lolo = low depression, low conspiracy beliefs; hilo = high depression, high conspiracy beliefs, etc.***

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

sumqi phqloanypollo, level(99)
sumqi phqloanypolhi, level(99)
sumqi phqhianypollo, level(99)
sumqi phqhianypolhi, level(99) 


***effect of political activity WITHOUT depression*
gen difflolovslohi=phqloanypolhi-phqloanypollo
sumqi difflolovslohi, level(99)
sumqi difflolovslohi, level(95)
sumqi difflolovslohi, level(90)

***effect of political activity WITH depression*
gen diffhilovshihi=phqhianypolhi-phqhianypollo
sumqi diffhilovshihi, level(99)
sumqi diffhilovshihi, level(95)
sumqi diffhilovshihi, level(90)

***hihi vs. lolo***
gen difflolovshihi=phqhianypolhi-phqloanypollo
sumqi difflolovshihi, level(99)
sumqi difflolovshihi, level(95)
sumqi difflolovshihi, level(90)

***hihi vs. lohi***
gen difflohivshihi=phqhianypolhi-phqloanypolhi
sumqi difflohivshihi, level(99)
sumqi difflohivshihi, level(95)
sumqi difflohivshihi, level(90)


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo* 
drop diff*




****TABLE A.3, MODEL 9 (FIGURE 2; CENTER PANEL): HYPOTHETICAL ELECTION VIOLENCE (JANUARY PANEL)***

estsimp ologit violence051 phq9 anypolactive misperceptionssum12 phq9_anypolactive phq9_fn12 phq9_fn12_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r male age socialmediaelecnewssum10 el_conf trumpsupporter [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r  3.29 interest 3.29 male .48 trumpsupporter .34

***sequence = phq9, then anypolactive, then fn12; so, for example, lohilo = low depression, high participatory inclination, low conspiracy beliefs***

***lololo***
setx  phq9 0 anypolactive 0 misperceptionssum12 0 phq9_anypolactive 0 phq9_fn12 0  phq9_fn12_anypolactive 0

simqi, prval(0) genpr(lololo0)
simqi, prval(.5) genpr(lololo5)
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



