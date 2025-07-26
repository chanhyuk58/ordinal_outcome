***TO RUN REMOVE THE "*" BEFORE "CD" AND INSERT THE NAME OF THE DIRECTORY WITH THE RECODED DATA.

*cd "..."


**************JANUARY************

use "January Panel Data File (for replication)_recoded.dta",clear

************HYPOTHETICAL ELECTION VIOLENCE DV (JANUARY) MODELS*************

*****TABLE A.9, MODEL 1 (FIGURE 7; TOP RIGHT-HAND PANEL): CONSPIRACY BELIEFS INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 phq9_fn12 anypolactive  dem rep ind libcon interest black white asian hispanic income_r edu_r age  socialmediaelecnewssum10 el_conf trumpsupporter if male==0 [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 trumpsupporter .34 anypolactive .27

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


*****TABLE A.9, MODEL 2 (FIGURE 7; BOTTOM RIGHT-HAND MODEL): PARTICIPATORY INCLINATION INTERACTION MODELS*******

estsimp ologit violence051 phq9 misperceptionssum12 anypolactive phq9_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r  age socialmediaelecnewssum10 el_conf trumpsupporter if male==0 [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r 4.37 interest 3.29 trumpsupporter .34 misperceptionssum12 .19

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


drop b1* b2* b3* b4* b5* b6* b7* b8* b9*
drop phqhi* phqlo* 
drop diff*




****TABLE A.9, MODEL 3 (FIGURE 8; RIGHT-HAND PANEL): HYPOTHETICAL ELECTION VIOLENCE (JANUARY PANEL)***

estsimp ologit violence051 phq9 anypolactive misperceptionssum12 phq9_anypolactive phq9_fn12 phq9_fn12_anypolactive dem rep ind libcon interest black white asian hispanic income_r edu_r age socialmediaelecnewssum10 el_conf trumpsupporter if male==0 [pw=weight_nat]

setx dem .39 rep .30 ind .27 interest 3.29 black .12 white .63 asian  .06 hispanic .16 income_r 3.92 socialmediaelecnewssum10 .47 el_conf  2.84 libcon 4.01 age  47.92 edu_r  3.29 interest 3.29  trumpsupporter .34

***sequence=phq9-anypolactie-fn12***

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

