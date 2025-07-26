***************WAVE 13/NOVEMBER DATA FILE RECODES*****************
clear

set more off

ssc install clarify

***DOWNLOAD THE NOVEMBER PANEL DATA FILE (FOR REPLICATION) FROM DATAVERSE AND SET UP THE WORKING DIRECTORY WHERE IT WILL BE STORED. KEEP ALL THE DATA IN THIS DIRECTORY.

***TO RUN REMOVE THE "*" BEFORE "CD" AND INSERT THE NAME OF THE DIRECTORY WITH THE DATA.

*cd "..."

use "November Data File (for replication).dta",clear



******DEPENDENT VARIABLE*******

***HYPOTHETICAL ELECTION VIOLENCE (NOVEMBER)***
***note: variable label in do file is "violence051"; in tables and in text, the variable name is "HYPOTHETICAL ELECTION VIOLENCE (NOVEMBER)"***
gen violence051=el_fair_4
recode violence051 1/2=0 3=.5 4/5=1

****INDEPENDENT VARIABLES****

***CONSPIRACY BELIEFS***
***note: variable label in do files is "misperceptionssum12" and variable name in tables and text is "Conspiracy Beliefs"***

gen olderpeople=fn_1
gen chinabioweapon=fn_3
gen bats=fn_6
gen natlemergency=fn_8
gen novaccine=fn_10
gen curewithheld=fn_11
gen fluvacriskscovid=fn_13
gen hydroxy=fn_14
gen facemaskcausecovid=fn_15

gen binladenalive=fn_17
gen ballotsindumpsters=fn_18
gen bidenprofitedfamily=fn_19

recode hydroxy 2/3=0
recode facemaskcausecovid 2/3=0
recode binladenalive 2/3=0
recode ballotsindumpsters 2/3=0
recode bidenprofitedfamily 2/3=0
recode olderpeople 2/3=0
recode chinabioweapon 2/3=0
recode bats 2/3=0
recode curewithheld 2/3=0
recode fluvacriskscovid 2/3=0
recode natlemergency 2=1 1=0 3=0
recode novaccine 2=1 1=0 3=0

gen misperceptionssum12=(hydroxy+facemaskcausecovid+binladenalive+ballotsindumpsters+bidenprofitedfamily+olderpeople+chinabioweapon+bats+curewithheld+fluvacriskscovid+natlemergency+novaccine)/11


***WAVE 13 NOVEMBER IV RECODES (USED IN BOTH NOVEMBER AND JANUARY ANALYSES)***

***TRUMP SUPPORTER***
gen trumpsupporter=voted20
recode trumpsupporter .=0
recode trumpsupporter 1=0 2=1 3/5=0
recode trumpsupporter 0=1 if support20==2

***DEPRESSION***
gen phq9=((phq9_1+phq9_2+phq9_3+phq9_4+phq9_5+phq9_6+phq9_7+phq9_8+phq9_9)-9)

***DEMOCRAT, REPUBLICAN and INDEPENDENT***
gen dem=party
gen rep=party
gen ind=party
recode rep 2/99999=0
recode dem 1=0 2=1 3/999999=0
recode ind 1/2=0 3=1 4/99999=0

***GENDER***
gen male=gender_r
recode male 2=0

***7-point ideology scale, running from liberal (1) to conservative (7): variable name in tables is IDEOLOGY***
gen libcon=ideology

***RACE/ETHNICITY***
gen hispanic=race_r
recode hispanic 1=0 2=1 3/9999999=0
gen black=race_r
recode black 1/2=0 3=1 4/9999999=0
gen asian=race_r
recode asian 1/3=0 4=1 5/99999=0
gen white=race_r
recode white 2/9999999=0

***PARTICIPATORY INCLINATION***
***note: variable label in do files is "anypolactive" and variable name in tables and text is "Participatory Inclination"***

gen polactive1=pol_par_1
gen polactive2=pol_par_2
gen polactive3=pol_par_3
gen polactive4=pol_par_4
gen polactive5=pol_par_5
gen polactive6=pol_par_6


recode polactive1 .=0
recode polactive2 .=0
recode polactive3 .=0
recode polactive4 .=0
recode polactive5 .=0
recode polactive6 .=0


gen polactivesum6=polactive1+polactive2+polactive3+polactive4+polactive5+polactive6

gen anypolactive=polactivesum6
recode anypolactive 2/6=1

****INDEPENDENT VARIABLES's THAT DO NOT REQUIRE RECODING***

***el_conf (election confidence)***
***edu_r (education)***
***age (age, up to 99)***
***income_r (income)***
***interest (interest in politics/public affairs)



********INTERACTION TERMS**********

***NOTE: "fn" stands for "fake news" and is the shorthand for the misperceptionssum12 variable in interaction terms. In Wave 13 it is "fn" and in the January wave it is "fn12" in all interaction terms.***

***misperceptions x depression x participatory inclination interactions***
gen phq9_fn=phq9*misperceptionssum12
gen phq9_anypolactive=phq9*anypolactive
gen phq9_fn_anypolactive=phq9*anypolactive*misperceptionssum12

***depression variable for moderate depression robustness tests***
gen phq9low=phq9
recode phq9low 0/4=1 5/27=0
gen phq9med=phq9
recode phq9med 1/4=0 5/14=1 15/27=0
gen phq9hi=phq9
recode phq9hi 1/14=0 15/27=1

***interaction terms for moderate depression robustness tests***
gen phq9low_anypolactive=phq9low*anypolactive
gen phq9med_anypolactive=phq9med*anypolactive
gen phq9hi_anypolactive=phq9hi*anypolactive
gen phq9low_fn=phq9low*misperceptionssum12
gen phq9med_fn=phq9med*misperceptionssum12
gen phq9hi_fn=phq9hi*misperceptionssum12
gen phq9low_anypolactive_fn=phq9low*misperceptionssum12*anypolactive
gen phq9med_anypolactive_fn=phq9med*misperceptionssum12*anypolactive
gen phq9hi_anypolactive_fn=phq9hi*misperceptionssum12*anypolactive


***SAVING RECODED DATA FOR ANALYSES
save "November Data File (for replication)_recoded.dta", replace

