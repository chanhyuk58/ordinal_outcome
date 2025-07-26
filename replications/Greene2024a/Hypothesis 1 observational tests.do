**********************************************************************************
**********************************************************************************
* All observational tests of Hypothesis 1   								     *  
*																			     *
* 1. Figure 3 and Appendix G,1, pp. 9-13 using Mexico 2018 Elections and Quality *
*    of Democracy (EQD) Panel survey 										     *
* 2. Extra test using Comparative National Election Project (CNEP) 			     *
*    Mexico 2012 post-election survey in Appendix G,2, pp. 10-11   			     *	
* 3. Extra test using Comparative Study of Electoral Systems (CSES) 		     *
*    Mexico 2003 survey in Appendix G,3, pp. 11-12 								 *
* 4. Extra test using State of Mexico 2017 Gubernatorial Preelection survey      *
**********************************************************************************
**********************************************************************************

******************************************
* 1. Figure 3 and Appendix G,1, pp. 9-10 *
******************************************

*********************************************************************
* Data Source: Greene, Kenneth and Alberto Simpser.                 *
* Mexico 2018 Elections and Quality of Democracy (EQD) Panel survey *
*********************************************************************

*********************************************
* Further details in Appendix G,1, pp. 9-10 *
*********************************************

* load needed packages
ssc install cem
ssc install mplotoffset
ssc install mediation
ssc install addplot

* Load data
cd "[your file path here]"
use "mx_2018_eqd_stata12.dta", clear

* Open log
log using "Figure3_Appendix_G3.log", replace

* Code for variables *
gen educ=D4
replace educ=8 if D4==8 | D4==9
replace educ=9 if D4==10
replace educ=10 if D4==11
replace educ=11 if D4>=12
recode educ (1=0) (2=1) (3=2) (4=3) (5=4) (6=5) (7=6) (8=7) (9=8) (10=9) (11=10)

gen female = Q1
recode female (1=0) (2=1)

gen ses=D12
recode ses (1=0) (2=1) (3=2) (4=3) (5=4) (98/99=.)

gen panftw1=T_Q9_5
replace panftw1=. if T_Q9_5==99

gen priftw1=T_Q9_6
replace priftw1=. if T_Q9_6==99

gen morenaftw1=T_Q9_7
replace morenaftw1=. if T_Q9_7==99

gen prdftw1=T_Q9_8
replace prdftw1=. if T_Q9_8==99

gen priftw1minusavg=priftw1-(panftw1+prdftw1+morenaftw1)/3

gen munvotetodosw1=0
replace munvotetodosw1=1 if Q8==2 | Q8==5 | Q8==8 | Q8==12 
replace munvotetodosw1 =. if Q8==99

gen munvotetodosw2=0
replace munvotetodosw2=1 if Q8A_w2==2 | Q8A_w2==5 | Q8A_w2==8 | Q8A_w2==12 
replace munvotetodosw2 =. if Q8A_w2==99

gen pri_vb_main_w1=0
replace pri_vb_main_w1=1 if (Q32B==2 & Q32A1_O1~=2 & Q32A2_O1~=2 & Q32A3_O1~=2) | ((Q32A1_O1==2 & Q32A1_O2==-1) | (Q32A2_O1==2 & Q32A2_O2==-1) | (Q32A3_O1==2 & Q32A3_O2==-1))
replace pri_vb_main_w1=. if (Q32B==98 & Q32B==99) | ((Q31A==8 | Q31A==. | Q31A==-1) & (Q31C==8 | Q31C==. | Q31C==-1) & (Q31D==8 | Q31D==. | Q31D==-1)) | ((Q31A==1 | Q31C==1 | Q31D==1) & ((Q32A1_O1 == -1 | Q32A1_O1 == 98) & (Q32A2_O1 == -1 | Q32A2_O1 == 98) & (Q32A3_O1 == -1 | Q32A3_O1 == 98)))

gen pri_vb_main_w2=0
replace pri_vb_main_w2=1 if (Q32B_w2==2 & Q32A1_O1_w2~=2 & Q32A2_O1_w2~=2 & Q32A3_O1_w2~=2) | ((Q32A1_O1_w2==2 & Q32A1_O2_w2==-1) | (Q32A2_O1_w2==2 & Q32A2_O2_w2==-1) | (Q32A3_O1_w2==2 & Q32A3_O2_w2==-1))
replace pri_vb_main_w2=. if (Q32B_w2==98 & Q32B_w2==99) | ((Q31A_w2==8 | Q31A_w2==. | Q31A_w2==-1) & (Q31C_w2==8 | Q31C_w2==. | Q31C_w2==-1) & (Q31D_w2==8 | Q31D_w2==. | Q31D_w2==-1)) | ((Q31A_w2==1 | Q31C_w2==1 | Q31D_w2==1) & ((Q32A1_O1_w2 == -1 | Q32A1_O1_w2 == 98) & (Q32A2_O1_w2 == -1 | Q32A2_O1_w2 == 98) & (Q32A3_O1_w2 == -1 | Q32A3_O1_w2 == 98)))

** Pre-process data using Coarsened-Exact Matching **
* Shown in Appendix G, 1, p. 9
imb educ female ses if pri_vb_main_w2~=., treatment(pri_vb_main_w2)
cem educ female ses if pri_vb_main_w2~=., treatment(pri_vb_main_w2)

** Model **
* Table A21 in Appendix G, 1, p. 9 
logit munvotetodosw2 munvotetodosw1 i.pri_vb_main_w2##c.priftw1minusavg educ female ses agew2 [iweight=cem_weight] if pri_vb_main_w1~=1

* Margins for p. 17 in the main text *
margins, dydx(pri_vb_main_w2) at(priftw1minusavg=(10))
margins, dydx(pri_vb_main_w2) at(priftw1minusavg=(-3))
margins, dydx(pri_vb_main_w2) at(priftw1minusavg=(-10))

* Figure 3 in main text *
* note that Stata doesn't permit truncating axes, so the code below truncates the errors bars to ensure they remain in 0 to 1. The recodes of pri_vb_main_w2 are for the horizontal axis of the figure only and act as jitter *
set graphics off
margins, at(pri_vb_main_w2=(0 1) priftw1minusavg=(-10 -3 10)) saving(Fig3file_temp, replace)
mplotoffset, scheme(s1mono) saving(Fig3plot_temp, replace)
use Fig3file_temp, clear
gen _ci_lb2= cond(_ci_lb <0, 0, _ci_lb)
gen _ci_ub2= cond(_ci_ub >1, 1, _ci_ub)
gen pri_vb_main_w2=-0.05 if _at==1
replace pri_vb_main_w2=0 if _at==2
replace pri_vb_main_w2=0.05 if _at==3
replace pri_vb_main_w2=0.95 if _at==4
replace pri_vb_main_w2=1 if _at==5
replace pri_vb_main_w2=1.05 if _at==6

twoway (rcap _ci_lb2 _ci_ub2 pri_vb_main_w2, sort pstyle(ci) ylabel(0(.2)1, nogrid) xlabel(0 "No" 1 "Yes") lpattern("1") lcolor(black) graphregion(color(white)) bgcolor(white)) (connected _margin pri_vb_main_w2, lpattern("1") lcolor(black) msymbol(O) mcolor(black)) if _at3==-10, xtitle("Received a Vote-Buying Offer from the PRI") ytitle("Probability of Voting for PRI Candidate") 

addplot: (rcap _ci_lb2 _ci_ub2 pri_vb_main_w2, sort pstyle(ci)  xlabel(0 "No" 1 "Yes") lpattern("1") lcolor(black)) (connected _margin pri_vb_main_w2, lpattern(dash) lcolor(black) msymbol(Sh) mcolor(black)) if _at3==-3

addplot: (rcap _ci_lb2 _ci_ub2 pri_vb_main_w2, sort pstyle(ci)  xlabel(0 "No" 1 "Yes") lpattern("1") lcolor(black)) (connected _margin pri_vb_main_w2, lpattern(shortdash) lcolor(black) msymbol(T) mcolor(black) legend(order(2 "Strongly opposed" 4 "Weakly opposed" 6 "Supporter") cols(3) rows(1))) if _at3==10

graph export Figure3.pdf, replace

set graphics on
log close

*****************************************************
* 2. Extra test of Hyp 1 in Appendix G,2, pp. 10-11 *
*****************************************************

*************************************************************
* Data Source: Comparative National Election Project (CNEP) *
* Mexico 2012 post-election survey						    *
* https://u.osu.edu/cnep/surveys/surveys-through-2012/		*
*************************************************************

**********************************************
* Further details in Appendix G,2, pp. 10-11 *
**********************************************

* Load data
cd "[your file path here]"
use "CN4Mexico2012_partial.dta", clear

* Open log
log using "Appendix_G2.log", replace

* Code variables *
* Q103. Recibió usted algún regalo u obsequio de algún candidato o partido durante las elecciones (SÍ) ¿De qué candidato o partido?
* 1. Vázquez Mota / PAN
* 2. Peña Nieto / PRI-PVEM
* 3. López Obrador / PRD-PT-Movimiento Ciudadano 4. Gabriel Quadri / PANAL
* 5. No recibió (PASE A 106)
* 6. NS/NC (PASE A 106)

gen vb3 = p103
recode vb3 (5=0) (6=.)
label define candidates1 1 "JVM" 2 "EPN" 3 "AMLO" 4 "GQT"
label values vb3 candidates1 

gen vb_epn=0
replace vb_epn=1 if vb3==2
replace vb_epn=. if vb3==.

* Q6. (BOLETA PRESIDENTE) Podría por favor marcar en esta boleta, ¿por quién votó usted para Presidente de la República?
* 1. Vázquez Mota/PAN
* 2. Peña Nieto/PRI-PVEM
* 3. López Obrador/PRD-PT-Movimiento Ciudadano 4. Gabriel Quadri/Nueva Alianza
* 5. Candidato no registrado
* 6. No sabe/ No recuerda
* 7. Anuló su voto
* 8. Ninguno / No votó

gen pres_vote_epn=0
replace pres_vote_epn=1 if p6==2 | p6==4 
replace pres_vote_epn=. if p6==0 | p6==.

gen pri_distance = abs(p74b-p73)
replace pri_distance=. if p74b==. | p73==.

gen pri_distance_rev = 9-pri_distance

gen age=pc

gen female=L_Gender
recode female (2=1) (1=0)

gen married=0
replace married=1 if L_Married==2
replace married=. if L_Married==.

** Model **
* Table A23 in Appendix G, 2, p. 10
logit pres_vote_epn i.vb_epn##c.pri_distance_rev L_Education age female L_Income L_Househead married K_PressVote1_F

** Margins and figure **
* Table A24 in Appendix G, 2, p. 10
margins, at(vb_epn=(0 1) pri_distance_rev=(0 4 9)) 
set graphics off
* Figure in Appendix G, 2, p. 11
mplotoffset, scheme(s1mono) title("") ytitle("Probability of Voting for PRI Candidate") xtitle("Received a Vote-Buying Offer from the PRI") 

graph export Appendix_G2.pdf, replace

set graphics on
log close

*****************************************************
* 3. Extra test of Hyp 1 in Appendix G,3, pp. 11-12 *
*****************************************************

**************************************************************
* Data Source: Comparative Study of Electoral Systems (CSES) *
* Mexico 2003 post-election survey						     *
* https://cses.org/											 *
**************************************************************

**********************************************
* Further details in Appendix G,3, pp. 11-12 *
**********************************************

* Load data
cd "[your file path here]"
use "CSES_2003_partial.DTA", clear

* Open log
log using "Appendix_G3.log", replace 

* Code variables *
gen vb_pan=P56_2A
recode vb_pan (0=0) (1=1) (2=0)

gen vb_pri=.
replace vb_pri=1 if P56_2B==1 
replace vb_pri=0 if P56_2B~=1 

gen vb_influence=P56_3
recode vb_influence (0=.) (1=1) (2=0) (8=.) (9=.)

gen pid_pan_cont=0
replace pid_pan_cont=1 if (P18A_1==1 | P18B==1 | P18D==1) & P18E==3 
replace pid_pan_cont=2 if (P18A_1==1 | P18B==1 | P18D==1) & P18E==2
replace pid_pan_cont=3 if (P18A_1==1 | P18B==1 | P18D==1) & P18E==1
replace pid_pan_cont=. if P18A_1==98 | P18A_1==99

gen pid_pri_cont=0
replace pid_pri_cont=1 if (P18A_1==2 | P18B==2 | P18D==2) & P18E==3 
replace pid_pri_cont=2 if (P18A_1==2 | P18B==2 | P18D==2) & P18E==2
replace pid_pri_cont=3 if (P18A_1==2 | P18B==2 | P18D==2) & P18E==1
replace pid_pri_cont=. if P18A_1==98 | P18A_1==99

gen female=S1
recode female (1=0) (2=1)

gen educ=S3-1
recode educ (97=.) (98=.)

** PAN model **
* Table A25 in Appendix G, 3, p. 11-12
reg vb_influence i.vb_pan##c.pid_pan_cont female  educ  

** PAN margins and figure **
* Table A26 in Appendix G, 3, p. 2
margins, at(vb_pan=(0 1) pid_pan_cont=(0 3))
set graphics off
* Figure A4 Panel A in Appendix G, 3, p. 11
mplotoffset, scheme(s1mono) title("") ytitle("Probability of Voting for PAN Candidate") xtitle("Received a Vote-Buying Offer from the PAN") 

** PRI model **
* Table A27 in Appendix G, 3, p. 12
reg vb_influence i.vb_pri##c.pid_pri_cont female  educ  

** PRI margins and figure **
* Table A28 in Appendix G, 3, p. 12
margins, at(vb_pri=(0 1) pid_pri_cont=(0  3))
* Figure A4 Panel B in Appendix G, 3, p. 11
mplotoffset, scheme(s1mono) title("") ytitle("Probability of Voting for PRI Candidate") xtitle("Received a Vote-Buying Offer from the PRI") 

graph export Appendix_G3.pdf, replace

set graphics on
log close

*****************************************************
* 4. Extra test of Hyp 1 in Appendix G,4, pp. 12-13 *
*****************************************************

**************************************************************
* Data Source: Buendia y Laredo		 						 *
* State of Mexico 2017 Gubernatorial Preelection survey      *
**************************************************************

**********************************************
* Further details in Appendix G,4, pp. 12-13 *
**********************************************

* Load data
cd "[your file path here]"
use "BL_Edomex_2017.dta", clear

* Open log
log using "Appendix_G4.log", replace 

* Code varianbles *
gen prift=p5_7
recode prift (5=4) (4=3) (3=2) (2=1) (1=0) (8/9=.) 

gen morenaft=p5_3
recode morenaft (5=4) (4=3) (3=2) (2=1) (1=0) (8/9=.) 

gen panft=p5_6
recode panft (5=4) (4=3) (3=2) (2=1) (1=0) (8/9=.) 

gen prdft=p5_8
recode prdft (5=4) (4=3) (3=2) (2=1) (1=0) (8/9=.) 

gen priloyal=.
replace priloyal=1 if prift>panft & prift>prdft & prift>morenaft
replace priloyal=0 if prift<=panft | prift<=morenaft | prift<=prdft

gen priswing=.
replace priswing=1 if prift==panft | prift==morenaft | prift==prdft
replace priswing=0 if prift>panft | prift>prdft | prift>morenaft | prift<panft | prift<prdft | prift<morenaft

gen priopp=.
replace priopp=1 if prift<panft | prift<prdft | prift<morenaft
replace priopp=0 if prift>=panft | prift>=morenaft | prift>=prdft

gen pri_voter_type = .
replace pri_voter_type=0 if priopp==1
replace pri_voter_type=1 if priswing==1
replace pri_voter_type=2 if priloyal==1

gen morenaloyal=.
replace morenaloyal=1 if morenaft>prift & morenaft>prdft & morenaft>panft
replace morenaloyal=0 if morenaft<=prift | morenaft<=panft | morenaft<=prdft

gen morenaswing=.
replace morenaswing=1 if morenaft==prift | morenaft==panft | morenaft==prdft
replace morenaswing=0 if morenaft>prift | morenaft>prdft | morenaft>panft | morenaft<prift | morenaft<prdft | morenaft<panft

gen morenaopp=.
replace morenaopp=1 if morenaft<prift | morenaft<prdft | morenaft<panft
replace morenaopp=0 if morenaft>=prift | morenaft>=panft | morenaft>=prdft

gen morena_voter_type = .
replace morena_voter_type=0 if morenaopp==1
replace morena_voter_type=1 if morenaswing==1
replace morena_voter_type=2 if morenaloyal==1

gen obligpri=extra1_1
recode obligpri (5=0) (4=1) (3=2) (2=3) (1=4) (8/9=.)

gen obligmorena=extra1_3
recode obligmorena (5=0) (4=1) (3=2) (2=3) (1=4) (8/9=.)

gen female = s1
recode female (2=1) (1=0)

gen age = s2

gen joblevel=s6
recode joblevel (-1=0) (1/2=1) (6=1) (3=2) (4/5=3) (7=2) (9=0)

** PRI model **
* Table A29 in Appendix G, 4, p. 13
reg obligpri pri_voter_type female age escol joblevel  

** PRI margin and figure **
* not shown 
margins, at(pri_voter_type=(0 1 2))

set graphics off
* Figure A5 Panel A in Appendix G, 4, p. 12
mplotoffset, scheme(s1mono) title("") ytitle("Felt Obligation to Vote for PRI Candidate for 500 Pesos") xtitle("Voter Type With Respect to PRI") 

graph export Appendix_G2_pri.pdf, replace

** MORENA model **
* Table A30 in Appendix G, 4, p. 13
reg obligmorena morena_voter_type female age escol joblevel  

** MORENA margin and figure **
* not shown
margins, at(morena_voter_type=(0 1 2))

* Figure A5 Panel B in Appendix G, 4, p. 12
mplotoffset, scheme(s1mono) title("") ytitle("Felt Obligation to Vote for MORENA Candidate for 500 Pesos") xtitle("Voter Type With Respect to MORENA") 

graph export Appendix_G2_morena.pdf, replace

set graphics on
log close

