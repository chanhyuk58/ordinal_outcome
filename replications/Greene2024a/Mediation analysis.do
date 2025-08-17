******************************************
******* TABLE 2: MEDIATION ANALYSIS ******
******************************************

*********************************************************************
* Data Source: Greene, Kenneth and Alberto Simpser.                 *
* Mexico 2018 Elections and Quality of Democracy (EQD) Panel survey *
*********************************************************************

****************************************************
* Further details in Appendix Section I, pp. 16-17 *
****************************************************

* load needed packages
ssc install mediation
net install moremata.pkg

* Load data *
cd "[your file path here]"
use "mx_2018_eqd_stata12.dta", clear

* Open log
log using "Table2.log", replace

* Code variables *
gen ce_choice=.
replace ce_choice=0 if Q27A==2
replace ce_choice=1 if Q27A==1

gen ce_ft = .
replace ce_ft=T_Q27B_1
replace ce_ft=. if T_Q27B_1==99

gen ce_pri=.
replace ce_pri=0 if c12~=1 
replace ce_pri=1 if c12==1 

gen ce_pan=.
replace ce_pan=0 if c12~=2 
replace ce_pan=1 if c12==2 

gen ce_prd=.
replace ce_prd=0 if c12~=3 
replace ce_prd=1 if c12==3 

gen ce_morena=.
replace ce_morena=0 if c12~=4 
replace ce_morena=1 if c12==4 

gen ce_leftcolpriw1=.
replace ce_leftcolpriw1=0 if c12~=1
replace ce_leftcolpriw1=1 if c12==1

gen ce_leftcolpanw1=.
replace ce_leftcolpanw1=0 if c12~=2
replace ce_leftcolpanw1=1 if c12==2

gen ce_leftcolprdw1=.
replace ce_leftcolprdw1=0 if c12~=3
replace ce_leftcolprdw1=1 if c12==3

gen ce_leftcolmorenaw1=.
replace ce_leftcolmorenaw1=0 if c12~=4
replace ce_leftcolmorenaw1=1 if c12==4

gen panftw1=T_Q9_5
replace panftw1=. if T_Q9_5==99

gen priftw1=T_Q9_6
replace priftw1=. if T_Q9_6==99

gen morenaftw1=T_Q9_7
replace morenaftw1=. if T_Q9_7==99

gen prdftw1=T_Q9_8
replace prdftw1=. if T_Q9_8==99

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

gen reciprocityw1=Q26E
recode reciprocityw1 (1=3) (2=2) (3=1) (4=0) (8=.) (9=.)

gen ce_leftcol_payoff_w1=c15
recode ce_leftcol_payoff_w1 (1=0) (2=1) (3=1)

gen ce_thankful_w1=T_Q27C_1
recode ce_thankful_w1 (1=3) (2=2) (3=1) (4=0) (5=.)

gen ce_offended_w1=T_Q27C_2
recode ce_offended_w1 (1=4) (2=3) (3=2) (4=0) (5=.)

gen ce_threat_w1=T_Q27C_3
recode ce_threat_w1 (1=4) (2=3) (3=2) (4=0) (5=.)

gen panftw1_others = panftw1-(priftw1+prdftw1+morenaftw1)
gen priftw1_others = priftw1-(panftw1+prdftw1+morenaftw1)
gen prdftw1_others = prdftw1-(panftw1+priftw1+morenaftw1)
gen morenaftw1_others = morenaftw1-(panftw1+priftw1+prdftw1)

gen ce_leftcol_ft_w1others=.
replace ce_leftcol_ft_w1others=priftw1_others if ce_leftcolpriw1==1
replace ce_leftcol_ft_w1others=panftw1_others if ce_leftcolpanw1==1
replace ce_leftcol_ft_w1others=prdftw1_others if ce_leftcolprdw1==1  
replace ce_leftcol_ft_w1others=morenaftw1_others if ce_leftcolmorenaw1==1

gen treat_v2=ce_leftcol_ft_w1others*ce_leftcol_payoff_w1

gen treat_v2a=.
replace treat_v2a=0 if treat_v2<0
replace treat_v2a=1 if treat_v2>=0

** MODELS **

* Table 2, Columns 1 and 2 (Gratitude models) in the main text *
medeff (regress ce_thankful_w1 treat_v2 reciprocityw1 ses educ female) (regress ce_ft treat_v2 ce_thankful_w1 reciprocityw1 ses educ female), sims(1000) seed(1) vce(cluster SbjNum) mediate(ce_thankful_w1) treat(treat_v2 -10 10)

* Appendix Section I,1, p. 16 - Gratitude sensitivity analysis *
medsens (regress ce_thankful_w1 treat_v2a reciprocityw1 ses educ female) (regress ce_ft treat_v2a ce_thankful_w1 reciprocityw1 ses educ female), eps(.01) med(ce_thankful_w1) treat(treat_v2a) sims(1000)

set graphics off

* Appendix Section I,1, p. 16 *
twoway rarea _med_updelta0 _med_lodelta0 _med_rho, bcolor(gs14) || line _med_delta0 _med_rho , lcolor(black) ytitle("Average mediation effect") xtitle("Sensitivity parameter: p") legend(off) title("ACME(p)") scheme(s1mono)

graph export Appendix_I_gratitude.pdf, replace

* Table 2, Columns 3 and 4 (Offense models) in the main text *
medeff (regress ce_offended_w1 treat_v2 reciprocityw1 ses educ female) (regress ce_ft treat_v2 ce_offended_w1 reciprocityw1 ses educ female), sims(1000) seed(1) vce(cluster SbjNum) mediate(ce_offended_w1) treat(treat_v2 -10 10)

* Appendix I,2, p. 17 - Offense sensitivity analysis *
medsens (regress ce_offended_w1 treat_v2a reciprocityw1 ses educ female) (regress ce_ft treat_v2a ce_offended_w1 reciprocityw1 ses educ female), eps(.01) med(ce_offended_w1) treat(treat_v2a) sims(1000)

* Appendix I,2, p. 17
twoway rarea _med_updelta0 _med_lodelta0 _med_rho, bcolor(gs14) || line _med_delta0 _med_rho , lcolor(black) ytitle("Average mediation effect") xtitle("Sensitivity parameter: p") legend(off) title("ACME(p)") scheme(s1mono)

graph export Appendix_I_offense.pdf, replace 

* Table 2, Columns 5 and 6 (Reciprocity models) in the main text *
medeff (regress reciprocityw1 treat_v2  ses educ female) (regress ce_ft treat_v2 reciprocityw1  ses educ female), sims(1000) seed(1) vce(cluster SbjNum) mediate(reciprocityw1) treat(treat_v2 -10 10)

* Table 2, Columns 7 and 8 (Threat models) in the main text *
medeff (regress ce_threat_w1 treat_v2 reciprocityw1 ses educ female) (regress ce_ft treat_v2 ce_threat_w1 reciprocityw1 ses educ female), sims(1000) seed(1) vce(cluster SbjNum) mediate(ce_threat_w1) treat(treat_v2 -10 10)

set graphics on
log close

