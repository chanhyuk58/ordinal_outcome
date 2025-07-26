
************************************************************************************************
************************************************************************************************
* All observational tests of Hypothesis 2 													   *
*																							   *
* Figure 5 in the main text and Appendix H, pp. 13-15						   			       *
* 1. Figure 5, Panel A using Mexico 2012 Panel Survey 										   *
* 2. Figure 5, Panel B using Mexico 2018 Elections and Quality of Democracy (EQD) Panel survey *
* 3. First extra test using Mexico 2010 Electoral Clientelism Survey 						   *
* 4. Second extra test using Argentina 2021 Clientelism Survey 								   *
************************************************************************************************
************************************************************************************************ 

*****************************************
* 1. Figure 5, Panel A in the main text *
*****************************************

******************************************************************************************************
* Data Source: Greene, Kenneth. Mexico Panel Study, 2012. Ann Arbor, MI: Inter-university Consortium *
* for Political and Social Research [distributor], 2016-03-11. https://doi.org/10.3886/ICPSR35024.v1 *
******************************************************************************************************

*******************************************
* Further details in Appendix H,1, p. 13 *
*******************************************

* load needed packages
ssc install cem

* Load data
cd "[your file path here]"
use "35024-0001-Data_partial.dta", clear

* Open log
log using "Figure5_PanelA_Appendix_H.log", replace 

* Code variables for use in models that produce Figure 5, Panel B *
gen dv=.
replace dv=0 if W2_P38B==2
replace dv=1 if W2_P38B==3 
replace dv=2 if W2_P38B==1

gen ses=P46
recode ses (1=5) (2=4) (3=3) (4=2) (5=1) (9=.) (-1=.)

gen educ=P44
recode educ (10=.)
recode educ (-1=.)

gen female=.
replace female=1 if P1==2
replace female=0 if P1==1
replace female=. if P1==-1

gen age=P2
recode age (-1=.)

gen prift=P16F
recode prift (-1=.) (99=.) 

gen prdft=P16G
recode prdft (-1=.) (99=.) 

gen panft=P16E
recode panft (-1=.) (99=.) 

gen prift_others=prift-(panft+prdft)/2
gen panft_others=panft-(prift+prdft)/2
gen prdft_others=prdft-(panft+prift)/2

gen vbacceptablew1=P37B
recode vbacceptablew1 (-1=.) (9=.) (1=1) (2=0) (3=.) (9=.)

gen vb_obs_w1=0
replace vb_obs_w1=1 if P40==1
replace vb_obs_w1=. if P40==-1 | P40==3 | P40==9 | P40==.

gen vb_obs_w2=0
replace vb_obs_w2=1 if W2_P41==1
replace vb_obs_w2=1 if W2_P40==1
replace vb_obs_w2=. if W2_P41==-1 | W2_P41==3 | W2_P41==9 | W2_P41==.
replace vb_obs_w2=0 if W2_P41AB=="El PAN" | W2_P41AB=="PAN" | W2_P41AB=="Los del PAN" | W2_P41AB=="PRD"| W2_P41AB=="El lider del PRD Fidel Alejandro"  

gen vb_obs_either_wave=.
replace vb_obs_either_wave=1 if vb_obs_w1==1 | vb_obs_w2==1
replace vb_obs_either_wave=0 if vb_obs_w1==0 | vb_obs_w2==0

** Pre-process data using Coarsened-Exact Matching **
* Shown in Appendix H,1, p. 13
imb educ female ses if vb_obs_either_wave~=., treatment(vb_obs_either_wave)
cem educ female ses if vb_obs_either_wave~=., treatment(vb_obs_either_wave)

** Model ** 
* Shown in Appendix H,1, p. 14
reg dv vbacceptablew1 i.vb_obs_w2##c.prift_others educ female age ses if vb_obs_w1~=1 [iweight=cem_weights] 

** Margins and plot for Figure 5, Panel A **
* * Shown in Appendix H,1, p. 14
margins, at(vb_obs_w2=(1) prift_others=(-3)) at(vb_obs_w2=(1) prift_others=(3)) at(vb_obs_w2=(1) prift_others=(10))

set graphics off

* Figure 5 Panel A in the main text *
marginsplot, plotopts(connect(i) msymbol(Sh) mcolor(black) msize(medlarge)) ciopts(lwidth(medthick) lpattern("1") lcolor(black)) plotregion(margin(large)) ylab(-1(1)3, nogrid) graphregion(color(white)) yline(0, lwidth(vthin) lpattern(-) lcolor(black)) xlabel(1 "Strongly opposed" 2 "Weakly opposed" 3 "Supporters") title("") ytitle("Perceived Legitimacy of Vote Buying") xtitle("")

graph export Figure5_PanelA.pdf, replace

** t-tests for specific margins ** 
* Repeat of model above shown in Appendix H,1, p. 14
reg dv vbacceptablew1 i.vb_obs_w2##c.prift_others educ female age ses if vb_obs_w1~=1 [iweight=cem_weights] 

* Shown in Appendix H,1, p. 14
margins, at(vb_obs_w2=(1) prift_others=(-3)) at(vb_obs_w2=(1) prift_others=(10)) post
test _b[1._at] = _b[2._at] 

* Repeat of model above shown in Appendix H,1, p. 14
reg dv vbacceptablew1 i.vb_obs_w2##c.prift_others educ female age ses if vb_obs_w1~=1 [iweight=cem_weights] 

* Shown in Appendix H,1, p. 14
margins, at(vb_obs_w2=(1) prift_others=(3)) at(vb_obs_w2=(1) prift_others=(10)) post
test _b[1._at] = _b[2._at] 

set graphics on
log close

************************
* 2. Figure 5, Panel B *
************************

*********************************************************************
* Data Source: Greene, Kenneth and Alberto Simpser.                 *
* Mexico 2018 Elections and Quality of Democracy (EQD) Panel survey *
*********************************************************************

******************************************
* Further details in Appendix H,2, p. 14 *
******************************************

* Load data
cd "[your file path here]"
use "mx_2018_eqd_stata12.dta", clear

* Open log
log using "Figure5_PanelB_Appendix_H.log", replace 

* Code variables for use in models that produce Figure 5, Panel B *
gen vblegitimatew1=Q25
recode vblegitimatew1 (4=0) (3=1) (2=2) (1=3) (98/99=.)

gen vblegitimatew2=Q25_w2
recode vblegitimatew2 (4=0) (3=1) (2=2) (1=3) (98/99=.)

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

gen direct_received_w1 = 0
replace direct_received_w1 = 1 if Q31A == 1
replace direct_received_w1 = . if (Q31A==-1 | Q31A==8 | Q31A==.) & (Q31C==-1 | Q31C==8 | Q31C==.) & (Q31D==-1 | Q31D==8 | Q31D==.)

gen direct_rejected_w1 = 0
replace direct_rejected_w1 = 1 if Q31C == 1
replace direct_rejected_w1 = . if (Q31A==-1 | Q31A==8 | Q31A==.) & (Q31C==-1 | Q31C==8 | Q31C==.) & (Q31D==-1 | Q31D==8 | Q31D==.)

gen direct_promised_w1 = 0
replace direct_promised_w1 = 1 if Q31D == 1
replace direct_promised_w1 = . if (Q31A==-1 | Q31A==8 | Q31A==.) & (Q31C==-1 | Q31C==8 | Q31C==.) & (Q31D==-1 | Q31D==8 | Q31D==.)

gen Q32A1_PRI_w1 = 0
replace Q32A1_PRI_w1 = 1 if Q32A1_O1 == 2 | Q32A1_O2 == 2
replace Q32A1_PRI_w1 = . if Q32A1_O1 == -1 | Q32A1_O1 == 98

gen Q32A2_PRI_w1 = 0
replace Q32A2_PRI_w1 = 1 if Q32A2_O1 == 2 | Q32A2_O2 == 2
replace Q32A2_PRI_w1 = . if Q32A2_O1 == -1 | Q32A2_O1 == 98

gen Q32A3_PRI_w1 = 0
replace Q32A3_PRI_w1 = 1 if Q32A3_O1 == 2 | Q32A3_O2 == 2
replace Q32A3_PRI_w1 = . if Q32A3_O1 == -1 | Q32A3_O1 == 98

gen pri_vb_main_w1=0
replace pri_vb_main_w1=1 if (Q32B==2 & Q32A1_O1~=2 & Q32A2_O1~=2 & Q32A3_O1~=2) | ((Q32A1_O1==2 & Q32A1_O2==-1) | (Q32A2_O1==2 & Q32A2_O2==-1) | (Q32A3_O1==2 & Q32A3_O2==-1))
replace pri_vb_main_w1=. if (Q32B==98 & Q32B==99) | ((Q31A==8 | Q31A==. | Q31A==-1) & (Q31C==8 | Q31C==. | Q31C==-1) & (Q31D==8 | Q31D==. | Q31D==-1)) | ((Q31A==1 | Q31C==1 | Q31D==1) & ((Q32A1_O1 == -1 | Q32A1_O1 == 98) & (Q32A2_O1 == -1 | Q32A2_O1 == 98) & (Q32A3_O1 == -1 | Q32A3_O1 == 98)))

gen direct_received_w2 = 0
replace direct_received_w2 = 1 if Q31A_w2 == 1
replace direct_received_w2 = . if (Q31A_w2==-1 | Q31A_w2==8 | Q31A_w2==.) & (Q31C_w2==-1 | Q31C_w2==8 | Q31C_w2==.) & (Q31D_w2==-1 | Q31D_w2==8 | Q31D_w2==.)

gen direct_rejected_w2 = 0
replace direct_rejected_w2 = 1 if Q31C_w2 == 1
replace direct_rejected_w2 = . if (Q31A_w2==-1 | Q31A_w2==8 | Q31A_w2==.) & (Q31C_w2==-1 | Q31C_w2==8 | Q31C_w2==.) & (Q31D_w2==-1 | Q31D_w2==8 | Q31D_w2==.)

gen direct_promised_w2 = 0
replace direct_promised_w2 = 1 if Q31D_w2 == 1
replace direct_promised_w2 = . if (Q31A_w2==-1 | Q31A_w2==8 | Q31A_w2==.) & (Q31C_w2==-1 | Q31C_w2==8 | Q31C_w2==.) & (Q31D_w2==-1 | Q31D_w2==8 | Q31D_w2==.)

gen Q32A1_PRI_w2 = 0
replace Q32A1_PRI_w2 = 1 if Q32A1_O1_w2 == 2 | Q32A1_O2_w2 == 2
replace Q32A1_PRI_w2 = . if SbjNum_w2==. | Q32A1_O1_w2 == 98 | (Q32A1_O1_w2 == -1 & (Q31A_w2==-1 | Q31A_w2==8 | Q31A_w2==.) & (Q31C_w2==-1 | Q31C_w2==8 | Q31C_w2==.) & (Q31D_w2==-1 | Q31D_w2==8 | Q31D_w2==.))

gen Q32A2_PRI_w2 = 0
replace Q32A2_PRI_w2 = 1 if Q32A2_O1_w2 == 2 | Q32A2_O2_w2 == 2
replace Q32A2_PRI_w2 = . if SbjNum_w2==. | Q32A2_O1_w2 == 98 | (Q32A2_O1_w2 == -1 & (Q31A_w2==-1 | Q31A_w2==8 | Q31A_w2==.) & (Q31C_w2==-1 | Q31C_w2==8 | Q31C_w2==.) & (Q31D_w2==-1 | Q31D_w2==8 | Q31D_w2==.))

gen Q32A3_PRI_w2 = 0
replace Q32A3_PRI_w2 = 1 if Q32A3_O1_w2 == 2 | Q32A3_O2_w2 == 2
replace Q32A3_PRI_w2 = . if SbjNum_w2==. | Q32A3_O1_w2 == 98 | (Q32A2_O1_w2 == -1 & (Q31A_w2==-1 | Q31A_w2==8 | Q31A_w2==.) & (Q31C_w2==-1 | Q31C_w2==8 | Q31C_w2==.) & (Q31D_w2==-1 | Q31D_w2==8 | Q31D_w2==.))

gen Q32A2_TPM_w2 = 0
replace Q32A2_TPM_w2 = 1 if Q32A2_O5_w2 == 11 | Q32A2_O7_w2 == 11 | Q32A2_O11_w2 == 11
replace Q32A2_TPM_w2 = . if SbjNum_w2==. | Q32A2_O1_w2 == 98 | (Q32A2_O1_w2 == -1 & (Q31A_w2==-1 | Q31A_w2==8 | Q31A_w2==.) & (Q31C_w2==-1 | Q31C_w2==8 | Q31C_w2==.) & (Q31D_w2==-1 | Q31D_w2==8 | Q31D_w2==.))

gen pri_vb_w2=0
replace pri_vb_w2=1 if Q32A1_PRI_w2==1 | Q32A2_PRI_w2==1 | Q32A3_PRI_w2==1 | Q32A2_TPM_w2==1 
replace pri_vb_w2=. if ((Q31A_w2==8 | Q31A_w2==.) & (Q31C_w2==8 | Q31C_w2==.) & (Q31D_w2==8 | Q31D_w2==.)) | ((Q31A_w2==1 | Q31C_w2==1 | Q31D_w2==1) & ((Q32A1_O1_w2 == -1 | Q32A1_O1_w2 == 98) & (Q32A2_O1_w2 == -1 | Q32A2_O1_w2 == 98) & (Q32A3_O1_w2 == -1 | Q32A3_O1_w2 == 98)))

gen pri_vb_main_w2=0
replace pri_vb_main_w2=1 if (Q32B_w2==2 & Q32A1_O1_w2~=2 & Q32A2_O1_w2~=2 & Q32A3_O1_w2~=2) | ((Q32A1_O1_w2==2 & Q32A1_O2_w2==-1) | (Q32A2_O1_w2==2 & Q32A2_O2_w2==-1) | (Q32A3_O1_w2==2 & Q32A3_O2_w2==-1))
replace pri_vb_main_w2=. if (Q32B_w2==98 & Q32B_w2==99) | ((Q31A_w2==8 | Q31A_w2==. | Q31A_w2==-1) & (Q31C_w2==8 | Q31C_w2==. | Q31C_w2==-1) & (Q31D_w2==8 | Q31D_w2==. | Q31D_w2==-1)) | ((Q31A_w2==1 | Q31C_w2==1 | Q31D_w2==1) & ((Q32A1_O1_w2 == -1 | Q32A1_O1_w2 == 98) & (Q32A2_O1_w2 == -1 | Q32A2_O1_w2 == 98) & (Q32A3_O1_w2 == -1 | Q32A3_O1_w2 == 98)))

** Pre-process data with Coarsened-Exact Matching **
* Shown in Appndix H,2, p. 14
imb female educ ses if pri_vb_w2~=., treatment(pri_vb_w2)
cem female educ ses if pri_vb_w2~=., treatment(pri_vb_w2)

** Model **
* Shown in Appndix H,2, p. 14
reg vblegitimatew2 vblegitimatew1 i.pri_vb_main_w2##c.priftw1minusavg educ ses female agew2 if pri_vb_main_w1~=1 [iweight=cem_weights]

** Margins and plot for Figure 5, Panel B **
* Shown in Appndix H,2, p. 15
margins, at(pri_vb_main_w2=(1) priftw1minusavg=(-9)) at(pri_vb_main_w2=(1) priftw1minusavg=(0)) at(pri_vb_main_w2=(1) priftw1minusavg=(10))

set graphics off

* Figure 5 Panel B in the main text
marginsplot, plotopts(connect(i) msymbol(S) mcolor(black) msize(medlarge)) ciopts(lwidth(medthick) lpattern("1") lcolor(black)) plotregion(margin(large)) ylab(-1(1)4, nogrid) graphregion(color(white)) yline(0, lwidth(vthin) lpattern(-) lcolor(black)) xlabel(1 "Strongly opposed" 2 "Weakly opposed" 3 "Supporters") title("") ytitle("Perceived Legitimacy of Vote Buying") xtitle("")

graph export Figure5_PanelB.pdf, replace

** t-tests for specific margins ** 
* Repeat of model above shown in Appndix H,2, p. 14
reg vblegitimatew2 vblegitimatew1 i.pri_vb_main_w2##c.priftw1minusavg educ ses female agew2 if pri_vb_main_w1~=1 [iweight=cem_weights]

* Shown in Appndix H,2, p. 15
margins, at(pri_vb_main_w2=(1) priftw1minusavg=(-9)) at(pri_vb_main_w2=(1) priftw1minusavg=(10)) post 
test _b[1._at] = _b[2._at] 

* Repeat of model above shown in Appndix H,2, p. 14
reg vblegitimatew2 vblegitimatew1 i.pri_vb_main_w2##c.priftw1minusavg educ ses female agew2 if pri_vb_main_w1~=1 [iweight=cem_weights]

* Shown in Appndix H,2, p. 15
margins, at(pri_vb_main_w2=(1) priftw1minusavg=(0)) at(pri_vb_main_w2=(1) priftw1minusavg=(10)) post
test _b[1._at] = _b[2._at] 

set graphics on
log close

*********************************
* 3. Extra test of Hypothesis 2 *
*********************************

******************************************************************
* Data Source: Greene, Kenneth, Chappell Lawson, and Ana de la O *
* Mexico 2010 Clientelism Survey 							     *
******************************************************************

******************************************
* Further details in Appendix H,3, p. 15 *
******************************************

* Load data
cd "[your file path here]"
use "mx_2010_clientelism", clear

* Open log
log using "Appendix_H3.log", replace 

* Code variables for use in extra models to test Hyp 2 *
gen is_vb=.
replace is_vb=1 if p16aii==1
replace is_vb=0 if p16aii==2

gen vb_pan=0
replace vb_pan=1 if p27a==1 | p27b==1 | p27c==1
replace vb_pan=. if p26==. & ((p27a==. & p27b==. & p27c==.) | (p27a==10 & p27b==10 & p27c==10) | (p27a==99 & p27b==99 & p27c==99))

gen vb_pri=0
replace vb_pri=1 if p27a==2 | p27b==2 | p27c==2
replace vb_pri=. if p26==. & ((p27a==. & p27b==. & p27c==.) | (p27a==10 & p27b==10 & p27c==10) | (p27a==99 & p27b==99 & p27c==99))

gen vb_prd=0
replace vb_prd=1 if p27a==3 | p27b==3 | p27c==3
replace vb_prd=. if p26==. & ((p27a==. & p27b==. & p27c==.) | (p27a==10 & p27b==10 & p27c==10) | (p27a==99 & p27b==99 & p27c==99))

gen pid_pan=0
replace pid_pan=1 if p3==1 | p3==5
replace pid_pan=. if p3==. | p3==99

gen pid_pri=0
replace pid_pri=1 if p3==2 | p3==6
replace pid_pri=. if p3==. | p3==99

gen pid_prd=0
replace pid_prd=1 if p3==3 | p3==7
replace pid_prd=. if p3==. | p3==99

gen pid_other=0
replace pid_other=1 if p3==4
replace pid_other=. if p3==. | p3==99

gen educ=p35
recode educ (99=.)

gen income=p36
recode income (99=.)

gen female=pa
recode female (1=0) (2=1)

gen age=pc

gen treat_support_v1=0
replace treat_support_v1=1 if (pid_pri==1 & vb_pri==1) | (pid_pan==1 & vb_pan==1) | (pid_prd==1 & vb_prd==1) 
replace treat_support_v1=. if (pid_pri==. | pid_pan==. & pid_prd==.) | p26==. 

gen treat_oppose_v3=0
replace treat_oppose_v3=1 if ((pid_pan==1 | pid_prd==1 | pid_other==1) & vb_pri==1) | ((pid_pri==1 | pid_prd==1 | pid_other==1) & vb_pan==1) | ((pid_pan==1 | pid_pri==1 | pid_other==1) & vb_prd==1) 
replace treat_oppose_v3=. if (pid_pri==. | pid_pan==. & pid_prd==.) | p26==. 

** Model **
* Shown in Appendix H,3, p. 15
* note: excluded category is weakly opposed *
logit is_vb treat_support_v1 treat_oppose_v3 income female age educ

** Margins **
* Shown in Appendix H,3, p. 15
margins, at(treat_support_v1=(0 1) treat_oppose_v3=(0 1))

** t-tests for specific margins **
* Repeat of model above shown in Appendix H,3, p. 15
logit is_vb treat_support_v1 treat_oppose_v3 income female age educ

* Shown in Appendix H,3, p. 15
margins, at(treat_support_v1=1 treat_oppose_v3=0) at(treat_support_v1=0 treat_oppose_v3=1) post
test _b[1._at] = _b[2._at] 

* Repeat of model above shown in Appendix H,3, p. 15
logit is_vb treat_support_v1 treat_oppose_v3 income female age educ

* Shown in Appendix H,3, p. 15
margins, at(treat_support_v1=1 treat_oppose_v3=0) at(treat_support_v1=0 treat_oppose_v3=0) post
test _b[1._at] = _b[2._at] 

log close
