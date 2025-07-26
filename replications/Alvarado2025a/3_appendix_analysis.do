/*==================================================
project:       Testing the Compensatory Theory: 
               A Survey Experiment on Covid-19 
			   and Redistributive Preferences in the UK
Author:        Pablo Querubin, Alan David Gomez 
E-email:       ad.gomezb@uniandes.edu.co
Dependencies:  New York University
----------------------------------------------------
Creation Date: 19 Mar 2024 - 07:23:58
Output:        Appendix figures A, B, C.2, D, E, F, G      
==================================================*/

/*==================================================
* Please note that some variables are created within 
* a preserve block. To ensure proper execution and 
* avoid potential issues, it is recommended to run 
* the code all at once
**================================================*/

/*==================================================
                 Appendix
==================================================*/

** Import replication data
** Please remember to set your own directory in the master dofile 

use "${output}uk_jul2023_rep.dta", clear

gen stdgroup=(treat==0)

* Create standardized weighted index
swindex inc_rate temp_tax wealth_tax inher_tax rem_loops other_tax corp_rate, gen(index_all) normby(stdgroup) fullrescale replace

swindex rich_comp_burden rich_comp_profit poor_comp_vat poor_comp_welf corp_comp_profit, gen(index_fair) normby(stdgroup) fullrescale replace

**# A. Balance

*** A.1 Table for covariates 
global vars1 "pre_trust_gvt gvt_role equalt male age ideology"
global vars2 "hhinc hh_received dummy_educ"
global vars3 "dum_white dum_fulltime dum_parttime"
global vars4 "dum_notlab dum_married dum_conservative"
global vars "$vars1 $vars2 $vars3 $vars4 dum_labour"

local r=0
foreach x of varlist $vars {
	local r = `r'+1
}

mat RR = J(`r',6,.)
mat SS = J(`r',6,0)

local i =1
foreach x of varlist $vars {
	reg `x' ib0.treat
	mat RR[`i',1] = r(table)[1,2]
	mat RR[`i',2] = r(table)[4,2]
	
	scalar pval1 = r(table)[4,2]
	scalar sig1 = (pval1<0.01)+(pval1<0.05)+(pval1<0.10)
	mat SS[`i',1] = sig1 
	
	mat RR[`i',3] = r(table)[1,3]
	mat RR[`i',4] = r(table)[4,3]
	
	scalar pval2 = r(table)[4,3]
	scalar sig2 = (pval2<0.01)+(pval2<0.05)+(pval2<0.10)
	mat SS[`i',3] = sig2
	
	mat RR[`i',5] = r(table)[1,4]
	mat RR[`i',6] = r(table)[4,4]
	
	scalar pval3 = r(table)[4,4]
	scalar sig3 = (pval3<0.01)+(pval3<0.05)+(pval3<0.10)
	mat SS[`i',5] = sig3
	
	local i= `i'+1
}

#d;
	frmttable using "${output}balance_tab.tex", tex frag statmat(RR) substat(1) 
			annotate(SS) asymbol(*,**,***) sdec(3) replace
			ctitles("","T1","T2","T3")
			rtitles("Trust in Goverment"\""\
					"Goverment Role"\""\
					"Equal Treatment"\""\
					"Male"\""\
					"Age"\""\
					"Ideological self-placement"\""\
					"Household Income"\""\
		"Received government assistance during pandemic"\""\
		"Education - Degree or above"\""\
		"Race - White"\""\
		"Employment Status - Full Time"\""\
		"Employment Status - Part Time"\""\
		"Employment Status - Not in Labour Force"\""\
		"Marital Status - Married"\""\
		"Party - Conservative"\""\
		"Party - Labour");
#d cr 

*** Table for Region Balance 
local r=0
foreach x of varlist dumreg_* {
	local r = `r'+1
}

mat RR = J(`r',6,.)
mat SS = J(`r',6,0)

local i =1
foreach x of varlist dumreg_* {
	reg `x' ib0.treat
	mat RR[`i',1] = r(table)[1,2]
	mat RR[`i',2] = r(table)[4,2]
	
	scalar pval1 = r(table)[4,2]
	scalar sig1 = (pval1<0.01)+(pval1<0.05)+(pval1<0.10)
	mat SS[`i',1] = sig1 
	
	mat RR[`i',3] = r(table)[1,3]
	mat RR[`i',4] = r(table)[4,3]
	
	scalar pval2 = r(table)[4,3]
	scalar sig2 = (pval2<0.01)+(pval2<0.05)+(pval2<0.10)
	mat SS[`i',3] = sig2
	
	mat RR[`i',5] = r(table)[1,4]
	mat RR[`i',6] = r(table)[4,4]
	
	scalar pval3 = r(table)[4,4]
	scalar sig3 = (pval3<0.01)+(pval3<0.05)+(pval3<0.10)
	mat SS[`i',5] = sig3
	
	local i= `i'+1
}

#d;
	frmttable using "${output}balance_reg_tab.tex", tex frag statmat(RR) substat(1) 
			annotate(SS) asymbol(*,**,***) sdec(3) replace
			ctitles("","T1","T2","T3")
			rtitles("North East"\""\
					"North West"\""\
					"Yorkshire and the Humber"\""\
					"East Midlands"\""\
					"West Midlands"\""\
					"East of England"\""\
					"Greater London"\""\
					"South East"\""\
					"South West"\""\
					"Scotland"\""\
					"Wales"\""\
					"Northern Ireland");
#d cr 

**# B. Results with all covariates

*Treatment effects
#d;
	reg index_all ib0.treat 
		pre_trust_gvt gvt_role 
		equalt male age dumreg_* 
		ideology hhinc 
		hh_received dum_white 
		dum_fulltime dum_parttime 
		dum_notlab dum_married 
		dum_conservative dum_labour 
		dummy_educ, baselevels;
#d cr 
est store i_tax

#d;
	reg index_fair ib0.treat 
		pre_trust_gvt gvt_role 
		equalt male age dumreg_* 
		ideology hhinc 
		hh_received dum_white 
		dum_fulltime dum_parttime 
		dum_notlab dum_married 
		dum_conservative dum_labour 
		dummy_educ, baselevels;
#d cr 
est store i_fair

#d;
	coefplot i_tax, bylabel("Panel A: Support for" "Increasing Taxes on Rich") 
	|| i_fair, bylabel("Panel B: Perceived Fairness" "of Compensatory Policies")  
	||, drop(pre_trust_gvt gvt_role 
		equalt male age dumreg_* 
		ideology hhinc 
		hh_received dum_white 
		dum_fulltime dum_parttime 
		dum_notlab dum_married 
		dum_conservative dum_labour 
		dummy_educ _cons) 
		base xline(0) levels(95 90) 
		aspectratio(.5) scheme(lean2);
#d cr 
graph export "${output}combined_rob.jpg", width(2000) replace

** Manipulation check
#d;
	reg manip ib0.treat 
		pre_trust_gvt gvt_role 
		equalt male age dumreg_* 
		ideology hhinc 
		hh_received dum_white 
		dum_fulltime dum_parttime 
		dum_notlab dum_married 
		dum_conservative dum_labour 
		dummy_educ, baselevels;
#d cr 
est store manipr

#d;
	coefplot (manipr, label()), 
	drop(pre_trust_gvt gvt_role 
		equalt male age dumreg_* 
		ideology hhinc 
		hh_received dum_white 
		dum_fulltime dum_parttime 
		dum_notlab dum_married 
		dum_conservative dum_labour 
		dummy_educ _cons) 
	base xline(0) title() 
	levels(95 90) scheme(lean2);
#d cr 
graph export "${output}manipr.jpg", width(2000) replace


**# C. LATEs

* C.2. Complier average treatment effects

** Complier dummy
gen complier=0
replace complier=1 if manip>3 

** Treated Dummy for IV
gen treat_iv = (treat==2 | treat==3)

*Treatment effects
preserve 
la var manip "Manip. Check"

** All
ivreg2 index_all (manip=treat_iv) male age ideology hhinc dumreg_*
est store i_all_late

ivreg2 index_fair (manip=treat_iv) male age ideology hhinc dumreg_*
est store i_fair_late

** Just Control
ivreg2 index_all (manip=treat_iv) male age ideology hhinc dumreg_* if (treat==0 | treat==2 | treat==3)
est store i_all_late_jc

ivreg2 index_fair (manip=treat_iv) male age ideology hhinc dumreg_* if (treat==0 | treat==2 | treat==3)
est store i_fair_late_jc

** Just T1
ivreg2 index_all (manip=treat_iv) male age ideology hhinc dumreg_* if (treat==1 | treat==2 | treat==3)
est store i_all_late_t1

ivreg2 index_fair (manip=treat_iv) male age ideology hhinc dumreg_* if (treat==1 | treat==2 | treat==3)
est store i_fair_late_t1

#d;
	coefplot i_all_late i_all_late_jc i_all_late_t1, 
	bylabel("Panel A: Support for" 
			"Increasing Taxes on Rich") 
	|| i_fair_late i_fair_late_jc i_fair_late_t1, 
	bylabel("Panel B: Perceived Fairness" 
			"of Compensatory Policies")  
	||, drop(age male dumreg_* ideology hhinc _cons) 
	base xline(0) levels(95 90) 
	scheme(lean2)
	aspectratio(.5)
	xtitle(, margin(b=4))
	legend(order(3 "Control Group and T1" 
				 6 "Control Group" 
				 9 "T1") rows(3) 
				 subtitle("Reference Category", size(small))
				 region(lcolor(black))
				 bmargin(t=5)
				 size(vsmall));
#d cr 
graph export "${output}late_cont.jpg", width(2000)  replace
restore


preserve 
la var complier "Dich. Manip. Check"

** All
ivreg2 index_all (complier=treat_iv) male age ideology hhinc dumreg_*
est store i_all_late

ivreg2 index_fair (complier=treat_iv) male age ideology hhinc dumreg_*
est store i_fair_late

** Just Control
ivreg2 index_all (complier=treat_iv) male age ideology hhinc dumreg_* if (treat==0 | treat==2 | treat==3)
est store i_all_late_jc

ivreg2 index_fair (complier=treat_iv) male age ideology hhinc dumreg_* if (treat==0 | treat==2 | treat==3)
est store i_fair_late_jc

** Just T1
ivreg2 index_all (complier=treat_iv) male age ideology hhinc dumreg_* if (treat==1 | treat==2 | treat==3)
est store i_all_late_t1

ivreg2 index_fair (complier=treat_iv) male age ideology hhinc dumreg_* if (treat==1 | treat==2 | treat==3)
est store i_fair_late_t1

#d;
	coefplot i_all_late i_all_late_jc i_all_late_t1, 
	bylabel("Panel A: Support for" 
			"Increasing Taxes on Rich") 
	|| i_fair_late i_fair_late_jc i_fair_late_t1, 
	bylabel("Panel B: Perceived Fairness" 
			"of Compensatory Policies")  
	||, drop(age male dumreg_* ideology hhinc _cons) 
	base xline(0) levels(95 90) 
	scheme(lean2)
	aspectratio(.5)
	xtitle(, margin(b=4))
	legend(order(3 "Control Group and T1" 
				 6 "Control Group" 
				 9 "T1") rows(3) 
				 subtitle("Reference Category", size(small))
				 region(lcolor(black))
				 bmargin(t=5)
				 size(vsmall));
#d cr 
graph export "${output}late_dich.jpg", width(2000)  replace
restore

*** Special Graph of Main Outcome
preserve
 
replace treat = . if treat==0 & complier==1
replace treat = . if treat==1 & complier==1
replace treat = . if treat==2 & complier==0
replace treat = . if treat==3 & complier==0


*Treatment effects
reg index_all ib0.treat male age dumreg_* ideology hhinc, baselevels
est store i_all2

reg index_fair ib0.treat male age dumreg_* ideology hhinc, baselevels
est store i_fairv2

#d;
	coefplot i_all2, bylabel("Panel A: Support for" "Increasing Taxes on Rich") 
	|| i_fairv2, bylabel("Panel B: Perceived Fairness" "of Compensatory Policies")  
	||, drop(age male dumreg_* ideology hhinc _cons) base xline(0) levels(95 90) 
	scheme(lean2) 
	aspectratio(.5);
#d cr 
graph export "${output}combined2_spec.jpg", replace width(2000) 

restore 


**# D. Additional outcomes

*Trust in government
reg post_trust_gvt ib0.treat male age dumreg_* ideology hhinc, baselevels
est store post_trust2

#d;
	coefplot (post_trust2, label()), 
	drop(age male dumreg_* ideology hhinc _cons) 
	base xline(0) title() levels(95 90) scheme(lean2);
#d cr 
graph export "${output}post_trust2_new.jpg", width(2000) replace

*ICW social spending
swindex ubi min_wage unemp soc_spend, gen(index_spend) normby(stdgroup) fullrescale replace

reg index_spend ib0.treat male age dumreg_* ideology hhinc, baselevels
est store i_spend2

#d;
	coefplot (i_spend2, label()), 
	drop(age male dumreg_* ideology hhinc _cons) 
	base xline(0) levels(95 90) scheme(lean2);
#d cr 
graph export "${output}index_spend2_new.jpg", width(2000) replace

*Support for taxing excess profits
reg tax_profits ib0.treat male age dumreg_* ideology hhinc, baselevels
estimates store tax_profits2

#d;
	coefplot (tax_profits2, label()), 
	drop(age male dumreg_* ideology hhinc _cons) 
	base xline(0) title() levels(95 90) scheme(lean2);
#d cr 
graph export "${output}tax_profits2_new.jpg", width(2000) replace


*Industry made too much profit
gen ex_profit= (fair_profit==2)
replace ex_profit=. if fair_profit==.

reg ex_profit i.treat male age dumreg_* ideology hhinc, baselevels
est store ex_profit

#d;
	coefplot (ex_profit, label()), 
	drop(age male dumreg_* ideology hhinc _cons) 
	base xline(0) title() levels(95 90) scheme(lean2);
#d cr 
graph export "${output}fair_profits2_new.jpg", width(2000) replace


*By attention level
reg index_all ib0.treat##i.attention male age dumreg_* ideology hhinc
margins, dydx(treat) at(attention=(0(1)3))

#d;
marginsplot,  
xtitle("Number of Correct Mock Vignette Checks") 
addplot(hist attention, discrete title( " ") 
gap(30) 
ytitle("Conditional Effect of Treatments" "on Support for Taxing the Rich") 
yaxis(2) yscale(alt axis(2)) percent 
ylabel(0 "0%" 10 "10%" 
	   20 "20%" 30 "30%"
	   40 "40%" 50 "50%" 
	   60 "60%" 70 "70%" 
	   80 "80%", 
	axis(2)) 
ytitle("Percent of Sample", 
axis(2) orientation(rvertical))  
fcolor(gs12%40) fintensity(100) 
lcolor(none)) scheme(lean2);
#d cr 
graph export "${output}int_attention2_new.jpg", width(2000) replace


**# E. Individual outcome measures

*Tax rates
reg inc_rate ib0.treat male age dumreg_* ideology hhinc, baselevels
estimates store inc_rate2

reg corp_rate ib0.treat male age dumreg_* ideology hhinc, baselevels
estimates store corp_rate2

#d;
	coefplot (inc_rate2), 
	bylabel(Preferred tax rate on rich (0-100))
	|| (corp_rate2), 
	bylabel(Preferred corporate tax rate (0-100)) 
	||, drop(age male dumreg_* ideology hhinc _cons) 
	base xline(0) levels(95 90) scheme(lean2);
#d cr 
graph export "${output}rates2_new.jpg", width(2000) replace

*Tax reforms
reg temp_tax ib0.treat male age dumreg_* ideology hhinc, baselevels
estimates store temp2
reg wealth_tax ib0.treat male age dumreg_* ideology hhinc, baselevels
estimates store wealth2
reg inher_tax ib0.treat male age dumreg_* ideology hhinc, baselevels
estimates store inher2
reg rem_loops ib0.treat male age dumreg_* ideology hhinc, baselevels
estimates store loops2
reg other_tax ib0.treat male age dumreg_* ideology hhinc, baselevels
estimates store other2

#d;
coefplot (temp2), bylabel(One-off pandemic wealth tax) 
|| (wealth2), bylabel(Permanent wealth tax) 
|| (inher2), bylabel(Higher inheritance tax rates) 
|| (loops2), bylabel(Closing loopholes) 
|| (other2), bylabel(Other reforms (open-ended)) 
||, drop(age male dumreg_* ideology hhinc _cons) 
base xline(0) levels(95 90) scheme(lean2);
#d cr 
graph export "${output}reforms2_new.jpg", width(2000) replace

*Social insurance
reg ubi ib0.treat male age dumreg_* ideology hhinc, baselevels
est store ubi2
reg min_wage ib0.treat male age dumreg_* ideology hhinc, baselevels
est store min2
reg unemp ib0.treat male age dumreg_* ideology hhinc, baselevels
est store unemp2
reg soc_spend ib0.treat male age dumreg_* ideology hhinc, baselevels
est store spend2

#d;
coefplot (ubi2), bylabel(Universal basic income (1-5)) 
|| (min2), bylabel(Minimum wage (1-5)) 
|| (unemp2), bylabel(Unemployment benefits (1-5)) 
|| (spend2), bylabel(Social spending (1-5)) 
||, drop(age male dumreg_* ideology hhinc _cons) 
base xline(0) levels(95 90) scheme(lean2);
#d cr 
graph export "${output}spending2_new.jpg", width(2000) replace

*Perceived fairness
reg rich_comp_burden ib0.treat male age dumreg_* ideology hhinc, baselevels
est store rich_burden2
reg rich_comp_profit ib0.treat male age dumreg_* ideology hhinc, baselevels
est store rich_profit2
reg poor_comp_vat ib0.treat male age dumreg_* ideology hhinc, baselevels
est store poor_vat2
reg poor_comp_welf ib0.treat male age dumreg_* ideology hhinc, baselevels
est store poor_welf2
reg corp_comp_profit ib0.treat male age dumreg_* ideology hhinc, baselevels
est store corp_profit2

#d;
coefplot (rich_burden2), bylabel(Rich Compensate Lower Burden) 
|| (rich_profit2), bylabel(Rich Compensate Profits) 
|| (corp_profit2), bylabel(Corporations Compensate Profits) 
|| (poor_vat2), bylabel(Poor Lower Vat) 
|| (poor_welf2), bylabel(Poor Higher Benefits) 
||, drop(age male dumreg_* ideology hhinc _cons) 
base xline(0) levels(95 90) scheme(lean2);
#d cr 
graph export "${output}rich_fair2_new.jpg", width(2000) replace


**# F. No controls

* F.1 Main results
*Treatment effects
reg index_all i.treat, baselevels
est store i_tax

reg index_fair i.treat, baselevels
est store i_fair

coefplot i_tax, bylabel("Panel A: Support for" "Increasing Taxes on Rich") ///
|| i_fair, bylabel("Panel B: Perceived Fairness" "of Compensatory Policies")  ///
||, drop(_cons) base xline(0) levels(95 90) aspectratio(.5) scheme(lean2)
graph export "${output}combined_noc.jpg", replace

*Manipulation check
reg manip i.treat, baselevels
est store manipr

coefplot (manipr, label()), drop(_cons) base xline(0) title() levels(95 90) scheme(lean2)
graph export "${output}manip_noc.jpg", replace


* F.2 Additional outcomes

*Trust in government
reg post_trust_gvt i.treat, baselevels
est store post_trust2

coefplot (post_trust2, label()), drop(_cons) base xline(0) title() levels(95 90) scheme(lean2)
graph export "${output}post_trust_noc.jpg", replace

*ICW social spending
reg index_spend i.treat, baselevels
est store i_spend2

coefplot (i_spend2, label()), drop(_cons) base xline(0) levels(95 90) scheme(lean2)
graph export "${output}index_spend_noc.jpg", replace

*Support for taxing excess profits
reg tax_profits i.treat, baselevels
estimates store tax_profits2

coefplot (tax_profits2, label()), drop(_cons) base xline(0) title() levels(95 90) scheme(lean2)
graph export "${output}tax_profits_noc.jpg", replace

*Industry made too much profit
reg ex_profit i.treat, baselevels
est store ex_profit

coefplot (ex_profit, label()), drop(_cons) base xline(0) title() levels(95 90) scheme(lean2)
graph export "${output}fair_profits_noc.jpg", replace

*By attention level
reg index_all i.treat##i.attention
margins, dydx(treat) at(attention=(0(1)3))

marginsplot,  ///
xtitle("Number of Correct Mock Vignette Checks") ///
addplot(hist attention, discrete title(" ") ///
gap(30) ///
ytitle("Conditional Effect of Treatments" "on Support for Taxing the Rich") ///
yaxis(2) yscale(alt axis(2)) percent ///
ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%" 60 "60%" 70 "70%" 80 "80%", axis(2) angle(0))  ///
ytitle("Percent of Sample", axis(2) orientation(rvertical))  ///
fcolor(gs12%40) fintensity(100) scheme(lean2) lcolor(none)) ///
ylabel(, axis(1) angle(0))  ///
legend(position(4) cols(1) lcolor(none))  // 
graph export "${output}int_attention_noc.jpg", replace

** F.3 Individual outcome measures

*Tax rates
reg inc_rate i.treat, baselevels
estimates store inc_rate2

reg corp_rate i.treat, baselevels
estimates store corp_rate2

coefplot (inc_rate2), bylabel(Preferred tax rate on rich (0-100)) ///
|| (corp_rate2), bylabel(Preferred corporate tax rate (0-100)) ///
||, drop(_cons) base xline(0) levels(95 90)
graph export "${output}rates_noc.jpg", replace

*Tax reforms
reg temp_tax i.treat, baselevels
estimates store temp2
reg wealth_tax i.treat, baselevels
estimates store wealth2
reg inher_tax i.treat, baselevels
estimates store inher2
reg rem_loops i.treat, baselevels
estimates store loops2
reg other_tax i.treat, baselevels
estimates store other2

coefplot (temp2), bylabel(One-off pandemic wealth tax) ///
|| (wealth2), bylabel(Permanent wealth tax) ///
|| (inher2), bylabel(Higher inheritance tax rates) ///
|| (loops2), bylabel(Closing loopholes) ///
|| (other2), bylabel(Other reforms (open-ended)) ///
||, drop(_cons) base xline(0) levels(95 90)
graph export "${output}reforms_noc.jpg", replace

*Perceived fairness
 * Effect on support for tax reforms 
reg rich_comp_burden i.treat, baselevels
est store rich_burden2
reg rich_comp_profit i.treat, baselevels
est store rich_profit2
reg poor_comp_vat i.treat, baselevels
est store poor_vat2
reg poor_comp_welf i.treat, baselevels
est store poor_welf2
reg corp_comp_profit i.treat, baselevels
est store corp_profit2
 * Effect on perceived fairness of compensatory policies
coefplot (rich_burden2), bylabel(Rich Compensate Lower Burden) ///
|| (rich_profit2), bylabel(Rich Compensate Profits) ///
|| (corp_profit2), bylabel(Corporations Compensate Profits) ///
|| (poor_vat2), bylabel(Poor Lower Vat) ///
|| (poor_welf2), bylabel(Poor Higher Benefits) ///
||, drop(_cons) base xline(0) levels(95 90)
graph export "${output}rich_fair_noc.jpg", replace

*Social insurance
reg ubi i.treat, baselevels
est store ubi2
reg min_wage i.treat, baselevels
est store min2
reg unemp i.treat, baselevels
est store unemp2
reg soc_spend i.treat, baselevels
est store spend2

coefplot (ubi2), bylabel(Universal basic income (1-5)) ///
|| (min2), bylabel(Minimum wage (1-5)) ///
|| (unemp2), bylabel(Unemployment benefits (1-5)) ///
|| (spend2), bylabel(Social spending (1-5)) ///
||, drop(_cons) base xline(0) levels(95 90)
graph export "${output}spending_noc.jpg", replace



**# G.T1 as reference category

** G.1 Main results
*Treatment effects
reg index_all ib1.treat male age dumreg_* ideology hhinc, baselevels
est store i_tax

reg index_fair ib1.treat male age dumreg_* ideology hhinc, baselevels
est store i_fair

#d;
	coefplot i_tax, bylabel("Panel A: Support for" "Increasing Taxes on Rich") 
	|| i_fair, bylabel("Panel B: Perceived Fairness" "of Compensatory Policies")  
	||, drop( male age region ideology hhinc dumreg_* _cons) base xline(0) levels(95 90)
	aspectratio(.5) scheme(lean2);
#d cr 
graph export "${output}combined_t1.jpg", width(2000) replace

*Manipulation check
reg manip ib1.treat male age dumreg_* ideology hhinc, baselevels
est store manipr

#d;
	coefplot (manipr, label()), 
	drop( male age dumreg_* ideology hhinc _cons) 
	base xline(0) title() levels(95 90) scheme(lean2);
#d cr 
graph export "${output}manip_t1.jpg", width(2000) replace

** G.2 Other outcomes

*Effect on trust in government
reg post_trust_gvt ib1.treat male age dumreg_* ideology hhinc, baselevels
est store post_trust2

#d;
	coefplot (post_trust2, label()), 
	drop( male age dumreg_* ideology hhinc _cons) 
	base xline(0) title() levels(95 90) scheme(lean2);
#d cr 
graph export "${output}post_trust_t1.jpg", width(2000) replace

*ICW social spending
reg index_spend ib1.treat male age dumreg_* ideology hhinc, baselevels
est store i_spend2

#d; 
	coefplot (i_spend2, label()), 
	drop(male age dumreg_* ideology hhinc _cons) 
	base xline(0) levels(95 90) scheme(lean2);
#d cr 
graph export "${output}index_spend_t1.jpg", width(2000) replace

*Support for taxing excess profits
reg tax_profits ib1.treat male age dumreg_* ideology hhinc, baselevels
estimates store tax_profits2

#d;
	coefplot (tax_profits2, label()), 
	drop( male age dumreg_* ideology hhinc _cons) 
	base xline(0) title() levels(95 90) scheme(lean2);
#d cr 
graph export "${output}tax_profits_t1.jpg", width(2000) replace

*Industry made too much profit
reg ex_profit ib1.treat male age dumreg_* ideology hhinc, baselevels
est store ex_profit

#d;
	coefplot (ex_profit, label()), 
	drop(male age dumreg_* 
		ideology hhinc _cons) 
	base xline(0) title() 
	levels(95 90) scheme(lean2);
#d cr 
graph export "${output}fair_profits_t1.jpg", width(2000) replace

*By attention level
reg index_all ib1.treat##i.attention male age dumreg_* ideology hhinc
margins, dydx(treat) at(attention=(0(1)3))

#d;
marginsplot,  
xtitle("Number of Correct Mock Vignette Checks") 
addplot(hist attention, discrete title( " ") 
gap(30) 
ytitle("Conditional Effect of Treatments" "on Support for Taxing the Rich") 
yaxis(2) yscale(alt axis(2)) percent 
ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 
	   40 "40%" 50 "50%" 60 "60%" 70 "70%"
	   80 "80%", axis(2)) 
ytitle("Percent of Sample", 
axis(2) orientation(rvertical))  
fcolor(gs12%40) fintensity(100) lcolor(none)) scheme(lean2);
#d cr 
graph export "${output}int_attention_t1.jpg", width(2000) replace


** G.3 Individual outcome measures

*Tax rates
reg inc_rate ib1.treat male age dumreg_* ideology hhinc, baselevels
estimates store inc_rate2

reg corp_rate ib1.treat male age dumreg_* ideology hhinc, baselevels
estimates store corp_rate2

#d;
	coefplot (inc_rate2), 
	bylabel(Preferred tax rate on rich (0-100)) 
	|| (corp_rate2), 
	bylabel(Preferred corporate tax rate (0-100)) 
	||, drop( male age dumreg_* ideology hhinc _cons) 
	base xline(0) levels(95 90) scheme(lean2);
#d cr 
graph export "${output}rates_t1.jpg", width(2000) replace

*Tax reforms
reg temp_tax ib1.treat male age dumreg_* ideology hhinc, baselevels
estimates store temp2
reg wealth_tax ib1.treat male age dumreg_* ideology hhinc, baselevels
estimates store wealth2
reg inher_tax ib1.treat male age dumreg_* ideology hhinc, baselevels
estimates store inher2
reg rem_loops ib1.treat male age dumreg_* ideology hhinc, baselevels
estimates store loops2
reg other_tax ib1.treat male age dumreg_* ideology hhinc, baselevels
estimates store other2

#d;
coefplot (temp2),
	bylabel(One-off pandemic wealth tax) 
	|| (wealth2), bylabel(Permanent wealth tax) 
	|| (inher2), bylabel(Higher inheritance tax rates) 
	|| (loops2), bylabel(Closing loopholes)
	|| (other2), bylabel(Other reforms (open-ended)) 
	||, drop( male age dumreg_* ideology hhinc _cons) 
	base xline(0) levels(95 90) scheme(lean2);
#d cr 
graph export "${output}reforms_t1.jpg", width(2000) replace

*Perceived fairness
reg rich_comp_burden ib1.treat male age dumreg_* ideology hhinc, baselevels
est store rich_burden2
reg rich_comp_profit ib1.treat male age dumreg_* ideology hhinc, baselevels
est store rich_profit2
reg poor_comp_vat ib1.treat male age dumreg_* ideology hhinc, baselevels
est store poor_vat2
reg poor_comp_welf ib1.treat male age dumreg_* ideology hhinc, baselevels
est store poor_welf2
reg corp_comp_profit ib1.treat male age dumreg_* ideology hhinc, baselevels
est store corp_profit2

#d;
	coefplot (rich_burden2), bylabel(Rich Compensate Lower Burden) 
	|| (rich_profit2), bylabel(Rich Compensate Profits) 
	|| (corp_profit2), bylabel(Corporations Compensate Profits) 
	|| (poor_vat2), bylabel(Poor Lower Vat) 
	|| (poor_welf2), bylabel(Poor Higher Benefits) 
	||, drop( male age dumreg_* ideology hhinc _cons) 
	base xline(0) levels(95 90) scheme(lean2);
#d cr 
graph export "${output}rich_fair_t1.jpg", width(2000) replace

*Social insurance
reg ubi ib1.treat male age dumreg_* ideology hhinc, baselevels
est store ubi2
reg min_wage ib1.treat male age dumreg_* ideology hhinc, baselevels
est store min2
reg unemp ib1.treat male age dumreg_* ideology hhinc, baselevels
est store unemp2
reg soc_spend ib1.treat male age dumreg_* ideology hhinc, baselevels
est store spend2

#d;
	coefplot (ubi2), bylabel(Universal basic income (1-5)) 
	|| (min2), bylabel(Minimum wage (1-5)) 
	|| (unemp2), bylabel(Unemployment benefits (1-5)) 
	|| (spend2), bylabel(Social spending (1-5)) 
	||, drop( male age dumreg_* ideology hhinc _cons) 
	base xline(0) levels(95 90) scheme(lean2);
#d cr 
graph export "${output}spending_t1.jpg", width(2000) replace

** End of the dofile

exit

