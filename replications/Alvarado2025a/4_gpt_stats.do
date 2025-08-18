/*==================================================
project:       Testing the Compensatory Theory: 
               A Survey Experiment on Covid-19 
			   and Redistributive Preferences in the UK
Author:        Pablo Querubin, Alan David Gomez 
E-email:       ad.gomezb@uniandes.edu.co
Dependencies:  New York University
----------------------------------------------------
Creation Date: 19 Mar 2024 - 07:23:58
Output:        Figures for Appendix in section C.1.2             
==================================================*/

/*==================================================
             1: Import and Fix data
==================================================*/

**# Appendix C.1.2

** Import replication data 
use "${output}uk_jul2023_rep.dta", clear
keep treat manip responseid  

merge 1:m responseid using "${dir0}classified_reas.dta"


*** Table for covariates 
tab category, gen(dum_)
tab treat, gen(dumtreat_)
gen complier = (manip>3)
gen notcomplier = (manip<3)

global vars "dum_5 dum_6 dum_2 dum_3 dum_1"


local r=0
foreach x of varlist $vars {
	local r = `r'+1
}

mat RR = J(`r',8,.)
mat SS = J(`r',8,0)

local i =1
foreach x of varlist $vars {
	
	reg `x' dumtreat_1
	mat RR[`i',1] = r(table)[1,1]
	mat RR[`i',2] = r(table)[4,1]
	
	scalar pval = r(table)[4,1]
	scalar sig1 = (pval<0.01)+(pval<0.05)+(pval<0.10)
	mat SS[`i',1] = sig1 
	
	reg `x' dumtreat_2
	mat RR[`i',3] = r(table)[1,1]
	mat RR[`i',4] = r(table)[4,1]
	
	scalar pval = r(table)[4,1]
	scalar sig1 = (pval<0.01)+(pval<0.05)+(pval<0.10)
	mat SS[`i',3] = sig1 
	
	reg `x' dumtreat_3
	mat RR[`i',5] = r(table)[1,1]
	mat RR[`i',6] = r(table)[4,1]
	
	scalar pval = r(table)[4,1]
	scalar sig1 = (pval<0.01)+(pval<0.05)+(pval<0.10)
	mat SS[`i',5] = sig1 
	
	reg `x' dumtreat_4
	mat RR[`i',7] = r(table)[1,1]
	mat RR[`i',8] = r(table)[4,1]
	
	scalar pval = r(table)[4,1]
	scalar sig1 = (pval<0.01)+(pval<0.05)+(pval<0.10)
	mat SS[`i',7] = sig1 
	
	
	local i= `i'+1
}

#d;
	frmttable using "${output}gpt_tab.tex", tex frag statmat(RR) substat(1) 
			annotate(SS) asymbol(*,**,***) sdec(3) replace
			ctitles("","Control","T1","T2","T3")
			rtitles("Policy Failure"\""\
					"Unequal Impact"\""\
					"Goverment Other"\""\
					"Limited Responsibility"\""\
					"External Causes"\"");
#d cr 


global vars "dum_5 dum_6 dum_2 dum_3 dum_1"


local r=0
foreach x of varlist $vars {
	local r = `r'+1
}

mat RR = J(`r',8,.)
mat SS = J(`r',8,0)

local i =1
foreach x of varlist $vars {
	
	reg `x' complier
	mat RR[`i',1] = r(table)[1,1]
	mat RR[`i',2] = r(table)[4,1]
	
	scalar pval = r(table)[4,1]
	scalar sig1 = (pval<0.01)+(pval<0.05)+(pval<0.10)
	mat SS[`i',1] = sig1 
	
	reg `x' notcomplier
	mat RR[`i',3] = r(table)[1,1]
	mat RR[`i',4] = r(table)[4,1]
	
	scalar pval = r(table)[4,1]
	scalar sig1 = (pval<0.01)+(pval<0.05)+(pval<0.10)
	mat SS[`i',3] = sig1 

	local i= `i'+1
}

#d;
	frmttable using "${output}gpt_tab_comp.tex", tex frag statmat(RR) substat(1) 
			annotate(SS) asymbol(*,**,***) sdec(3) replace
			ctitles("","Complier","Non-Complier")
			rtitles("Policy Failure"\""\
					"Unequal Impact"\""\
					"Goverment Other"\""\
					"Limited Responsibility"\""\
					"External Causes"\"");
#d cr 



** End of the dofile

exit

