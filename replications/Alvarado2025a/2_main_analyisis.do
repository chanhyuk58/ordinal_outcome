/*==================================================
project:       Testing the Compensatory Theory: 
               A Survey Experiment on Covid-19 
			   and Redistributive Preferences in the UK
Author:        Pablo Querubin, Alan David Gomez 
E-email:       ad.gomezb@uniandes.edu.co
Dependencies:  New York University
----------------------------------------------------
Creation Date: 19 Mar 2024 - 07:23:58        
Output:        fig1.eps
               fig2.eps      
==================================================*/

/*==================================================
                   Main Analysis
==================================================*/

** Import replication data 
use "${output}uk_jul2023_rep.dta", clear

gen stdgroup=(treat==0)

* Create standardized weighted index
swindex inc_rate temp_tax wealth_tax inher_tax rem_loops other_tax corp_rate, gen(index_all) normby(stdgroup) fullrescale replace

swindex rich_comp_burden rich_comp_profit poor_comp_vat poor_comp_welf corp_comp_profit, gen(index_fair) normby(stdgroup) fullrescale replace


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
graph export "${output}combined2.jpg", replace width(2000)
graph export "${outeps}fg1.eps", replace

*Manipulation check
reg manip ib0.treat male age dumreg_* ideology hhinc, baselevels
est store manip

#d;
	coefplot (manip, label()), 
	drop(age male dumreg_* ideology hhinc _cons) 
	base xline(0) title() levels(95 90) scheme(lean2);
#d cr 
graph export "${output}manip2.jpg", replace width(2000) 
graph export "${outeps}fg2.eps", replace
