/*==================================================
project:       Testing the Compensatory Theory: 
               A Survey Experiment on Covid-19 
			   and Redistributive Preferences in the UK
Author:        Pablo Querubin, Alan David Gomez 
E-email:       ad.gomezb@uniandes.edu.co
Dependencies:  New York University
----------------------------------------------------
Creation Date: 19 Mar 2024 - 07:23:58
Output:        uk_jul2023_rep.dta 
               responses.dta             
==================================================*/

/*==================================================
* Please note that some variables are created within 
* a preserve block. To ensure proper execution and 
* avoid potential issues, it is recommended to run 
* the code all at once
**================================================*/

/*==================================================
                Import and Fix data
==================================================*/

** Import Data
#d; 
	import delimited
		"${dir0}rawdata.csv", 
			varnames(1) clear;
#d cr 

*drop those who did not consent
drop if consent=="0"

*drop unnecessary variables
drop status distributionchannel userlanguage startdate enddate consent finished progress q_terminateflag 

*drop first two rows
drop in 1/2

destring, replace

*renaming, labelling
rename q2_1 ideology
label var ideology "Ideological self-placement 0(left)-10(right)"

rename q3 gvt_role
label var gvt_role "Need a strong government to handle economic problems"

rename q4 pre_trust_gvt
label var pre_trust_gvt "How much do you trust British gvts to place needs of nation above (pre-T)?"
label define tr_gvt 1 "Almost never" 2 "Only some of the time" 3 "Most of the time" 4 "Just about Always"
label values pre_trust_gvt tr_gvt

rename q5_1 equalt
label var equalt "Equal treatment position 1(equal)-5(unequal)"

*q9-11 attention check
gen att1=q9==3
gen att2=q10==2
gen att3=q11==1
egen attention=rowtotal(att1 att2 att3)
label var attention "Number of correct answers to mock vignette questions (/3)"
drop q9 q10 q11 att1 att2 att3

gen description=""
replace description=q14 if treatment=="T1"
replace description=q17 if treatment=="T2"
replace description=q20 if treatment=="T3"
drop q14 q17 q20

rename q23_1 inc_rate
label var inc_rate "Preferred average tax rate on rich individuals"

rename q24_6 corp_rate
label var corp_rate "Preferred tax rate on corporations"

rename q106_1 ubi
label var ubi "Position on creating a universal basic income"
rename q106_2 min_wage
label var min_wage "Position on raising minimum wage"
rename q106_3 unemp
label var unemp "Position on long-term expansion of unemployment benefits"
rename q106_4 soc_spend
label var soc_spend "Position on increasing spending on welfare programs"
label define pos 1 "Strongly oppose" 2 "Somewhat oppose" 3 "Neither favor nor oppose" 4 "Somewhat favor" 5 "Strongly favor"
foreach var of varlist ubi min_wage unemp soc_spend {
	label values `var' pos
}

rename q26 tax_profits
label var tax_profits "Should the gvt tax excess profits made by companies during pandemic?"
label define tax 1 "Definitely not" 2 "Probably not" 3 "Maybe yes, maybe not" 4 "Probably yes" 5 "Definitely yes"
label values tax_profits tax

rename q27_2 temp_tax
label var temp_tax "Supports one-off pandemic wealth tax"
rename q27_3 wealth_tax
label var wealth_tax "Supports new and permanent tax on wealth"
rename q27_4 inher_tax
label var inher_tax "Supports higher inheritance tax rates"
rename q27_6 rem_loops
label var rem_loops "Supports closing loopholes in current tax regimes"
rename q27_7 other_tax
label var other_tax "Supports other taxes on the rich"
rename q27_7_text other_tax_open
label var other_tax_open "Other taxes on the rich they support (open ended)"
rename q27_8 no_tax
label var no_tax "Does not support increasing taxes on the rich"

rename q50_1 rich_comp_burden
label var rich_comp_burden "Fair to charge higher taxes on rich to compensate lower burden of pandemic"
rename q50_2 rich_comp_profit
label var rich_comp_profit "Fair to charge higher taxes on rich to compensate profits from pandemic"
rename q50_3 poor_comp_vat
label var poor_comp_vat "Fair to charge lower VAT taxes on poor to compensate higher burden of pandemic"
rename q50_4 poor_comp_welf
label var poor_comp_welf "Fair to give higher benefits payments to poor to compensate higher burden of pandemic"
rename q50_5 corp_comp_profit
label var corp_comp_profit "Fair to charge excess profits tax on corpotations to compensate profits from pandemic"
label define fair 1 "Extremely unfair" 2 "Unfair" 3 "Neutral" 4 "Fair" 5 "Extremely fair"
foreach var of varlist rich_comp_burden rich_comp_profit poor_comp_vat poor_comp_welf corp_comp_profit {
	label values `var' fair
}

rename q30 fair_profit
label var fair_profit "During the pandemic did industry make too much profit?"
recode fair_profit (1=2) (2=1)
label define profit 0 "Not enough profit" 1 "A fair profit" 2 "Too much profit"
label values fair_profit profit

rename q32 manip
label var manip "Is gvt responsible for unequal burden of pandemic?"
label define agree 1 "Strongly disagree" 2 "Somewhat disagree" 3 "Neither agree nor disagree" 4 "Somewhat agree" 5 "Strongly agree"
label values manip agree

rename q33_1 reason1
rename q33_2 reason2

rename q36 male
replace male=0 if male==9
rename q37 age

rename q38 race
label define ethn 1 "White" 2 "Black" 3 "Asian" 4 "Other"
label values race ethn

rename q39 region

rename q40 hhinc

rename q41 educ
label var educ "Education level"
label define edu 1 "No qualifications" 2 "Below A levels" 3 "A levels or equivalent" 4 "Other higher education" 5 "Degree or above" 
label values educ edu

rename q42 marital
label var marital "Marital status"

rename q43 work
label var work "Employment status"
label define wrk 1 "Full-time employee" 2 "Part-time employee" 3 "Self-employed or small business-owner" 4 "Unemployed and looking for work" 5 "Student" 6 "Not in labor force" 7 "On gvt training" 8 "Sick or disabled" 9 "Other"
label values work wrk

rename q44 partyid
label var partyid "Party identification"

rename q45 post_trust_gvt
label var post_trust_gvt "How much do you trust British gvts to place needs of nation above (post-T)?"
label values post_trust_gvt tr_gvt

rename q46_1 received
label var received "Respondent received gvt assistance during pandemic"

rename q46_2 hh_received
label var received "Hh member received gvt assistance during pandemic"

rename q46_0 no_received
label var received "Neither respondent nor other hh members received gvt assistance during pandemic"


foreach var of varlist received no_received hh_received hhinc ideology gvt_role equalt temp_tax wealth_tax inher_tax rem_loops other_tax no_tax inc_rate corp_rate soc_spend tax_profits corp_comp_profit fair_profit {
	replace `var'=. if `var'==-99
}

*create variable with number of different reforms for raising taxes on the rich they support
egen n_tax=rowtotal(temp_tax wealth_tax inher_tax rem_loops other_tax), missing
label var n_tax "Number of different reforms to raise taxes on the rich they support"

*create dummy for those who support reforms to raise taxes on the rich
gen tax_rich=1-no_tax
replace tax_rich=. if no_tax==.
label var tax_rich "Supports reforms to raise taxes on the rich"

*create numeric variable with treatment assigned
gen treat=.
replace treat=1 if treatment=="T1" 
replace treat=2 if treatment=="T2"
replace treat=3 if treatment=="T3" 
replace treat=0 if treatment=="Control" 
label var treat "Treatment assigned"
label define tr 0 "Control" 1 "T1" 2 "T2" 3 "T3" 4 "T4"
label values treat tr


*variables with timing data for treatment pages
gen time_treat=.
foreach num of numlist 1/3 {
	replace time_treat=timer_t`num'_pagesubmit if treatment=="T`num'"
	drop timer_t`num'_pagesubmit
}
label var time_treat "Time spent in treatment page"

** Create dummies for region 
tab region, gen(dumreg_)

** Create dummy for race 
gen dum_white = (race==1)
replace dum_white = . if race==.

** Create dummy for full time work
gen dum_fulltime = (work==1)
replace dum_fulltime = . if work==.

** Create dummy for part time work
gen dum_parttime = (work==2)
replace dum_parttime = . if work==.

** Create dummy for not in labour force 
gen dum_notlab = (work==6)
replace dum_notlab = . if work==.

** Create dummy for marital status  
gen dum_married = (marital==1)
replace dum_married=. if marital==.

** Create dummy for Conservative party  
gen dum_conservative = (partyid==1)

** Create dummy for Labour party  
gen dum_labour = (partyid==2)

** Create dummy for Degree or above in education
gen dummy_educ = (educ==5)
replace dummy_educ =. if educ==.


save "${output}uk_jul2023_rep.dta", replace

*-----  Append response classification by ChatGPT
keep responseid manip reason1 reason2

preserve 

keep responseid reason1
rename reason1 reason

gen num_reason = 1 

tempfile numreas 
save `numreas'

restore 

keep responseid reason2
rename reason2 reason
gen num_reason = 2

append using  `numreas'

save "${GPT}responses.dta", replace


