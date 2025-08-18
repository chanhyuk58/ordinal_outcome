version 14
***************************************************************************
* This file provides analysis in "Public Reactions to Non-Compliance	  * 
* in the Colombian Tutela"							 				      *
*																		  *
* Jeffrey K. Staton													      *
* July 12, 2021  														  *
***************************************************************************


	
/* Measurement Notes for Studies 1 and 2
	a. Legitimacy: The original legitimacy items are coded so higher numbers reflect lower levels of legitimacy. We reverse these
	codes with any variable that has an "underscore r." Here is a summary of the measures.  
		legit1_r is reverse coded measure of the limit jurisdiction question
		legit2_r is reverse coded measure of reducing power if they start making decisions with which people disagree
		legitimacyscale is a the average of legit1_r and legit2_r
		veryhigh_legit is binary: 1 if legitimacyscale>=5 and 0 if legitimacyscale<5 (there are no values between 
		
	b. Rule of law: All of the rule of law measures are reverse coded, as well. We use four for our RoL index. 
		rol1a_r: There are times when authorities should disobey the law
		rol1c_r: There are times when citizens should disobey the law
		rol6_r: Hard to follow the law when others do not
		rol2_r: Violating the law is not bad; getting caught is. 
		ruleoflaw_index_r: Average of these four variables. 
		
	c. Trust: 
		gsstrust: Generalized trust -- it increases in trust
		conbur: Trust in the bureaucracy to make good decisions for socieity (increases in trust)
		conjuez: Trust in judges to make decisions good for society (increases in trust)
*/

* I. Section 2: "The Colombian Tutela"
	cd "/Users/jkstato/Dropbox/Research/Current Projects/Non-Compliance/Data/Replication Final" //set working directory
	
	use study3.dta, clear

	
	sum you_use if study==0 		//Means in Control Group
	sum friend_use  if study==0 
	sum import_use  if study==0
	alpha you_use friend_use import_use if study==0
	
	*Figure 1: "Views of Tutela" for control group
	graph bar (mean) you_use (mean) friend_use (mean) import_use (mean) avg_use if study==0, bargap(5) ///
		ylabel(1(1)7) legend(off) scheme(s1mono) ///
		title(Views of Tutela) ///
		ttext(-.2 15 "Personally use") ///
		ttext(-.2 38 "Recommend use") ///
		ttext(-.2 62 "Tutela important") ///
		ttext(-.2 86 "Average") ///
		bar(1, color(gs7))  ///
		bar(2, color(gs7)) ///
		bar(3, color(gs7)) ///
		bar(4, color(gs7))
		gr export "outcomes.pdf", replace
	
	*Figure 1: "Sometimes it's necessary to disobey the law variable" for control group
	hist rol_disobey_authority, discrete freq scheme(s1mono) xlabel(1(1)7) title(Views of Non-compliance) ///
		xtitle("Sometimes Necessary for Authorities to Disobey the Law (Agree - Disagree)") ///
		color(gs7) lcolor(black)
		gr export "nec_disobey.pdf", replace	
			

	*Figures 1: Regional Contexts
	use "collapsedAB2004-2020.dta", clear
	xtset countrynum waver
	xtline b10a, overlay i(countrynum) t(waver) scheme(s1mono) legend(off) ///
		plot1opts(lcolor(gs12) lw(thin)) ///
		plot2opts(lcolor(gs12) lw(thin)) ///
		plot3opts(lcolor(gs12) lw(thin)) ///
		plot4opts(lcolor(gs12) lw(thin)) ///
		plot5opts(lcolor(gs12) lw(thin)) ///
		plot6opts(lcolor(gs12) lw(thin)) ///
		plot7opts(lcolor(gs12) lw(thin)) ///
		plot8opts(lcolor(black) lw(thick)) ///
		plot9opts(lcolor(gs12) lw(thin)) ///
		plot10opts(lcolor(gs12) lw(thin)) ///
		plot11opts(lcolor(gs12) lw(thin)) ///
		plot12opts(lcolor(gs12) lw(thin)) ///
		plot13opts(lcolor(gs12) lw(thin)) ///
		plot14opts(lcolor(gs12) lw(thin)) ///
		plot15opts(lcolor(gs12) lw(thin)) ///
		plot16opts(lcolor(gs12) lw(thin)) ///
		plot17opts(lcolor(gs12) lw(thin)) ///
		plot18opts(lcolor(gs12) lw(thin)) ///
		plot19opts(lcolor(red) lw(thin)) ///
		xlabel(2004(2)2018) ylabel(2(1)5) ///
		xtitle(Year) ytitle(Trust in the Judiciary) ///
		ttext(4.19 2004.73 "Colombia", color(black)) ///
		ttext(3.94 2005.63 "Lat Amer Avg.", color(red)) ///
		title ("Trust in the Judiciary, 2004-2018") 
		gr export "judicial_trust_lapop.pdf", replace
		
	xtline b3, overlay i(countrynum) t(waver) scheme(s1mono) legend(off) ///
		plot1opts(lcolor(gs12) lw(thin)) ///
		plot2opts(lcolor(gs12) lw(thin)) ///
		plot3opts(lcolor(gs12) lw(thin)) ///
		plot4opts(lcolor(gs12) lw(thin)) ///
		plot5opts(lcolor(gs12) lw(thin)) ///
		plot6opts(lcolor(gs12) lw(thin)) ///
		plot7opts(lcolor(gs12) lw(thin)) ///
		plot8opts(lcolor(black) lw(thick)) ///
		plot9opts(lcolor(gs12) lw(thin)) ///
		plot10opts(lcolor(gs12) lw(thin)) ///
		plot11opts(lcolor(gs12) lw(thin)) ///
		plot12opts(lcolor(gs12) lw(thin)) ///
		plot13opts(lcolor(gs12) lw(thin)) ///
		plot14opts(lcolor(gs12) lw(thin)) ///
		plot15opts(lcolor(gs12) lw(thin)) ///
		plot16opts(lcolor(gs12) lw(thin)) ///
		plot17opts(lcolor(gs12) lw(thin)) ///
		plot18opts(lcolor(gs12) lw(thin)) ///
		plot19opts(lcolor(red) lw(thin)) ///
		xlabel(2004(2)2018) ylabel(2(1)5) ///
		xtitle(Year) ytitle(Basic Rights Are Protected) ///
		ttext(4.14 2004.6 "Colombia", color(black)) ///
		ttext(3.86 2005 "Lat Amer Avg.", color(red)) ///
		title ("Perceptions of Protection for Basic Rights, 2004-2018")
		gr export "basic_rights_lapop.pdf", replace	

* II. Section 5: "Non-Compliance Tracking Study"
	use tracking, clear		
		
		sum nocomply
		tab clearorder
		tab plazo
		tab edlevel
		
		
* III. Section 6: Study 1
	use study1_2, clear
		drop if tasa2==88 // Drop observations with missing data for acceptability
		
	*A. Descriptive Results: Accceptance and Donation Outcomes
		*tasa2 is acceptability measure
		sum tasa2  if inform==1, detail // inform==1 if resp. learned the actual non-compliance rate 
		sum tasa2, detail //just for comparison, remembering that we are including people who did not learn the true rate. 
		
		gen accept_3=1 if tasa2>=1&tasa2<=3 //three categor measure
			replace accept_3=2 if tasa2==4
			replace accept_3=3 if tasa2>=5&tasa2<=7
			label variable accept_3 "Acceptance of Non-Compliance (3 pt. Scale)"
		tab accept_3
		tab accept_3 if inform==1
		bysort arm: tab accept_3 if inform==1
		bysort arm: tab accept_3
		
		*don1 is donation measure
		tab don1 if inform==1
		sum don1 if inform==1, detail
		sum don1, detail

	*B. Figure 4: 
		hist tasa2 if inform==1, freq xlabel(1(1)7) discrete scheme(s1mono) ///
				xtitle (Non-Compliance is Acceptable (Scale is ordered Low to High)) ///
				title(Acceptance of Non-Compliance)
				graph export accept_hist.pdf, replace
				
		hist don1 if inform==1, freq xlabel(0(1)17) discrete scheme(s1mono) ///
				xtitle (Number of Points Donated) ///
				title(Respondent Donations)
				graph export donate_hist.pdf, replace

	*D. Relationship between Acceptability and Donation (for individuals who learned the true non-compliance rate)
		reg don1 tasa2 if inform==1
		xi: reg don1 tasa2 female i.ageRecode i.region veryhigh_legit gsstrust ruleoflaw_index_r i.nse  if inform==1
			
		
	*E. Effects of Becoming Informed (control is Control w/o Prior)
		use study1_2, clear
		drop if tasa2==88

		*Table 2 and Table 11(Appendix)
		drop if arm==2|arm==4 //Dropping the arms for Study 2 (i.e., the arms with elicited priors)
		reg tasa2 arm3 arm5 arm6 arm7 arm8 // acceptability
		reg don1 arm3 arm5 arm6 arm7 arm8  //donation
		xi: reg tasa2 arm3 arm5 arm6 arm7 female i.ageRecode i.region ruleoflaw_index_r if arm8==0 
		xi: reg don1 arm3 arm5 arm6 arm7 female i.ageRecode i.region ruleoflaw_index_r legitimacyscale if arm8==0	
			
	*F. Table 3  
		drop if arm==1
		reg tasa2 arm5 arm6 arm7 arm8
		reg don1 arm5 arm6 arm7 arm8

	*H. Heterogenous Treatment Effects: Rule of Law, Judicial Legitimacy, and Judicial Trust
		use study1_2, clear
		drop if tasa2==88
		drop if arm==2|arm==4
			
		*Create Interactions for Treatment Arms & pre-treatment covariates
			gen ruleoflawXstudy= ruleoflaw_index_r*arm3
			gen ruleoflawXvague= ruleoflaw_index_r*arm5
			gen ruleoflawXcost= ruleoflaw_index_r*arm6
			gen ruleoflawXses= ruleoflaw_index_r*arm7
			gen ruleoflawXnoc= ruleoflaw_index_r*arm8

			gen legitXstudy= legitimacyscale*arm3
			gen legitXvague= legitimacyscale*arm5
			gen legitXcost= legitimacyscale*arm6
			gen legitXses= legitimacyscale*arm7
			gen legitXnoc= legitimacyscale*arm8
				
			gen jtrustXstudy= conjuez*arm3
			gen jtrustXvague= conjuez*arm5
			gen jtrustXcost= conjuez*arm6
			gen jtrustXses= conjuez*arm7
			gen jtrustXnoc= conjuez*arm8
			
	*G. Acceptability Models
		*Rule of Law Interactions
		reg tasa2 ruleoflaw_index_r arm5 ruleoflawXvague arm6 ruleoflawXcost arm7 ruleoflawXses ///
			if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */
			
		reg tasa2 ruleoflaw_index_r arm5 ruleoflawXvague arm6 ruleoflawXcost arm7 ruleoflawXses ///
			female i.ageRecode i.region legitimacyscale if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */
		
		reg tasa2 ruleoflaw_index_r arm3 ruleoflawXstudy arm5 ruleoflawXvague arm6 ruleoflawXcost arm7 ruleoflawXses ///
			if arm8==0 /*baseline = pure control grou, i.e., Arm 1 */ 
				
		reg tasa2 ruleoflaw_index_r arm3 ruleoflawXstudy arm5 ruleoflawXvague arm6 ruleoflawXcost arm7 ruleoflawXses ///
			legitimacyscale female i.ageRecode i.region   if arm8==0 /*baseline = pure control grou, i.e., Arm 1 */
			*Note the coef. on vagueness interaction. 
			
		*Judicial Legitimacy Interactions 
		reg tasa2 legitimacyscale arm5 legitXvague arm6 legitXcost arm7 legitXses ///
			if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */
			*Note Legitimacy and High Cost interaction

		reg tasa2 legitimacyscale arm5 legitXvague arm6 legitXcost arm7 legitXses ///
			female i.ageRecode i.region  ruleoflaw_index_r  if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */
			*Note Legitimacy and High Cost interaction
	
		reg tasa2 legitimacyscale arm3 legitXstudy arm5 legitXvague arm6 legitXcost arm7 legitXses ///
			if arm8==0 /*baseline = pure control grou, i.e., Arm 1 */
			*Now effect estimated to be different for vagueness but in a strange way. Also the cost interaction has changed. 
					
		reg tasa2 legitimacyscale arm3 legitXstudy arm5 legitXvague arm6 legitXcost arm7 legitXses ///
			ruleoflaw_index_r female i.ageRecode i.region   if arm8==0 /*baseline = pure control grou, i.e., Arm 1 */
					
		*Judicial Trust Interactions
		reg tasa2 conjuez arm5 jtrustXvague arm6 jtrustXcost arm7 jtrustXses ///
			if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */
			
		reg tasa2 conjuez arm5 jtrustXvague arm6 jtrustXcost arm7 jtrustXses ///
			legitimacyscale ruleoflaw_index_r female i.ageRecode i.region ///
				if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */
		
		reg tasa2 conjuez arm3 jtrustXstudy arm5 jtrustXvague arm6 jtrustXcost arm7 jtrustXses ///
			if arm8==0 /*baseline = pure control group, i.e., Arm 1 */
			*And here we have the cost interaction. Very hard to interpret. 
				
		reg tasa2 conjuez arm3 jtrustXstudy arm5 jtrustXvague arm6 jtrustXcost arm7 jtrustXses ///
			legitimacyscale ruleoflaw_index_r female i.ageRecode i.region   ///
			if arm8==0 /*baseline = pure control group, i.e., Arm 1 */
			*And here we have the cost interaction. Very hard to interpret. 
	
		*Summary of Acceptance Models w/ Interactions: We fit 12 models. There were 36 interactions across these models. 
		*We found a statistically significant interaction term 6 times, but for different treatments.  
		*There is no consistent finding and certainly nothing that would survive a multiple comparisons correction. 
		
		
		*Donation Models
		*Rule of Law Interactions
		reg don1 ruleoflaw_index_r arm5 ruleoflawXvague arm6 ruleoflawXcost arm7 ruleoflawXses ///				
		if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */
			
		reg don1 ruleoflaw_index_r arm5 ruleoflawXvague arm6 ruleoflawXcost arm7 ruleoflawXses ///
			female i.ageRecode i.region legitimacyscale if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */
		
		reg don1 ruleoflaw_index_r arm3 ruleoflawXstudy arm5 ruleoflawXvague arm6 ruleoflawXcost arm7 ruleoflawXses ///
			if arm8==0 /*baseline = pure control grou, i.e., Arm 1 */ 
				
		reg don1 ruleoflaw_index_r arm3 ruleoflawXstudy arm5 ruleoflawXvague arm6 ruleoflawXcost arm7 ruleoflawXses ///
			legitimacyscale female i.ageRecode i.region   if arm8==0 /*baseline = pure control grou, i.e., Arm 1 */
			
			 
		*Judicial Legitimacy Interactions 
		reg don1 legitimacyscale arm5 legitXvague arm6 legitXcost arm7 legitXses ///
			if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */

		reg don1 legitimacyscale arm5 legitXvague arm6 legitXcost arm7 legitXses ///
			female i.ageRecode i.region  ruleoflaw_index_r  if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */
	
		reg don1 legitimacyscale arm3 legitXstudy arm5 legitXvague arm6 legitXcost arm7 legitXses ///
			if arm8==0 /*baseline = pure control group, i.e., Arm 1 */
		
		reg don1 legitimacyscale arm3 legitXstudy arm5 legitXvague arm6 legitXcost arm7 legitXses ///
			ruleoflaw_index_r female i.ageRecode i.region   if arm8==0 /*baseline = pure control grou, i.e., Arm 1 */
					
		*Judicial Trust Interactions
		reg don1 conjuez arm5 jtrustXvague arm6 jtrustXcost arm7 jtrustXses ///
			if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */
			
		reg don1 conjuez arm5 jtrustXvague arm6 jtrustXcost arm7 jtrustXses ///
			legitimacyscale ruleoflaw_index_r female i.ageRecode i.region ///
			if arm1==0&arm8==0 /*baseline = study group, i.e., Arm 3 */
		
		reg don1 conjuez arm3 jtrustXstudy arm5 jtrustXvague arm6 jtrustXcost arm7 jtrustXses ///
			if arm8==0 /*baseline = pure control group, i.e., Arm 1 */
				
		reg don1 conjuez arm3 jtrustXstudy arm5 jtrustXvague arm6 jtrustXcost arm7 jtrustXses ///
			legitimacyscale ruleoflaw_index_r female i.ageRecode i.region   ///
			if arm8==0 /*baseline = pure control group, i.e., Arm 1 */
		*Summary of Donation Model w/ Interactions: We fit another 12 models. We found no interactions that were statistically significant. 

		*Judicial Legitimacy, Rule of Law, and the Outcomes
		use study1_2, clear
		drop if tasa2==88
		drop if arm==2|arm==4
		xi: reg tasa2 arm3 arm5 arm6 arm7 female i.ageRecode i.region ruleoflaw_index_r legitimacyscale if arm8==0 
		xi: reg don1 arm3 arm5 arm6 arm7 female i.ageRecode i.region ruleoflaw_index_r legitimacyscale if arm8==0	

		
* IV. Section 7: Study 2
		use study1_2, clear
		drop if tasa2==88
		keep if arm==2|arm==4 // We are only studying the people whose priors we elicited.

		*Generate interaction variables
			gen informXprior_above= inform*prior_above
			tab arm prior_above, row //The prior information is basially balanced across treatment and control

		*Table 4 and Table 12(Appendix)
			reg tasa2 inform prior_above informXprior_above
			reg tasa2 inform prior_above informXprior_above female i.ageRecode i.region ruleoflaw_index_r 
			reg don1 inform prior_above informXprior_above 
			reg don1 inform prior_above informXprior_above female i.ageRecode i.region ruleoflaw_index_r	

		
		
*V. Section 8: Study 3
	use study3, clear
		
	*A. Without taking account of prior beliefs
	
	*Table 5
	reg avg_use study // Effect of Learning the Rate
	reg avg_use study prior_above studyXabove 
	estat vce
		*Effect for Priors Below 30%: -.33 
			*Variance of Effect : .053
			*SE of Effect		: .23
			*95% CI				: (-.78, .12 )
			*90% CI				: (-.70, .048 )	
		
		*Effect for Priors Above 30%: .28
			*Variance of Effect	: var(study) + 1(var(studyXabove) + 2(cov(study,studyXabove))
			*Variance of Effect	: .053 + .074 + 2(-.053) = .021
			*SE of Effect		: .145
			*95% CI				: (0, .56)
			*90% CI				: (.04,.52)
	
	
	
	*Table 13 (Appendix)
	reg avg_use study
	reg avg_use study female i.ageRecode i.nse  rol j_legitimacy
	reg avg_use study prior_above studyXabove 
	reg avg_use study prior_above studyXabove female i.ageRecode i.nse  rol j_legitimacy
	/*So, model with controls is a little different. But we need to be very careful with the model with controls. Missing data is not at random.  
	Where RoL and Legitimacy are missing, subjects are very likely to have prior beliefs above the true rate. In other words, 
	when we fit these models with controls, we are losing people who expected a rate above
	what we estimated. */ 
	
	*Missing Data Analysis
	sum prior_above if rol==.
	sum prior_above if rol~=.
	sum prior_above if j_legitimacy==.
	sum prior_above if j_legitimacy~=.
	

	*C. Balance 
	bysort study: tab prior_above // As before we are very balanced across study and control with respect to prior beliefs
	reg study prior_above
		
	*D. Here we consider whether rule of law, legitimacy, or judicial trust might be confounders for prior beliefs. 
	reg prior_above rol // Rule of law 
	reg prior_above j_legitimacy  // Judicial legitimacy 
	reg prior_above conjuez  //Priors more likely to be above 30% as trust in judges increases, though the relationship is weak
	reg avg_use study prior_above studyXabove conjuez //Controlling for judicial trust. 
	

* VI. Appendix (Results not already included and noted above.)
	
	*Balance
	*Table 7, 8, and 9
	use study1_2, clear
	drop if tasa2==88
	bysort arm: sum(age)
	bysort arm: sum(female)
	bysort arm: sum(low_legitimacyscale)
	bysort arm: sum(ruleoflaw_index)
	bysort arm: sum(gsstrust)
				
	use	study3, clear	
	bysort study: sum(age)
	bysort study: sum(female)
	bysort study: sum(j_legitimacy)
	bysort study: sum(rol)
		
		
	*Table 10: Manipulation Checks for Studies 1 and 2
	use study1_2, clear
		bysort arm: sum(tutela)
		bysort arm: sum(nocomply_rate)
		bysort arm: sum(vague_orders)
		bysort arm: sum(high_cost)
		bysort arm: sum(low_ed)
		bysort arm: sum(no_information)
	
		*Here are some difference in proportion tests - they are all highly significant
		prtest tutela, by(inform)
		prtest nocomply_rate, by(inform)
		prtest vague_orders, by(arm5)
		prtest high_cost, by(arm6)
		prtest low_ed, by(arm7)		

	*Results with Controls are replicated above in the context of the analysis in the text. 		

	*Table 14
	use study1_2, clear
	drop if tasa2==88
	drop if arm==1|arm==2|arm==4
	tab votesol						// Full Sample 
	tab votesol if tasa2<=3			// Found Rate Unacceptable */
	tab votesol if arm==5 			// Learned about Judge Vagueness*/
	use study1_2, clear
	keep if arm==2|arm==4
	tab votesol if tasa2<=3& prior_above==0 //People who had priors below 30 and an Acceptability score no higher than 3.

