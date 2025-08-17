/*==================================================
project:       Testing the Compensatory Theory: 
               A Survey Experiment on Covid-19 
			   and Redistributive Preferences in the UK
Author:        Pablo Querubin, Alan David Gomez 
E-email:       ad.gomezb@uniandes.edu.co
Dependencies:  New York University
----------------------------------------------------
Creation Date: 19 Mar 2024 - 07:23:58 
Output:        OUTREP and EPS_OUTREP             
==================================================*/

/*==================================================
              0: Program set up
Create OUTREP EPS_OUTREP and GPT folders for proper
 and organized execution
==================================================*/
drop _all
clear all 

** Manually set relative path
global dir0 "..\\..\\JOP Replication\Replicator\"

** Global of Output
global output "${dir0}OUTREP\"

** Global of Output for EPS files 
global outeps "${dir0}EPS_OUTREP\"

global GPT "${dir0}GPT\"

** Install programs
*ssc install swindex
*findit lean1
*ssc install ranktest
*ssc install ivreg2
*ssc install ftools 
*ssc install reghdfe
*ssc install ivreghdfe
*ssc install coefplot, replace
*net install gr0002_3, from(http://www.stata-journal.com/software/sj4-3)
*ssc install outreg2
*ssc inst outreg, replace

set scheme lean2

/*==================================================
                  Execute dofiles
==================================================*/

*clean data
do "${dir0}1_data_cleaning.do"

*run main analysis
do "${dir0}2_main_analyisis.do"

*run analysis for the appendix
do "${dir0}3_appendix_analysis.do"

*run analysis on open answers
do "${dir0}4_gpt_stats.do"

** End of the dofiles

exit
