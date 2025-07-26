The Political Consequences of Depression: How Conspiracy Beliefs, Participatory Inclinations, and Depression Relate to Support for Political Violence

Matthew A. Baum, Harvard University, matthew_baum@hks.harvard.edu 
James N. Druckman, Northwestern University, druckman@northwestern.edu 
Matthew Simonson, University of Pennsylvania, msimonso@sas.upenn.edu 
Jennifer Lin, Northwestern University, JenniferLin2025@u.northwestern.edu 
Roy H. Perlis, Harvard Medical School, rperlis@mgh.harvard.edu


All data analyses conducted using Stata/MP version 17.0 for Mac and Mac OS version 12.6.3

* Note: In order to load, save, and use the below-referenced data files, user will need download the data and do files into a directory and then either work strictly out of that directory or enter the relevant directory in the do files after the "cd" command (as explained in those files).
DATA FILE CONTENT* Filename: “November Data File (for replication)”: this file contains all variables necessary to run the November models.* Filename: “January Panel Data File (for replication)”: this file is the January panel dataset and contains all variables necessary to run the January models.

RECODING FILE CONTENT
* Filename: “variable recoding for November data file for depression-riot paper”: this file contains all variable recoding necessary to run the November models.* Filename: “variable recoding for January panel data file for depression-riot paper”: this file is the January panel dataset and contains all variable recoding necessary to run the January models.*Note that throughout the do files, any statistical model/Clarify simulation that references November data would use the November Data File, while any statistical model that references January data would use the January Panel Data File. Because many of the tables include models employing both the November and January data files, rather than separate the do files by table/figure numbers, the table and/or figure to which each model applies (and where it is located) is described in detail within the do files at the top of each regression/clarify simulation.DO FILE CONTENT(Note that “A.” designation indicates that a figure or table appears in the online supplemental appendix, rather than in the main paper)* Filename: “TABLE A.1 and TABLE A.2 - SUMMARY STATISTICS.do” – contains models for summary statistics tables* Filename: “Table A.3 - All Models.do” – contains models for Table A.3 & Figures 1 & 2* Filename: “Table A.4 - All Models.do” – contains models for Table A.4 & Figures A.2 and A.3* Filename: “TABLE A.5 - All Models.do” – contains models for Table A.5 & Figures 3 & 4
* Filename: “TABLE A.6 - All Models.do” – contains models for Table A.6 & Figures 3 & 4
* Filename: “TABLE A.7 - All Models.do” – contains models for Table A.7 & Figures 5 & 6
* Filename: “TABLE A.8 - All Models.do” – contains models for Table A.8 & Figures 5 & 6
* Filename: “TABLE A.9 - All Models.do” – contains models for Table A.9 & Figures 7 & 8
* Filename: “TABLE A.10 - All Models.do” – contains models for Table A.10 & Figures 7 & 8
* Filename: “TABLE A.11 - All Models.do” – contains models for Table A.11 & Figures A.4, A.5. A.6, & A.7
* Filename: “TABLE A.12 - All Models.do” – contains models for Table A.12 & Figures A.4, A.5. A.6, & A.7 
* Filename: “TABLE A.13 - All Models.do” – contains models for Table A.13 & Figures A.8, A.9. A.10, & A.11
* Filename: “TABLE A.14 - All Models.do” – contains models for Table A.14 & Figures A.8, A.9. A.10, & A.11
* Filename: “TABLE A.15 - All Models.do” – contains models for Table A.15 & Figures A.12, A.13. A.14, & A.15
* Filename: “TABLE A.16 - All Models.do” – contains models for Table A.16 & Figures A.12, A.13. A.14, & A.15
* Filename: “FIGURE A.1 - HISTOGRAMS.do” – contains code for constructing Figure A.1


ADDITIONAL FILES
* Filename: "full set of model runs.txt" - contains a complete set of Stata runs for all regression models employed in this paper
* Filename: "Codebook.pdf" - contains the codebook for constructing all variables necessary to replicate all analysesMAPPING OF FIGURES TO TABLESFigure 1: Table A.3Figure 2: Table A.3Figure 3: Dems-Table A.5; Reps-Table A.6 (2-way interactions)Figure 4: Dems-Table A.5; Reps-Table A.6 (3-way interactions)Figure 5: Trump Supporters-Table A.7; Trump Non-Supporters-Table A.8 (2-way interactions)Figure 6: Trump Supporters-Table A.7; Trump Non-Supporters-Table A.8 (3-way interactions)Figure 7: Gender-Table A.9 (Females) and Table A.10 (Males) (2-way interactions)Figure 8: Gender-Table A.9 (Females) and Table A.10 (Males) (3-way interactions)Figure A.2: Moderate Depression-Table A.4 (2-way interactions)Figure A.3: Moderate Depression-Table A.4 (3-way interactions)Figure A.4: Party Split Sample-Dems-Table A.11; Reps-Table A.12 (2-way interactions)Figure A.5: Party Split Sample-Dems-Table A.11; Reps-Table A.12 (3-way interactions)Figure A.6: Party Split Sample-Dems-Table A.11; Reps-Table A.12 (2-way interactions)Figure A.7: Party Split Sample-Dems-Table A.11; Reps-Table A.12 (3-way interactions)Figure A.8: Trump Support Split Sample-Supporter-Table A.13; Non-supporter-Table A.14 (2-way interactions)Figure A.9: Trump Support Split Sample-Supporter-Table A.13; Non-supporter-Table A.14 (3-way interactions)Figure A.10: Trump Support Split Sample-Supporter-Table A.13; Non-supporter-Table A.14 (2-way interactions)Figure A.11: Trump Support Split Sample-Supporter-Table A.13; Non-supporter-Table A.14 (3-way interactions)Figure A.12: Gender-Table A.15 (Females) and Table A.16 (Males) (2-way interactions)Figure A.13: Gender-Table A.15 (Females) and Table A.16 (Males) (3-way interactions)Figure A.14: Gender-Table A.15 (Females) and Table A.16 (Males) (2-way interactions)Figure A.15: Gender-Table A.15 (Females) and Table A.16 (Males) (3-way interactions)Please also note that in using Clarify, we allow it to select random seed numbers and thus there may be slight differences on each run (e.g., relative to the manuscript). As the Clarify codebook states, “Slight discrepancies are a result of taking a finite number of simulations and using a different random number seed."


GENERATING FIGURES

DATA FILE CONTENT* Filename: "Stata Output for R - Main Figures.xlsx": this file contains output from the Stata clarify simulations, manually copied and pasted into an excel file. All data needed for the figures in the main paper are contained in this file.
* Filename: "Stata Output for R - SI Figures.xlsx": this file is identical to the one above, except that it contiains the data used in the SI figures.

R FILE CONTENT
* Filename: “make_figures.R" generates the figures from the above data files.

PROCEDURE

With the exception of Figure A.1 (which, as mentioned above, was generated by “FIGURE A.1 - HISTOGRAMS.do”), all other figures were generated by first running Clarify simulations in Stata, then pasting the results into an Excel spreadsheet, and then uploading the spreadsheet into R to make the plots.

* Used R version 4.1.1 in RStudio 2023.03.0+386 on a Mac running the Venture 13.1 operating system. The following R packages must be installed  * tidyverse 1.3.1  * readxl 1.4.0  * patchwork 1.1.1  * ggh4x 0.2.3  * ggstance 0.3.5

Note that earlier and later versions will likely also work.

* Our first step was to take the predicted values from Stata and create an Excel spreadsheet called: "Stata Output for R - Main Figures.xlsx" into which we manually entered predicted values and 95% confidence intervals (after which we had a second individual manually double-check all values). Likewise, we manually pasted values intended for the SI into an Excel spreadsheet called: "Stata Output for R - SI Figures.xlsx".

* To generate the figures, run the R file "make_figures.R" using RStudio. This file pulls data from the above mentioned spreadsheets and outputs figures into the "/figures" directory. SI figures are out put to the "/figures/SI" subdirectory. Please ensure that the working directory is set to the source file location before running the code.

* The script names the figures files according to how they are numbered in the article (e.g., Figure_2.png).  

Note: Figures 1, 3, 5, and 7 are composed of two image files, one for the top panel and one for the bottom panel (e.g., Figure_1a.png and Figure_1b.png). In each case, the two images were simply inserted one after another in the Word Document to create the appearance of a single figure.