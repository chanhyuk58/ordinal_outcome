***TABLE A.1***

***TO RUN REMOVE THE "*" BEFORE "CD" AND INSERT THE NAME OF THE DIRECTORY WITH THE RECODED DATA.

*cd "..."

use "January Panel Data File (for replication)_recoded.dta",clear

svyset [pw=weight_nat]

svy: mean phq9 misperceptionssum12 socialmediaelecnewssum10 supportstormers01 el_conf violence051 interest anypolactive rep dem ind white black hispanic asian  income_r libcon edu_r age male trumpsupporter


***TABLE A.2***
use "November Data File (for replication)_recoded.dta",clear

svyset [pw=weight_nat]

svy: mean phq9 misperceptionssum12 el_conf violence051 interest anypolactive rep dem ind white black hispanic asian  income_r libcon edu_r age male trumpsupporter
