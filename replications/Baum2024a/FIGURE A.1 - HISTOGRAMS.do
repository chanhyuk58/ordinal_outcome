
***TO RUN REMOVE THE "*" BEFORE "CD" AND INSERT THE NAME OF THE DIRECTORY WITH THE RECODED DATA.

*cd "..."

use "November Data File (for replication)_recoded.dta",clear

histogram violence051, discrete percent xlabel(0(.5)1) title(Hypothetical Violence-November Wave)

use "January Panel Data File (for replication)_recoded.dta",clear

histogram violence051, discrete percent xlabel(0(.5)1) title(Hypothetical Violence-January Wave)

histogram supportstormers01, bin(10) percent title(Support Capitol Riot-January Wave)
