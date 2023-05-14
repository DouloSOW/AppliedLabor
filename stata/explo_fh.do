clear

global path "C:\Users\Public\SowVallette"
global path_brest "\ExportedStataData\BREST"
global path_fh "\ExportedStataData\FH"
global path_mmo "\ExportedStataData\MMO"

use "${path}${path_fh}/DE_TEMPO_reduced.dta", clear

//On garde seulement ceux qui sont inscrits à pe durant 2017
egen tokeep = max(year(DATINS) == 2017), by(id_force)

keep if tokeep
drop tokeep
*preserve

bys id_force: gen nobs = _N
*tab nobs
*tab MOTANN

replace MOTANN = "" if MOTANN == "XX"
destring MOTANN, replace
* identify real transition to employment
//gen ue = DATANN if MOTANN >= 11 // not reliable --> identify real transition thx to MMO

* identify real entry to unemployment (min entry after last transition out)
sort id_force DATINS
bys id_force: gen nobs2 = _n
*bys id_force ue: gen nobs_sortie = _N

by id_force: gen eu = DATINS //if nobs == 1
sort id_force DATINS
replace eu = eu[_n - 1] if nobs2 > 1 & eu[_n - 1] != . & DATINS <= DATANN[_n - 1] + 31
/* transition from employment to unemployment only gap of more than 31 days since last leave */

* identify transition out of FH register
by id_force: gen out = DATANN
sort id_force DATINS
replace out = DATANN[_n+1] if DATANN + 31 >= DATINS[_n + 1] & id_force == id_force[_n+1]

bys id_force eu out: gen dup = cond(_N==1,0,_n)
drop if dup > 1
* keep if max(out) for an eu

bys id_force eu: egen maxout = max(out)
gen tokeep = (out == maxout)
keep if tokeep
drop tokeep
gen censored = (DATANN == 22918) // censored after sept 2022
/*
*gen duree_chomage = ue - eu


* keep only obs with real entry or real exit and not censored
count if ue == . & eu == .

count if censored == 1
count if MOTANN < 11 | (MOTANN == . & censored == 0)

drop if MOTANN < 11 | (MOTANN == . & censored == 0)
*/
save "${path}\modified\fh_2017_v2.dta", replace

/* 
/* reduce size of dataset */
use "${path}\modified\brest_id.dta"

merge 1:m id_force using "${path}\modified\fh_2017_v2.dta"
drop if _merge ==1 // drop if id not in fh

/* next step would be to keep randomly part of observations _merge == 2 (in FH but no trainings) and part of obs _merge == 3 (those who did some trainings) */

/* TO DO */ --> done in resampling.do


/* then keep only id_force and save id */

keep id_force
bys id_force: gen dup = cond(_N==1,0,_n)
drop if dup > 1
drop dup

save "${path}\modified\fh_brest_id.dta"
/* then run explo_mmo with this id list */
*/
/* reduce size of dataset */
use "${path}\modified\id_fh_brest.dta"
merge 1:m id_force using "${path}\modified\fh_2017_v2.dta"
keep if _merge == 3
drop _merge maxout dup

save "${path}\modified\fh_reduced.dta", replace
