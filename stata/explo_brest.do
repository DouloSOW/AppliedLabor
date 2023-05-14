clear

global path "C:\Users\Public\SowVallette"
global path_brest "\ExportedStataData\BREST"
global path_fh "\ExportedStataData\FH"
global path_mmo "\ExportedStataData\MMO"

use "${path}${path_brest}\brest.dta"

/* 4729951 obs */
*describe
*mdesc
destring ANNEE_ENTREE, replace
/* Keep only trainings starting between 2017 and 2019 

egen OK = anymatch(ANNEE_ENTREE), values(2017 2018 2019)
keep if OK // only 2474072 obs
drop OK
*/

/* 2 measures of duration */
gen duration_day = datediff(DATE_ENTREE, DATE_FIN, "day")
gen training_censored = (DATE_FIN == .)
count if training_censored==1 // 4,362 censored
rename DUREE_FORMATION duration_hours

/* Drop if no ID_FORCE */
count if strpos(id_force, "FORCE") == 0
drop if strpos(id_force, "FORCE") == 0 // 85,712 obs drop

/* Incoherences */
gen incoherence = ((DATE_ENTREE > DATE_FIN) | (duration_hours <= 0)) 
count if incoherence == 1 // no obs
drop incoherence

/* Missing */

count if strpos(OBJECTIF_STAGE, "99") == 1
count if strpos(OBJECTIF_STAGE, "9") == 1
count if DOMAINE_FORMATION == ""
count if strpos(DOMAINE_FORMATION, "NON RENSEIGNE") == 1

gen missing = 1 if strpos(OBJECTIF_STAGE, "99") == 1 | strpos(OBJECTIF_STAGE, "9") == 1 | DOMAINE_FORMATION== "" | strpos(DOMAINE_FORMATION, "NON RENSEIGNE") == 1
*drop if strpos(OBJECTIF_STAGE, "99") == 1

*drop if strpos(OBJECTIF_STAGE, "9") == 1

*tab DOMAINE_FORMATION

*drop if DOMAINE_FORMATION == "" | strpos(DOMAINE_FORMATION, "NON RENSEIGNE") == 1
drop if missing == 1

/* Duplicate */

egen ut = min(DATE_ENTREE), by(id_force OBJECTIF_STAGE DOMAINE_FORMATION) // transition unemployment-training minimum date
egen tu = max(DATE_FIN), by(id_force OBJECTIF_STAGE DOMAINE_FORMATION) // transition training-unemployment max date

bys id_force OBJECTIF_STAGE DOMAINE_FORMATION: gen dup = cond(_N==1,0,_n)
drop if dup > 1 // drop duplicate with same start and end date for training with same objective and sector

* find overlap if latest transition from unemployment to training start after earliest transition from training to unemployment
bys id_force: egen max_ut = max(ut)
bys id_force: egen min_tu = min(tu)
bys id_force: gen nb_form = _N // can overlap only if several trainings

bys id_force: gen overlap = (max_ut < min_tu & nb_form > 1)

* in case of overlap, keep only longest training
gen duration_day_revised = tu - ut
bys id_force: egen longest = max(duration_day_revised)
gen todrop = (overlap == 1 & longest != duration_day_revised)
drop if todrop == 1

save "${path}\modified\brest_1form.dta", replace


/* get unique id of brest */
use "${path}\modified\brest_1form.dta", clear

keep id_force
bys id_force: gen dup = cond(_N==1,0,_n)
drop if dup > 1
drop dup

save "${path}\modified\brest_id.dta"

/* reduce sample */
use "${path}\modified\id_fh_brest.dta", clear

merge 1:m id_force using "${path}\modified\brest_1form.dta"
keep if _merge == 3
drop _merge

drop todrop longest overlap max_ut min_tu dup VB_IDENT missing

save "${path}\modified\brest_reduced.dta"




/*
bys id_force OBJECTIF_STAGE DOMAINE_FORMATION: gen nb_form_dom = _N

bys id_force: gen obsstotal=_N
count if obsstotal>1

bys id_force DATE_ENTREE: gen obss=_N
*br id_force DATE_ENTREE if obss>1
count if obss>1 // 73,425
*preserve
*bys id_force DATE_ENTREE: keep if _n == 1
bys id_force DATE_ENTREE: gen obss_n=_n
tab obss_n obss
count if obss_n>1 
keep if obss_n==1
*restore




*unab vlist : _all
sort DATE_ENTREE DATE_FIN 
quietly by DATE_ENTREE DATE_FIN : gen dup = cond(_N==1,1,0)
count if dup == 1

gen date_start = date(DATE_ENTREE, "YMD")
format %td date_start



count if duration < DUREE_FORMATION

drop if DATE_ENTREE



count if strpos(id_force, "FORCE") == 1

replace id_force = . if id_force != ""
describe
mdesc
gen debut_ctt_year = date()
destring DebutCTT

gen debut_ctt_year = substr(DebutCTT, 1, 4)
destring debut_ctt_year, replace
tab debut_ctt_year
*/







