clear

global path "C:\Users\Public\SowVallette"
global path_brest "\ExportedStataData\BREST"
global path_fh "\ExportedStataData\FH"
global path_mmo "\ExportedStataData\MMO"

*2022
use "${path}\modified\id_fh_brest.dta", clear

merge 1:m id_force using "${path}${path_mmo}\MMO_2_T22022_F10.dta"
keep if _merge == 3
drop _merge

save "${path}\modified\mmo_2022_id_fh.dta", replace

*2021
use "${path}\modified\id_fh_brest.dta", clear

merge 1:m id_force using "${path}${path_mmo}\MMO_2_2021_F10.dta"
keep if _merge == 3
drop _merge

save "${path}\modified\mmo_2021_id_fh.dta", replace

*2020
use "${path}\modified\id_fh_brest.dta", clear

merge 1:m id_force using "${path}${path_mmo}\MMO_2_2020_F10_VF.dta"
keep if _merge == 3
drop _merge

save "${path}\modified\mmo_2020_id_fh.dta", replace

*2019
use "${path}\modified\id_fh_brest.dta", clear

merge 1:m id_force using "${path}${path_mmo}\MMO_2_2019_F10.dta"
keep if _merge == 3
drop _merge

save "${path}\modified\mmo_2019_id_fh.dta", replace

*2018
use "${path}\modified\id_fh_brest.dta", clear

merge 1:m id_force using "${path}${path_mmo}\MMO_2_2018_F10.dta"
keep if _merge == 3
drop _merge

save "${path}\modified\mmo_2018_id_fh.dta", replace

*2017
use "${path}\modified\id_fh_brest.dta", clear

merge 1:m id_force using "${path}${path_mmo}\MMO_2_2017_F10.dta"
keep if _merge == 3
drop _merge

save "${path}\modified\mmo_2017_id_fh.dta", replace


/* merge */
clear all
use "${path}\modified\mmo_2017_id_fh.dta"
append using "${path}\modified\mmo_2018_id_fh.dta"
append using "${path}\modified\mmo_2019_id_fh.dta"
append using "${path}\modified\mmo_2020_id_fh.dta"
append using "${path}\modified\mmo_2021_id_fh.dta"
append using "${path}\modified\mmo_2022_id_fh.dta"

bys id_force DebutCTT FinCTT: gen dup = cond(_N==1,0,_n)
drop if dup > 1
drop dup
drop if DebutCTT == ""

*bys id_force: replace training = training[_N] if missing(training)
bys id_force: egen training_complete = max(training)
// if the individual is trained, all obs are training = 1
replace training_complete = 0 if missing(training_complete)
drop training
rename training_complete training
*replace training = 0 if missing(training) // all the other are in the control group

save "${path}\modified\mmo_merged_id_fh.dta", replace

use "${path}\modified\mmo_merged_id_fh.dta", clear

/* keep only one contract at the same time */
gen start_contract = date(DebutCTT, "YMD")
gen end_contract = date(FinCTT, "YMD")
format start_contract end_contract %td

sort id_force start_contract

bys id_force: gen overlap = 1 if start_contract < end_contract[_n - 1] & end_contract != . & _n > 1
// overlapping contracts
bys id_force: gen double_ctt = 1 if end_contract < end_contract[_n - 1] & end_contract != . & overlap == 1
// short contract within a longer one, we keep only the longer

drop if double_ctt == 1
drop double_ctt

save "${path}\modified\mmo_merged_id_fh.dta", replace


