clear

global path "C:\Users\Public\SowVallette"
global path_brest "\ExportedStataData\BREST"
global path_fh "\ExportedStataData\FH"
global path_mmo "\ExportedStataData\MMO"

use "${path}\modified\brest_id.dta"
gen training = 1

merge 1:m id_force using "${path}\modified\fh_2017_v2.dta"

keep id_force training _merge
drop if _merge == 1 // drop if id from Brest are not matched to FH
replace training = 0 if _merge == 2
drop _merge

bys id_force: gen dup = cond(_N==1,0,_n)
drop if dup > 1
drop dup

gen random = runiform()
sort training random
by training: gen group = 1 + (_n > 500000)
keep if group == 1

drop random group

save "${path}\modified\id_fh_brest.dta"