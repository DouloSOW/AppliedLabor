clear

global path "C:\Users\Public\SowVallette"

/* pour tester le merge, je me restreints juste aux données date d'évennements pour voir la
cohérence de l'historique qu'on a pour un individu.
Il y a clairement des trous et des chevauchements donc faudra décider de quelles règles on choisit */

use "${path}/modified/fh_reduced.dta"

*keep id_force idx Region training eu out

append using "${path}/modified/brest_reduced.dta"

*keep id_force idx Region ID_BREST training eu out ut tu

append using "${path}/modified/mmo_merged_id_fh.dta"

*keep id_force idx Region ID_BREST IdSISMMO training eu out ut tu start_contract end_contract
*sort id_force
format eu out %td
format tu ut %td

gen event = eu if eu != .
replace event = ut if ut != .
replace event = start_contract if start_contract != .
format event %td

*order  id_force idx Region ID_BREST IdSISMMO event training eu out ut tu start_contract end_contract
sort id_force event

rename out out_pe // out is the date where the individual is out of Pole Emploi

expand 2 if ut != ., gen(dupindicator)
*sort id_force event
replace event = tu if dupindicator == 1
drop dupindicator

expand 2 if end_contract != ., gen(dupindicator)
sort id_force event
replace event = end_contract if dupindicator == 1


*gen type_event = 0 if (event == eu & eu != . ) | (event == end_contract & end_contract!=. & end_contract - start_contract >= 31) // 0 = transition from employment to unemployment
replace type_event = 0 if (event == eu & eu != . ) | (event == end_contract & end_contract!=. & end_contract - start_contract >= 31)
replace type_event = 1 if event == ut&ut!=. // 1 = transition from unemployment to training
replace type_event = 2 if event == tu&tu!=. // 2 = transition from training to unemployment
replace type_event = 3 if event == start_contract &  start_contract!=. & ((end_contract - start_contract >= 31 & end_contract != .) | end_contract == .) // 3 = transition from unemployment to employment 

bys id_force: replace type_event = 4 if event == start_contract & start_contract != . & event[_n-1] == start_contract[_n-1] & start_contract <= end_contract[_n - 1] + 31 & end_contract[_n - 1] !=. & ((end_contract - start_contract >= 31 & end_contract != .) | end_contract == .) // employment to employment transition

bys id_force: replace type_event = 5 if event == start_contract & start_contract != . & end_contract - start_contract < 31
// start short contract

bys id_force: replace type_event = 6 if event == end_contract & end_contract != . & end_contract - start_contract < 31 & dupindicator == 1
// end short contract
drop dupindicator

//PS: Nous avons des cas où event=.

label define type_event 0 "0. employment to unemployment" 1 "1. unemployment to training"  2 "2. training to unemployment"  3 "3. unemployment to employment" 4 "4. employment to employment" 5 "5. Start short contract" 6 "6. End short contract"
label values type_event type_event

by id_force: gen nRecordedEvents = _N
by id_force: gen temp = sum(type_event == 5)
by id_force: egen nShortContract = max(temp)
drop temp
by id_force: gen temp = sum(type_event == 1)
by id_force: egen nTraining = max(temp)
drop temp

*save "${path}/modified/small_merge.dta", replace


*use "${path}/modified/small_merge.dta", clear

sort id_force event

drop if type_event == 5 | type_event == 6 // drop short contract

// our sample start with the employment-unemployment transition
bys id_force: egen entry = min(cond(type_event == 0 & event >= 20820,event,.)) // first entry in unemployment after Jan 1st 2017
format entry %td

bys id_force: gen temp = cond(type_event == 0 & event < 20820,1,0)
bys id_force: egen prev_unemployed = max(temp) // dummy for being previously unemployed
drop temp

// our sample start with events in 2017
drop if event < entry

*drop if event < 20820 // 01jan17
*gen year_entry = year(entry) // our sample start with one entry in 2017 
*drop if year_entry != 2017
/*
bys id_force: gen obss = _n
tab type_event if obss == 1 // still some individuals with a first obs =/= of employment-unemployment transition
drop if obss == 1 & type_event == 3 & type_event[_n+1] == 0
drop if obss == 1 & type_event == 3 & type_event[_n+1] == 3
drop if obss == 1 & type_event == 3 & type_event[_n+1] == 4
drop if obss == 1 & type_event == 4 & type_event[_n+1] == 0

bys id_force: gen temp = cond(obss == 1 & type_event != 0,1,0)
bys id_force: egen todrop = max(temp)
drop if todrop > 0
drop temp todrop obss

bys id_force: gen obss = _n
drop if obss == 1 & type_event == 3 & type_event[_n+1] == 0
drop if obss == 1 & type_event == 4 & type_event[_n+1] == 0
bys id_force: gen temp = cond(obss == 1 & type_event != 0,1,0)
bys id_force: egen todrop = max(temp)
drop if todrop > 0
drop temp todrop obss
*/
bys id_force: gen obss = _n
tab type_event if obss == 1 // all individuals start with 


// our sample ends with the first transition to employment

bys id_force: egen first_out = min(cond(type_event == 3,event,.))
format first_out %td
/*
bys id_force: egen last_training = max(cond(type_event == 2, event, .))
format last_training %td
bys id_force: egen first_out = min(cond(type_event == 3 & ((event > last_training & training == 1) | training == 0),event,.))
format first_out %td
*/
bys id_force: gen outofsample = cond(event > first_out,1,0)
bys id_force: gen temp = cond(type_event == 0 & event > first_out,1,0)
bys id_force: egen post_unemployed = max(temp) // dummy for being unemployed again 
drop if outofsample == 1
drop outofsample temp

// if 2 type_event = 0, keep information on pole emploi (first 0 would be from end_contract, second will be the registration at PE)
bys id_force: gen double0 = cond(type_event == 0 & type_event[_n - 1] == 0,1,0)
drop if double0 == 1
drop double0

bys id_force: gen temp = cond(obss == 1 & event != eu,1,0)
bys id_force: egen startpe = max(temp) // dummy if the individual first obs is from FH
drop temp

// missing event = only training with no end date, just drop these obs

bys id_force: gen temp = cond(missing(event),1,0)
bys id_force: egen todrop = max(temp)
drop if todrop
drop todrop

*save "${path}/modified/small_merge_v2.dta", replace
save "${path}/modified/data_merged.dta", replace



