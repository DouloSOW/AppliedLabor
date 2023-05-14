clear

global path "C:\Users\Public\SowVallette"

use "${path}\modified\data_to_use.dta", clear

recode AGE (min/26 = 1)  (26/51 = 2)  (51/max = 3), gen(age_recod)
label define age_recod 1 "under 26y" 2 "Between 26 and 50y" 3 "50+"
label values age_recod age_recod

label define women 0 "Male" 1 "Female"
label values women women

gen nivdipl_rec = 0 if inlist(NIVFOR, "AFS", "C12", "C3A", "CFG", "CP4")
replace nivdipl_rec = 1 if NIVFOR == "NV5"
replace nivdipl_rec = 2 if NIVFOR == "NV4"
replace nivdipl_rec = 3 if inlist(NIVFOR, "NV3", "NV2", "NV1")

label define nivdipl_rec 0 "No diploma" 1 "CAP / BEP" 2 "BAC"  3 "BAC+"
label values nivdipl_rec nivdipl_rec

gen single = 1-(SITMAT=="K" |SITMAT=="M")
label define single  1 "Yes" 0 "No"
label values single single 

gen foreign = 1 - (NATION == "01")
label define foreign  1 "Yes" 0 "No"
label values foreign foreign 

gen OBJECTIF_STAGE_rec = substr(OBJECTIF_STAGE ,1,1)
destring OBJECTIF_STAGE_rec, replace

label define OBJECTIF_STAGE_rec 1 "certification" 2 "professionnalisation" 3 "pré-qualification"  4 "adaptation au poste de travail" 5 "remise à niveau" 6 "mobilisation" 7 "perfectionnement" 8 "création d'entreprise" 9 "Inconnu"
label values OBJECTIF_STAGE_rec OBJECTIF_STAGE_rec

recode duration_hours (min/120 = 1)  (120/400 = 2)  (400/700 = 3) (700/max = 4), gen(duration_hours_rec)
label define duration_hours_rec 1 "Less than 120h" 2 "Between 120 and 400h" 3 "Between 400 and 700h" 4 "More than 700h"
label values duration_hours_rec duration_hours_rec

recode duration_training (min/31 = 1)  (31/90 = 2)  (90/180 = 3) (180/365 = 4) (365/max = 5), gen(duration_training_rec)
label define duration_training_rec 1 "Less than 1m" 2 "Between 1 and 3m" 3 "Between 3 and 6m" 4 "Between 6 and 12m" 5 "More than a year"
label values duration_training_rec duration_training_rec

gen un_pre_training = ut - event if training2 == 1
replace un_pre_training = duration_unemployment if training2 == 0

save "${path}\modified\data_to_use_label.dta", replace

numlabel, add
foreach var of varlist  women foreign nivdipl_rec age_recod{
	dis "-------------------`var'----------------------------"
	asdoc tab `var' training2, col  save(summary.doc) append title(Descriptive statistics `var' )
}

foreach var of varlist  OBJECTIF_STAGE_rec DOMAINE_FORMATION duration_hours_rec duration_training_rec {
	dis "-------------------`var'----------------------------"
	asdoc tab `var', save(summary.doc) append title(Descriptive statistics `var' )
}

tabstat OBJECTIF_STAGE_rec DOMAINE_FORMATION duration_hours_rec duration_training rec if training2 == 1

eststo test: estpost tab women training2
esttab test using "${path}\tex\test.tex", cells("pct(fmt(2))") replace
cd "${path}"
asdoc tab1 women foreign nivdipl_rec age_recod, append


stset duration_un_net, failure(censored == 1)
est clear
eststo: stcox i.training2 i.women i.foreign i.nivdipl_rec i.age_recod i.single i.prev_unemployed
esttab using "${path}\tex\cox_net_duration.tex", eform b se 

stset duration_unemployment, failure(censored == 1)
est clear
eststo: stcox i.training2 i.women i.foreign i.nivdipl_rec i.age_recod i.single i.prev_unemployed
esttab using "${path}\tex\cox_brut_duration.tex", eform b se 


