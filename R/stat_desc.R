library(haven)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(DescTools)
library(tidyr)
library(data.table)
library(stats)
library(zoo)

library(survival)
library(ggsurvfit)
library(lubridate)
library(gtsummary)
library(pch)
library(eha)
library(visreg)
library(casebase)
library(NPHazardRate)
setwd('C:/Users/Public/SowVallette')
getwd()

data = read_stata(file = "./modified/data_merged.dta")

ls(data)

# [1] "AGE"                    "AGE_ENTREE_STAGE"      
# [3] "ANNEE_ENTREE"           "ANNEE_NAISSANCE"       
# [5] "censored"               "CODE_POSTAL"           
# [7] "COMMANDITAIRE"          "COMMUNE"               
# [9] "CONTRAT"                "CPF_autonome"          
# [11] "CPF_PE"                 "DATANN"                
# [13] "DATE_ENTREE"            "DATE_FIN"              
# [15] "DATINS"                 "DebutCTT"              
# [17] "DEPARTEMENT_HABITATION" "DIPLOME"               
# [19] "DISPOSITIF"             "DISPOSITIF_TR"         
# [21] "DOMAINE_FORMATION"      "duration_day"          
# [23] "duration_day_revised"   "duration_hours"        
# [25] "end_contract"           "entry"                 
# [27] "eu"                     "event"                 
# [29] "FinCTT"                 "first_out"             
# [31] "FORMACODE"              "ID_BREST"              
# [33] "id_force"               "IdSISMMO"              
# [35] "idx"                    "last_training"         
# [37] "MOIS_NAISSANCE"         "MOTANN"                
# [39] "MOTINS"                 "NATION"                
# [41] "Nature"                 "nb_form"               
# [43] "NDEM"                   "NENF"                  
# [45] "NIV_DIPLOME"            "NIV_DIPLOME_TR"        
# [47] "NIVFOR"                 "nobs"                  
# [49] "nobs2"                  "nRecordedEvents"       
# [51] "nShortContract"         "nTraining"             
# [53] "OBJECTIF_STAGE"         "obss"                  
# [55] "out_pe"                 "overlap"               
# [57] "PcsEse"                 "post_unemployed"       
# [59] "prev_unemployed"        "PUBLIC_PIC"            
# [61] "QPV"                    "QUALIF"                
# [63] "Region"                 "REGION_HABITATION"     
# [65] "ROME"                   "SEXE"                  
# [67] "SITMAT"                 "start_contract"        
# [69] "startpe"                "temp"                  
# [71] "training"               "training_censored"     
# [73] "TRAVAILLEUR_HANDICAPE"  "tu"                    
# [75] "type_event"             "TYPE_REMUNERATION"     
# [77] "ut"                     "year_entry" 
var_todrop <- c("DATINS", "DebutCTT", "duration_day", "duration_day_revised", "DATE_ENTREE", "DATE_FIN",
                "FinCTT", "ID_BREST", "IdSISMMO", "idx", "nobs", "nobs2",
                "obss", "overlap", "temp")
data <- data %>% select(-all_of(var_todrop))


data <- data %>% 
  mutate(type_event = as.numeric(type_event))

#data <- data %>% filter(event >= entry)
# df <- data %>% filter(event >= entry) %>% select(id_force, training, event, type_event, eu, start_contract, end_contract, ut, tu) %>% arrange(id_force, event)

#data <- data %>% mutate(duration_training_theory = as.numeric(tu - ut))
# 
# df <- df %>%
#   mutate(duration_training_theory = as.numeric(tu - ut))

# df <- df %>% arrange(id_force, event) %>% group_by(id_force) %>%
#   mutate(overlap = ifelse(type_event == 3 & lag(type_event, 1) == 3,1,0))

# df <- df %>% arrange(id_force, event) %>% group_by(id_force) %>%
#   mutate(overlap2 = ifelse(type_event == 0 & lag(type_event, 1) == 0,1,0))
# 
# df2 <- df %>% arrange(id_force, event) %>% group_by(id_force) %>%
#   filter(row_number() != 1 | (row_number() == 1 & type_event == 0))
# 
# df2 <- df2 %>% arrange(id_force, event) %>% group_by(id_force) %>%
#   filter((overlap2 == 0 | (is.na(overlap2) & row_number() == 1)) & overlap == 0)
# 
# df3 <- df2 %>% select(id_force, training, event, type_event)
# #df_wide <- df3 %>% group_by(id_force) %>% pivot_wider(id_cols = c('id_force', 'training'), names_from = type_event, values_from = event, names_expand = T)
# 
# 
# df4 <- df %>% filter(!(type_event ==2 & event > as.Date("2022-09-01")))
# 
# 
# #table(subset(data, type_event == 1)$nTraining)
# 
# 
# df5 <- df4 %>% filter(type_event == 0 | type_event == 3)
# 
# 
# df5 <- df5 %>%
#   mutate(duration_unemployment_day =
#            ifelse(type_event == 3 & lag(type_event, order_by = id_force)  == 0, event - lag(event, order_by = id_force), NA))


# df6 <- df %>% arrange(id_force, event) %>% group_by(id_force) %>%
#   mutate(cumsum = cumsum(type_event == 3))
# df6 <- df6 %>% group_by(id_force) %>% mutate(cumsum2 = cumsum(cumsum))
# df6 <- df6 %>% filter(cumsum2 < 2)
# df6 <- df6 %>% mutate(training2 = (type_event == 1)) %>% group_by(id_force) %>% mutate(training2 = max(training2))
#            
# dupdf6 <- df6 %>% group_by(id_force) %>% filter(row_number() == 1)
# table(dupdf6$training2)
# 
# df6 <- df6 %>% group_by(id_force) %>% mutate(duration_training = ifelse(type_event == 1, lead(event, order_by = id_force) - event, NA))
# df6 <- df6 %>% mutate(duration_training = ifelse(is.na(duration_training), 0, duration_training))
# df6 <- df6 %>% group_by(id_force) %>% mutate(duration_training = max(duration_training))
# df6 <- df6 %>% filter(type_event %in% c(0,3))

# df7 <- df6 %>% group_by(id_force) %>% filter(!(type_event == 0 & row_number() == 2))

# df7 <- df7 %>% group_by(id_force) %>% mutate(duration_unemployment = ifelse(type_event == 0, lead(event, order_by = id_force) - event, NA))

# df7 <- df7 %>% mutate(duration_training = ifelse(is.na(duration_training), 0, duration_training))
# df7 <- df7 %>% group_by(id_force) %>% mutate(duration_training = max(duration_training))

# df7 <- df7 %>% mutate(transtion_unemployment = structure(ifelse(type_event == 0, event, NA), class = 'Date'),
#                       transition_employment = structure(ifelse(type_event == 3, event, NA), class = 'Date'))

# df7 <- df7 %>% group_by(id_force) %>%
#   mutate(transition_employment = structure(ifelse(is.na(transition_employment), lead(transition_employment), transition_employment), class = 'Date'))
# 
# df7 <- df7 %>% mutate(censored = is.na(transition_employment)) 
# dfwide <- df7 %>% group_by(id_force) %>% filter(row_number() == 1)
# 
# dfwide2 <- dfwide %>% select(id_force, training2, duration_training, duration_unemployment, transtion_unemployment, transition_employment, censored)
# dfwide2 <- dfwide2 %>% mutate(duration_un_net = duration_unemployment - duration_training)
# dfwide2 <- dfwide2 %>% filter(duration_un_net >= 0)
# 
# table(dfwide$training2)
# dfwide2 <- dfwide2 %>% mutate(censored2 = 1 - censored)

########################################################################################
# We want to get a dataframe with only one obs per individual with all the information #

data2 <- data %>% arrange(id_force, event) %>% group_by(id_force) %>%
  mutate(cumsum = cumsum(type_event == 3)) %>% group_by(id_force) %>%
  mutate(cumsum2 = cumsum(cumsum)) %>%
  filter(cumsum2 < 2) %>% # drop all but first transition to employment (just as a check should have been done in stata code)
  mutate(training2 = (type_event == 1)) %>% group_by(id_force) %>%
  mutate(training2 = max(training2)) # our true training variable can change (if training were after first transition to employment)
# Compute duration_training (in days)
data2 <- data2 %>% group_by(id_force) %>% mutate(duration_training = ifelse(type_event == 1, lead(event, order_by = id_force) - event, NA))
data2 <- data2 %>% mutate(duration_training = ifelse(is.na(duration_training), 0, duration_training)) #set duration to 0 if no training
data2 <- data2 %>% group_by(id_force) %>% mutate(duration_training = max(duration_training)) #only consider longest training
# training_var <- c('ANNEE_ENTREE', 'DISPOSITIF_TR', 'DISPOSITIF', 'CPF_autonome', 'OBJECTIF_STAGE',
#                   'DOMAINE_FORMATION', 'FORMACODE', 'duration_hours', 'TYPE_REMUNERATION',
#                   'NIV_DIPLOME', 'NIV_DIPLOME_TR', 'PUBLIC_PIC', 'TRAVAILLEUR_HANDICAPE', 'QPV',
#                   'MOIS_NAISSANCE', 'ANNEE_NAISSANCE', 'AGE_ENTREE_STAGE', 'CODE_POSTAL', 'COMMUNE',
#                   'DEPARTEMENT_HABITATION', 'REGION_HABITATION', 'COMMANDITAIRE', 'CPF_PE', 'nb_form')
# training_var <- c('OBJECTIF_STAGE', 'DOMAINE_FORMATION', 'duration_hours', 'TYPE_REMUNERATION',
#                   'NIV_DIPLOME', 'NIV_DIPLOME_TR', 'PUBLIC_PIC', 'nb_form')
var_tokeep <- c('id_force', 'training', 'SEXE', 'NENF', 'NATION', 'NIVFOR', 'DIPLOME',
                'SITMAT', 'QUALIF', 'ROME', 'AGE', 'PcsEse', 'Nature', 'end_contract',
                'event', 'type_event', 'nRecordedEvents', 'nTraining', 'nShortContract',
                'prev_unemployed','post_unemployed', 'ut', 'tu', 'duration_training', 'OBJECTIF_STAGE',
                'DOMAINE_FORMATION', 'duration_hours', 'TYPE_REMUNERATION',
                'NIV_DIPLOME', 'NIV_DIPLOME_TR', 'PUBLIC_PIC', 'nb_form')
data3 <- data2 %>% select(all_of(var_tokeep))
data3 <- data3 %>% mutate_all(na_if, "")
data3 <- data3 %>% dplyr::group_by(id_force) %>% tidyr::fill(all_of(var_tokeep), .direction = c('downup')) # fill missing data with value of other obs from the same individual
data3 <- data3 %>% mutate(training2 = (type_event == 1)) %>% group_by(id_force) %>% mutate(training2 = max(training2))
data4 <- data3 %>% filter(type_event %in% c(0,3))
data4 <- data4 %>% group_by(id_force) %>% filter(!(type_event == 0 & row_number() == 2))
data4 <- data4 %>% group_by(id_force) %>% mutate(duration_unemployment = ifelse(type_event == 0, lead(event, order_by = id_force) - event, NA))
data4 <- data4 %>% mutate(employment_duration = ifelse(type_event == 3 & !is.na(end_contract), end_contract - event, NA))

data4 <- data4 %>% mutate(duration_training = ifelse(is.na(duration_training), 0, duration_training))
#data4 <- data4 %>% group_by(id_force) %>% mutate(duration_training = max(duration_training))
data4 <- data4 %>% mutate(transition_unemployment = structure(ifelse(type_event == 0, event, NA), class = 'Date'))
data4 <- data4 %>% mutate(transition_employment = ifelse(type_event == 3, event, NA))
#data4 <- data4 %>% mutate(transition_employment = ymd(transition_employment))

data4 <- data4 %>% group_by(id_force) %>% mutate(transition_employment = structure(ifelse(is.na(transition_employment), lead(transition_employment), transition_employment), class = 'Date'))
data4 <- data4 %>% group_by(id_force) %>%
  mutate(transition_employment = structure(ifelse(is.na(transition_employment), lead(transition_employment), transition_employment), class = 'Date'))
data4 <- data4 %>% group_by(id_force) %>%
  mutate(employment_duration = ifelse(is.na(employment_duration), lead(employment_duration), employment_duration))

data4 <- data4 %>% mutate(censored = !is.na(transition_employment)) 
data4 <- data4 %>% mutate(censored_emp = !is.na(employment_duration)) 

dwide <- data4 %>% group_by(id_force) %>% filter(row_number() == 1)
dwide <- dwide %>% mutate(duration_un_net = duration_unemployment - duration_training)
dwide <- dwide %>% filter(duration_un_net >= 0)
dwide <- dwide %>% filter(employment_duration >= 0)
#dwide <- dwide %>% mutate(training2 = !(is.na(OBJECTIF_STAGE) | is.na(DOMAINE_FORMATION)))
dwide <- dwide %>% mutate(women = (SEXE == 2)) #homme = 1, femme = 2

write_dta(dwide, path = "./modified/data_to_use.dta")

# Histogram
ggplot(data = subset(dwide, !is.na(duration_unemployment)), aes(x = duration_unemployment)) +
  geom_histogram(binwidth = 31) 

ggplot(data = subset(dwide, !is.na(duration_un_net)), aes(x = duration_un_net)) +
  geom_histogram(binwidth = 31) 

ggplot(data = subset(dwide, !is.na(duration_unemployment)), aes(x = duration_unemployment, fill = factor(training2))) +
  geom_histogram(aes(y=31*..density..), binwidth = 31) +
  facet_wrap(~training2, nrow = 2)
ggplot(data = subset(dwide, !is.na(duration_un_net)), aes(x = duration_un_net, fill = factor(training2))) +
  geom_histogram(aes(y=31*..density..), binwidth = 31) +
  facet_wrap(~training2, nrow = 2)

 
# CDF
dwide$training2b <- dwide$training2 + 2

ggplot(dwide, aes(x = duration_unemployment, colour = factor(training2))) + stat_ecdf(geom = 'step')+
  stat_ecdf(aes(x = duration_un_net, colour = factor(training2b)),geom = 'step')+
  scale_color_manual(values = c("green", "blue","green","black"))


# CDF
dwide$training2b <- dwide$training2 + 2
dwide = dwide %>% mutate(training2bis = ifelse(training2 == 1, 2,training2))



ggplot(dwide, aes(x = duration_unemployment, color = factor(training2), colour = c("0","1"))) + stat_ecdf(geom = 'step')+
  stat_ecdf(aes(x = duration_un_net, color = factor(training2bis), colour = c("2")),geom = 'step')+
  scale_color_manual("Group", values = c("green", "blue","black"), breaks = c("0", "1", "2"),
                     labels = c('Non trainees', 'Trainees (training duration included)', 'Trainees (training duration excluded)')) +
  xlab("Duration of unemployment (days)") +
  ylab("") +
  theme_classic()


ggplot(filter(dwide, training2 == 1), aes(x = duration_unemployment, group = OBJECTIF_STAGE, colour = OBJECTIF_STAGE)) + stat_ecdf(geom = 'step') +
  xlab("Duration of unemployment (days)") +
  ylab("") +
  theme_classic()







ggplot(dwide, aes(x = duration_un_net, colour = factor(training2))) + stat_ecdf(geom = 'step')

plot(ecdf(filter(dwide, training2 == 1)$duration_unemployment), verticals=T, do.points=F, col = 'red')
plot(ecdf(filter(dwide, training2 == 0)$duration_unemployment), verticals=T, do.points=F, add=T, col = 'blue')
plot(ecdf(dwide$duration_un_net), verticals=T, do.points=F, add=T, col = 'green')

ggplot(data = dwide, aes(x = duration_unemployment,colour = factor(training2)))+
  geom_freqpoly()

library(ggplotify)
as.ggplot(plot(ecdf(dwide$duration_un_net), verticals=T, do.points=F, add=T, col = 'green'))

ggplot(dwide, aes(x = employment_duration, colour = factor(training2))) + stat_ecdf(geom = 'step')

survfit2(Surv(as.duration(duration_un_net), censored) ~ training2, data = dwide) %>%
  ggsurvfit() + add_confidence_interval()

survfit2(Surv(as.duration(employment_duration), censored) ~ training2, data = dwide) %>%
  ggsurvfit() + add_confidence_interval() + xlim(c(0,500))

plot(dwide$duration_unemployment, HazardRate(dwide$duration_unemployment, "weibull", .6, 1))

hr <- HazardRateEst(dwide$transition_unemployment, dwide$transition_employment, Epanechnikov, dwide$censored)

model <- HazardRateEst(transition_unemployment, transition_employment)
cox_nocontrols <- coxph(Surv(as.duration(duration_unemployment), censored) ~ training2, data = dwide)
cox_nocontrols %>% tbl_regression(exp = T)
cox_controls <- coxph(Surv(as.duration(duration_unemployment), censored) ~ training2 + women + NENF + AGE + SITMAT + DIPLOME + prev_unemployed, data = dwide)
cox_controls %>% tbl_regression(exp = T)
summary(cox_controls)

# cz <- cox.zph(cox_nocontrols)
# print(cz)
# plot(cz)
# coxph(Surv(as.duration(duration_un_net), censored) ~ training2, data = dwide) %>%
#   tbl_regression(exp = T)

cuts  <- seq(90, 2000, by = 90)
fit_noc <- pchreg(Surv(duration_unemployment, censored) ~ training2, cuts = cuts, data = dwide)
summary(fit_noc)
fit_c <- pchreg(Surv(duration_unemployment, censored) ~ training2 + women + AGE + NENF + SITMAT + DIPLOME + prev_unemployed, cuts = cuts, data = dwide)
summary(fit_c)


fit.cr <- eha::coxreg(Surv(duration_un_net, censored) ~ training2, data = dfwide2)
compHaz(fit.cr, fit)
