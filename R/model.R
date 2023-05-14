
library(haven)
library(dplyr)
library(mosaicCalc)
library(maxLik)
library(tidyr)
library(stats4)

#library(errorist)
setwd('C:/Users/Public/SowVallette')
getwd()

data = read_stata(file = "./modified/data_to_use_label.dta")

ls(data)
var_tokeep <- c('id_force', 'training2', 'age_recod', 'censored', 'censored_emp', 'duration_hours_rec',
                'duration_training', 'duration_training_rec', 'duration_un_net', 'duration_unemployment',
                'employment_duration', 'event', 'foreign', 'NENF', 'nivdipl_rec', 'prev_unemployed',
                'single', "transition_employment", "transition_unemployment", "tu", "un_pre_training",
                "ut", "women")

df <- data %>% select(var_tokeep)

df <- df %>% filter(un_pre_training >= 0)
dfcopy <- df
df <- df[sample(nrow(df), 1000),]

df$foreign <- as.numeric(df$foreign)
df$age_recod <- as.numeric(df$age_recod)
df$women <- as.numeric(df$women)
df$single <- as.numeric(df$single)
df <- df %>% mutate(kid = ifelse(NENF > 0,1,0))
df$prev_unemployed <- as.numeric(df$prev_unemployed)
df$nivdipl_rec <- as.numeric(df$nivdipl_rec)
df <- df %>% mutate(duration_tu = un_pre_training + duration_training)

contr_indiv_ue = c()
contr_indiv_ut = c()
contr_indiv_tu = c()
msl <- function(param){
  
  ####1. LIKELIHOOD OF THE TRANSITIONS.####
  
  #####1.1 Define Phi
  Phi = function(t, a1 ,a2 ,a3 ,A =c(0,365,730, Inf)){##Use exponential as a constraint to have a positive baseline hazard
    a1*between(t, A[1], A[2])+
    a2*between(t, A[2]+1, A[3])+
    a3*between(t, A[3]+1, A[4])

  }
  #####1.2 Define the etas: eta_ue; eta_tu; eta_ut
  eta_ue = function(t=t_ue, a1_ue = param[1],a2_ue = param[2],a3_ue = param[3],A = c(0,365,730, Inf), beta_foreign_ue = param[4], alpha_ue = param[5], beta_ue = param[6], beta_foreign_training_ue = param[7], alpha_v_ue = param[8],
                 beta_women_ue=param[9], beta_kid_ue = param[10], beta_single_ue = param[11], beta_age_recod_ue = param[12], beta_nivdipl_rec_ue = param[13], beta_prev_unemployed_ue = param[14],
                 beta_women_training_ue=param[15], beta_kid_training_ue=param[16], beta_single_training_ue=param[17], beta_age_recod_training_ue=param[18],
                 beta_niv_dipl_training_ue=param[19], beta_prev_un_training_ue=param[20]){

    as.numeric(Phi(t=t_ue,a1 = param[1],a2 = param[2],a3 = param[3],A =c(0,365,730, Inf))*exp(beta_foreign_ue*df[indiv,'foreign'] + beta_women_ue*df[indiv,'women'] + beta_kid_ue*df[indiv,'kid'] +beta_single_ue*df[indiv,'single'] +
                                                                                               beta_age_recod_ue*df[indiv,'age_recod'] + beta_nivdipl_rec_ue*df[indiv,'nivdipl_rec']+ beta_prev_unemployed_ue*df[indiv,'prev_unemployed'] +
                                                                                               rnorm(mean = alpha_v_ue,1))*( t<=df[indiv,'un_pre_training'])+
      Phi(t=t_ue, a1 = param[1],a2 = param[2],a3 = param[3], A =c(0,365,730, Inf))*exp(beta_foreign_ue*df[indiv,'foreign'] + rnorm(mean = alpha_v_ue,1) + alpha_ue*df[indiv,'duration_training'] +
                                                                                     beta_ue*(t-df[indiv,'duration_training']-df[indiv,'un_pre_training']) +
                                                                                     beta_foreign_training_ue*df[indiv,'foreign']*df[indiv,'training2'] +
                                                                                       beta_women_training_ue*df[indiv,'women']*df[indiv,'training2']+
                                                                                       beta_kid_training_ue*df[indiv,'kid']*df[indiv,'training2']+
                                                                                       beta_single_training_ue*df[indiv,'single']*df[indiv,'training2']+
                                                                                       beta_age_recod_training_ue*df[indiv,'age_recod']*df[indiv,'training2']+
                                                                                       beta_niv_dipl_training_ue*df[indiv,'nivdipl_rec']*df[indiv,'training2']+
                                                                                       beta_prev_un_training_ue*df[indiv,'prev_unemployed']*df[indiv,'training2'])*
      ( t>df[indiv,'duration_training']+df[indiv, 'un_pre_training'])
      )
  }
  
  eta_tu = function(t=t_tu, a1 = param[21],a2 = param[22],a3 = param[23],A =c(0,365,730, Inf), 
                    beta_foreign_tu = param[24], alpha_v_tu = param[25], beta_women_tu = param[26],   beta_kid_tu = param[27],
                    beta_single_tu = param[28],
                    beta_age_recod_tu = param[29],
                    beta_nivdipl_rec_tu = param[30],
                    beta_prev_unemployed_tu = param[31]){
    
    if_else(df[indiv,'training2']==1, as.numeric(Phi(t=t_tu,a1 = param[21],a2 = param[22],a3 = param[23],A =c(0,365,730, Inf))*exp(beta_foreign_tu*df[indiv,'foreign'] +
                                                                                                           beta_women_tu*df[indiv,'women'] + 
                                                                                                           beta_kid_tu*df[indiv,'kid'] +
                                                                                                           beta_single_tu*df[indiv,'single'] +
                                                                                                           beta_age_recod_tu*df[indiv,'age_recod'] +
                                                                                                           beta_nivdipl_rec_tu*df[indiv,'nivdipl_rec']+
                                                                                                           beta_prev_unemployed_tu*df[indiv,'prev_unemployed'] +
                                                                                                           rnorm(mean = alpha_v_tu,1))), as.numeric(1/t_tu)
    )
    
  }
  
  eta_ut = function(t=t_ut, a1 = param[32],a2 = param[33],a3 = param[34],A =c(0,365,730, Inf), 
                    beta_foreign_ut = param[35], alpha_v_ut = param[36], beta_women_ut = param[37],   beta_kid_ut = param[38],
                    beta_single_ut = param[39],
                    beta_age_recod_ut = param[40],
                    beta_nivdipl_rec_ut = param[41],
                    beta_prev_unemployed_ut = param[42]){
    
    as.numeric(Phi(t=t_ut,a1 = param[32],a2 = param[33],a3 = param[34],A =c(0,365,730, Inf))*exp(beta_foreign_ut*df[indiv,'foreign'] +
                                                                                                 beta_women_ut*df[indiv,'women'] + 
                                                                                                 beta_kid_ut*df[indiv,'kid'] +
                                                                                                 beta_single_ut*df[indiv,'single'] +
                                                                                                 beta_age_recod_ut*df[indiv,'age_recod'] +
                                                                                                 beta_nivdipl_rec_ut*df[indiv,'nivdipl_rec']+
                                                                                                 beta_prev_unemployed_ut*df[indiv,'prev_unemployed'] +
                                                                                                 rnorm(mean = alpha_v_ut,1))
    )
  }
  #####1.3 Survival functions as function of the etas
  
  survi = function(t, eta){
    #print(paste0("survi - ", t))
    tt = seq(1,t ) 
    exp(-sum(sapply(tt, FUN = function (x) eta(t=x))))}
  
  ###UE transition: here censored corresponds to 1 if we observe the transition to employment and 0 otherwise
  l_eu_i = function(t=t_ue){
    as.numeric((eta_ue(t=t_ue)**df[indiv,'censored']))*survi(t=t_ue, eta_ue)
  }
  
  l_eu_int = function(t=t_ue){
    random_v = rnorm(10, mean = alpha_v_ue, 1)
    sum(l_eu_i(t=t_ue)*sapply(random_v, FUN = function(x) (1/sqrt(2*pi))*exp(-.5*(x-alpha_v_ue)**2)))

  }
  ###TU transition : here censored corresponds to 1 for all the individuals, since there is no one censored
  l_tu_i = function(t=t_tu){
    as.numeric((eta_tu(t=t_tu)**df[indiv,'training2']))*survi(t=t_tu, eta_tu)
  }
  
  l_tu_int = function(t=t_tu){
    random_v = rnorm(10, mean = alpha_v_tu, 1)
    sum( l_tu_i(t=t_tu)*sapply(random_v, FUN = function(x)(1/sqrt(2*pi))*exp(-.5*(x-alpha_v_tu)**2)))
    
  }
  ###UT transition: here censored corresponds to 1 if the individual followed a training, so the variable censored is equal to training
  l_ut_i = function(t=t_ut){
    as.numeric((eta_ut(t=t_ut)**df[indiv,'training2']))*survi(t=t_ut, eta_ut)
  }
  
  l_ut_int = function(t=t_ut){
    random_v = rnorm(10, mean = alpha_v_ut, 1)
    sum(l_ut_i(t=t_ut)*sapply(random_v, FUN = function(x) (1/sqrt(2*pi))*exp(-.5*(x-alpha_v_ut)**2)))
    
  }
  
  #####1.4 Individual contributions to the likelihood
  ##Set the parameters
  
  ##Related to UE
  a1_ue  <- param[1]
  a2_ue  <- param[2]
  a3_ue  <- param[3]
  beta_foreign <- param[4]
  alpha_ue <- param[5]
  beta_ue <- param[6]
  beta_foreign_training_ue <- param[7]
  alpha_v_ue <- param[8]
  beta_women_ue=param[9]
  beta_kid_ue = param[10]
  beta_single_ue = param[11]
  beta_age_recod_ue = param[12]
  beta_nivdipl_rec_ue = param[13]
  beta_prev_unemployed_ue = param[14]
  beta_women_training_ue=param[15]
  beta_kid_training_ue=param[16]
  beta_single_training_ue=param[17]
  beta_age_recod_training_ue=param[18]
  beta_niv_dipl_training_ue=param[19]
  beta_prev_un_training_ue=param[20]
  
  ##Related to UT   
  a1_ut  <- param[21]
  a2_ut  <- param[22]
  a3_ut  <- param[23]
  beta_foreign_ut <- param[24]
  alpha_v_ut <- param[25]
  beta_women_ut=param[26]
  beta_kid_ut = param[27]
  beta_single_ut = param[28]
  beta_age_recod_ut = param[29]
  beta_nivdipl_rec_ut = param[30]
  beta_prev_unemployed_ut = param[31]
  
  ##Related to TU
  a1_tu  <- param[32]
  a2_tu  <- param[33]
  a3_tu  <- param[34]
  beta_foreign_tu <- param[35]
  alpha_v_tu <- param[36]
  beta_women_tu=param[37]
  beta_kid_tu = param[38]
  beta_single_tu = param[39]
  beta_age_recod_tu = param[40]
  beta_nivdipl_rec_tu = param[41]
  beta_prev_unemployed_tu = param[42]
  

  for(indiv in 11:12){

      #nrow(df)){
    print(indiv)

    t_ue = as.numeric(df$duration_unemployment[indiv]) 
    t_ut = as.numeric(df$un_pre_training[indiv]) 
    t_tu = as.numeric(df$duration_tu[indiv]) 

    
    ###The vectors of individual contribution to the likelihoods
    contr_indiv_ue = c(contr_indiv_ue,l_eu_int(t=t_ue))
    contr_indiv_ut = c(contr_indiv_ut,l_ut_int(t=t_ut))
    contr_indiv_tu = c(contr_indiv_tu,l_tu_int(t=t_tu))
    
    # print(paste0("l_eu_int ", l_eu_int(t=t_ue)))
    # print(paste0("l_eu_i ", l_eu_i(t=t_ue)))
    # print(paste0("eta ", eta_ue(t=t_ue)))
    # print(paste0("survi ", survi(t=t_ue, eta_ue)))
    # print(paste0("contr ", contr_indiv_ue))
  #   if (indiv==3){     
  #   i = i+1
  #   print(paste0("iteration ",i))}
   }
  ###The final likelihood
  #print(c(contr_indiv_ue,contr_indiv_ut, contr_indiv_tu))

  #return(prod(contr_indiv_ue,contr_indiv_ut, contr_indiv_tu))
  log(prod(contr_indiv_ue,contr_indiv_ut, contr_indiv_tu))
}

#msl(param=rep(0.0005,42))
  cont = maxControl(iterlim = 1, tol = 2000, printLevel = 3)
  msl_result <- maxLik(msl, start = c(##Related to UE
                                      a1_ue=0.0001,
                                      a2_ue=0.0001,
                                      a3_ue=0.0001,
                                      beta_foreign_ue=0.0001,
                                      alpha_ue=0.0001,
                                      beta_ue=0.0001,
                                      beta_foreign_training_ue=0.0001,
                                      alpha_v_ue=0.0001,
                                      beta_women_ue=0.0001, 
                                      beta_kid_ue =0.0001, 
                                      beta_single_ue =0.0001, 
                                      beta_age_recod_ue =0.0001, 
                                      beta_nivdipl_rec_ue =0.0001, 
                                      beta_prev_unemployed_ue =0.0001,
                                      beta_women_training_ue=0.0001,
                                      beta_kid_training_ue=0.0001, 
                                      beta_single_training_ue=0.0001, 
                                      beta_age_recod_training_ue=0.0001,
                                      beta_niv_dipl_training_ue=0.0001, 
                                      beta_prev_un_training_ue=0.0001,    
                                      
                                      ##Related to UT   
                                      a1_ut  =0.0001,
                                      a2_ut  =0.0001,
                                      a3_ut  =0.0001,
                                      beta_foreign_ut =0.0001,
                                      alpha_v_ut =0.0001,
                                      beta_women_ut=0.0001,
                                      beta_kid_ut =0.0001,
                                      beta_single_ut =0.0001,
                                      beta_age_recod_ut =0.0001,
                                      beta_nivdipl_rec_ut =0.0001,
                                      beta_prev_unemployed_ut =0.0001,
                                      
                                      ##Related to TU
                                      a1_tu  =0.0001,
                                      a2_tu  =0.0001,
                                      a3_tu  =0.0001,
                                      beta_foreign_tu =0.0001,
                                      alpha_v_tu =0.0001,
                                      beta_women_tu=0.0001,
                                      beta_kid_tu =0.0001,
                                      beta_single_tu =0.0001,
                                      beta_age_recod_tu=0.0001,
                                      beta_nivdipl_rec_tu=0.0001,
                                      beta_prev_unemployed_tu =0.0001),
                       #method = 'BHHH', 
                      control=maxControl(iterlim = 2,printLevel = 3), method = 'SANN', fixed = c(rep(T, 41), F))
  summary(msl_result)
  
  
