# AppliedLabor
Applied Labor: Effect of Training on Unemployment Duration using Time-of-Event model

 All the codes related to the 
 ### (1) data processing and cleaning,
 
 Stata scripts whose names start with "explo" correspond to a first cleaning of each dataset (BREST, MMO and FH).
 
 
 resampling.do select a random sample of ID that will be use for the merge.do file that merges all the - reduced - datasets.
 
 
 ### (2) descriptive statistics production and first duration analyses,
 
 DescStat.do produces descriptive statistics used in Table 1 and Figure 3, as well as the Cox estimation presented in Table 2.
 
 
 statdesc.R produces Kaplan-Meier estimates presented in Figure 1 as well as CDF of Figure 2.

 ### (3) the implementation of the Time-of-Event model using R.
 
We implement our likelihood and maximization in model.R


model_parallelTest.R implement the same likelihood but with parallel compilation.
