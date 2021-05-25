# 4_model.R
# model the results
# May 2021
library(TeachingDemos)
seed = char2seed('cobblers')
library(R2WinBUGS)
source('4_make_winbugs.R')
source('4_MCMC_basics.R')
source('99_functions.R')
load('data/analysis_ready.RData') # from 2_check_extracted.R

# prepare the data for the Bayesian model
for_model = make_stats_for_bayes_model(indata = table_data) # see 99_functions.R

#
# use t-distribution with df instead?

# prepare the data for Winbugs
N = nrow(for_model) # number of statistics
N_studies = max(for_model$study) # number of studies
p_issue = 0.05 # prior probability that the study variance is too narrow or mean is non-zero
bdata = list(N = N, 
             N_studies = N_studies, 
             df = for_model$size - 2, # degrees of freedom
             study = for_model$study, 
             mdiff = for_model$mdiff, 
             sem2 = for_model$sem2, 
             eps = 0.001, # inverse-variance for slab
             p_issue = p_issue)
inits = list(I=rep(0, N_studies), I.var=rep(0, N_studies)) # initial values  
inits = rep(list(inits), n.chains) # repeat per chains

parms = c('I','I.var')
bugs = bugs(data=bdata, inits=inits, parameters=parms, model.file=bfile, DIC=FALSE,
                n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=seed, debug=debug,
                bugs.directory="c:/Program Files/WinBUGS14")
