# 4_model_simulated.R
# model the simulated data
# May 2021
library(TeachingDemos)
seed = char2seed('cobblers')
library(R2WinBUGS)
source('4_make_winbugs.R')
source('4_MCMC_basics.R')
source('99_functions.R')
load('data/analysis_ready.RData') # from 0_simulate_data.R

#
for_model = make_stats_for_bayes_model(indata=sim_data) # see 99_functions.R
#
with(for_model, plot(size, t))

# prepare the data for Winbugs
N = nrow(for_model) # number of years
N_studies = max(for_model$study)
p_issue = 0.05 # prior probability that the study variance is too narrow
bdata = list(N = N, 
             N_studies = N_studies, 
             study = for_model$study, 
             mdiff = for_model$mdiff, 
             sem2 = for_model$sem2, 
             size = log2(for_model$size) - log2(60), # centre
             p_issue = p_issue)
inits = list(reduction=0, alpha=0, delta=rep(1, N_studies)) # initial values  
inits = rep(list(inits), n.chains) # repeat per chains

parms = c('reduction','delta','alpha')
bugs = bugs(data=bdata, inits=inits, parameters=parms, model.file=bfile, DIC=FALSE,
                n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=seed, debug=debug,
                bugs.directory="c:/Program Files/WinBUGS14")
