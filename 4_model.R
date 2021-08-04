# 4_model.R
# model the table data from pubmed central
# August 2021
library(dplyr)
library(tidyr)
library(TeachingDemos)
seed = char2seed('cobblers')
library(R2WinBUGS)
source('4_make_winbugs.R')
source('4_MCMC_basics.R')
source('99_functions.R')

# get the data: 1) data from automated extraction, 2) retracted data with known issues
load('data/analysis_ready.RData') # from 2_check_extracted.R
load('data/retracted_data.RData') # from 0_read_retracted_data.R
# combine two data sets
table_data = bind_rows(table_data, retracted_data)

# prepare the data for the Bayesian model
for_model = make_stats_for_bayes_model(indata = table_data) # see 99_functions.R
# quick visual check
with(for_model, plot(size, t))
with(for_model, plot(pmcid, t))

# prepare the data for Winbugs
N = nrow(for_model) # number of statistics
N_studies = max(for_model$study) # number of studies
p_issue = 0.05 # prior probability that the study mean is non-zero
p_invar = c(0.05,0.9,0.05) # prior probabilities that the study precision is: too wide, zero, too narrow
bdata = list(N = N, 
             c = log(1), # 1 or greater inside brackets (minimum multiplier)
             N_studies = N_studies, 
             df = for_model$size - 2, # degrees of freedom
             study = for_model$study, 
             mdiff = for_model$mdiff, 
             sem2 = 1 / for_model$sem2,  # inverse
             eps = 0.001, # inverse-variance for slab
             p_issue = p_issue,
             p_invvar = p_invar)
## initial values
mu.var = matrix(data=NA, ncol=3, nrow=N_studies) # start with NA
mu.var[,1] = -0.1 # small negative for negative side of slab
mu.var[,3] = 0.1 # small positive for positive side of slab
mu.beta = matrix(data=NA, ncol=2, nrow=N_studies)
mu.beta[,2] = rnorm(n=N_studies, mean=0, sd=0.1)
#
inits = list(mu.beta=mu.beta, mu.var=mu.var, var.flag=rep(2, N_studies), mean.flag=rep(0, N_studies)) # initial values  
inits = rep(list(inits), n.chains) # repeat per chains

parms = c('mean.flag','var.flag')
bugs = bugs(data=bdata, inits=inits, parameters=parms, model.file=bfile, DIC=FALSE,
                n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=seed, debug=debug,
                bugs.directory="c:/Program Files/WinBUGS14")

# can remove some parts from bugs to save room
bugs$sims.array = NULL
bugs$sims.list = NULL
bugs$sd = NULL
bugs$last.values = NULL

# save
study_numbers = select(for_model, pmcid, study) %>% 
  unique()
save(study_numbers, bugs, file='results/bugs_real.RData')
