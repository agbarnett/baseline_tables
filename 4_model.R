# 4_model.R
# Fits a Bayesian model to the baseline table data extracted from RCTs published on pubmed central
# November 2021
library(dplyr)
library(tidyr)
library(TeachingDemos)
seed = char2seed('cobblers') # random number seed for computational reproducibility
library(R2WinBUGS)
source('4_make_winbugs.R')
source('4_MCMC_basics.R')
source('99_functions.R')

## get the data; select which source of data to use
sources = c('my_search', 'trialstreamer', 'validation', 'simulation', 'bland')
source = sources[5] # controls which data source is run
stage = 'model'
source('1_which_data_source.R') # uses `source` and `stage`

# add papers that are problematic/retracted 
add_retracted = FALSE
if(add_retracted == TRUE){
  load('data/retracted_data.RData') # from 0_read_retracted_data.R
  # combine two data sets
  table_data = bind_rows(table_data, retracted_data)
}

# prepare the data for the Bayesian model
for_model = make_stats_for_bayes_model(indata = table_data) # see 99_functions.R
# quick visual checks
with(for_model, plot(log2(size), t))
with(for_model, plot(as.numeric(as.factor(pmcid)), t))
with(for_model, plot(mdiff, log2(sem)))

# temporary
#for_model_small = filter(for_model, study <= 300)

# run the model
bugs = run_bugs(in_data = for_model, debug=TRUE)

# can remove some results from bugs to save room
bugs$sims.array = NULL
bugs$sims.list = NULL
bugs$sd = NULL
bugs$last.values = NULL

# save
## keep study numbers and sample size
study_numbers = group_by(for_model, pmcid, study) %>%
  summarise(size = median(size)) %>% # median sample size 
  ungroup()
save(for_model, study_numbers, bugs, file=outfile)
