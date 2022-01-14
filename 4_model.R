# 4_model.R
# Fits a Bayesian model to the baseline table data extracted from RCTs published on pubmed central
# January 2022
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
source = sources[2] # controls which data source is run
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

## check for missing rows in model after calculation of t-statistics
table_counts = filter(table_data, statistic %in% c('continuous','percent','numbers')) %>%
  select(pmcid, row, column) %>%
  arrange(pmcid, row, column) %>%
  group_by(pmcid, row) %>%
  slice(n()) %>% # last result per row
  mutate(comb = ncol(combn(column, 2))) %>% # number of paired comparisons per row
  group_by(pmcid) %>%
  summarise(table = sum(comb) )
model_counts = group_by(for_model, pmcid) %>% summarise(model = length(unique(row)))
count_compare = full_join(table_counts, model_counts, by='pmcid') %>%
  filter(table != model)
# differences are often due to errors in table, e.g., PMC7763038

## exclude perfectly correlated neighbours, e.g. male/female gender
# does knock out some valid data, e.g, PMC7821012, but works very well on others, e.g, PMC6937882 with multiple examples
to_remove = filter(for_model, statistic %in% c('percent','numbers')) %>%
  group_by(pmcid) %>%
  arrange(pmcid, row) %>%
  mutate(r = row - round(row),
         diff = abs(lag(t) - t*(-1)) ) %>% # perfectly negative correlation in neighbouring rows
  ungroup() %>%
  filter(diff < 0.001 & t!=0 & r==0) %>% # small difference; r==0 means only applies to two groups
  select(pmcid, row) 
for_model = anti_join(for_model, to_remove, by=c('pmcid','row'))
n_removed = nrow(to_remove)

# quick visual checks of data
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
save(for_model, study_numbers, n_removed, bugs, file=outfile)
