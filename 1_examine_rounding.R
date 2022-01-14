# 1_examine_rounding.R
# examine the effect of rounding for continuous statistics
# using the thermal clothing data
## used simulation study instead ##
# January 2021
source('99_functions.R')
library(dplyr)
library(tidyverse)
library(R2WinBUGS)

# read in the thermal clothing study
# from url = 'https://figshare.com/articles/dataset/Thermal_clothing_clinical_trial/4649231/'
load('data/Trial.Data.RData')

## round continuous results and examine probability of dispersion
# get the continuous variables
cont.vars = c('age','EQ5D.baseline','ef','bpsyst1','bpdias1','crp_baseline','chol_baseline','fib_baseline','b.pittsburgh')
long = select(data, random, all_of(cont.vars)) %>%
  mutate(EQ5D.baseline = 100*EQ5D.baseline) %>% # to round on a reasonable scale
  pivot_longer(cols=all_of(cont.vars))
table_data = group_by(long, random, name) %>%
  filter(!is.na(value)) %>%
  summarise(stat1 = mean(value), # summary stats
            stat2 = sd(value),
            sample_size = n()) %>%
  group_by(random) %>%
  mutate(
    pmcid = 'PMC5640030',
    statistic = 'continuous',
    row = 1:n()) %>% # add table row
  ungroup() %>%
  rename('column' = 'random')

# round
table_data = mutate(table_data, 
                    stat1 = round(stat1),
                    stat2 = round(stat2))

for_model = make_stats_for_bayes_model(indata = table_data) # see 99_functions.R

# run the model
source('4_MCMC_basics.R')
source('4_make_winbugs.R')
seed = 1234
bugs = run_bugs(in_data = for_model, debug=TRUE)
bugs$summary[,c(1,3,7)]
