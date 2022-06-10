# 0_simulate_data_bland.R
# simulate baseline table data; assume two groups per paper
# testing three of Martin Bland's difficult results for the non-uniform p-distribution
# added categorical correlation, e.g., ethnicity
# June 2022
library(MASS) # for mvrnorm 
library(dplyr)
library(tidyverse)
source('99_functions_simulate.R')
library(TeachingDemos)
char2seed('oldham') # use seed to generate same random data

# key parameters for simulated data
gamma_table = c(2.2, 0.15) # gamma shape and rate for the number of table rows (based on real data)
dp = 1 # decimal places for rounding

# number of papers
N_papers = 500 # number of papers per simulation type

## a) papers that are small binary papers under H0 of no fraud
table_data1 = NULL
for (loop in 1:N_papers){
  sim_data = simulate_table1(loop = loop,
                              prop_continuous = 0,
                              gamma_table = gamma_table,
                              gamma_sample_size = c(10000, 1000), # sample size = 10 (as per Bland)
                              exp_sample_size = FALSE,
                              min_sample_size = 10, # minimum sample size
                              dp = dp, # decimal places for rounding
                              issue = 'small binary') # 
  table_data1 = bind_rows(table_data1, sim_data)
}

## b) papers that are large binary papers under H0 of no fraud
table_data2 = NULL
for (loop in 1:N_papers){
  sim_data = simulate_table1(loop = loop,
                              prop_continuous = 0,
                              gamma_table = gamma_table,
                              gamma_sample_size = c(1000000, 1000), # sample size = 1000 (as per Bland)
                              exp_sample_size = FALSE,
                              min_sample_size = 10, # minimum sample size
                              dp = dp, # decimal places for rounding
                              issue = 'large binary') # 
  table_data2 = bind_rows(table_data2, sim_data)
}

## c) papers that are skewed under H0 of no fraud
table_data3 = NULL
for (loop in 1:N_papers){
  sim_data = simulate_table1(loop = loop,
                              prop_continuous = 1,
                              add_skew = TRUE, 
                              gamma_table = gamma_table,
                              gamma_sample_size = c(1000000, 1000), # sample size = 1000 (as per Bland)
                              exp_sample_size = FALSE,
                              min_sample_size = 10, # minimum sample size
                              dp = dp, # decimal places for rounding
                              issue = 'skewed') # 
  table_data3 = bind_rows(table_data3, sim_data)
}


## d) papers that try to mimic real tables with no issue (see 0_simulate_data.R)
gamma_sample_size = c(11.2, 3.0) # from Pubmed Central tables
table_data4 = NULL
for (loop in 1:N_papers){
  sim_data = simulate_table1_alternative2(loop = loop,
                                          prop_continuous = 0.5, # from real data (close to half)
                                          gamma_table = c(2.2, 0.15),
                                          gamma_sample_size = gamma_sample_size, # 
                                          exp_sample_size = TRUE,
                                          min_sample_size = 4, # minimum sample size
                                          dp = dp, # decimal places for rounding
                                          issue = 'none') #
  table_data4 = bind_rows(table_data4, sim_data)
}

## e) as previous, but with rounding
table_data5 = mutate(table_data4,
                     issue = 'rounded',
                     pmcid = str_replace(pmcid, 'none', 'rounded'),
                     stat1 = round(stat1, 0), # take 1 dp off mean (1 -> 0)
                     stat2 = round(stat2, 1)) # take 1 dp off SD (2 -> 1)

## f) papers that try to mimic real tables that are too precise (under-dispersed) (see 0_simulate_data.R)
table_data6 = NULL
for (loop in 1:N_papers){
  sim_data = simulate_table1_alternative2(loop = loop,
                                          prop_copy = 0.5,
                                         prop_continuous = 0.5, # from real data (close to half)
                                         gamma_table = c(2.2, 0.15),
                                         gamma_sample_size = gamma_sample_size, # 
                                         exp_sample_size = TRUE,
                                         min_sample_size = 4, # minimum sample size
                                         dp = dp, # decimal places for rounding
                                         issue = 'too precise') #
  table_data6 = bind_rows(table_data6, sim_data)
}

## g) papers that try to mimic real tables that are too variable (see 0_simulate_data.R)
table_data7 = NULL
for (loop in 1:N_papers){
  sim_data = simulate_table1_alternative(loop = loop,
                                         prop_continuous = 0.5, # from real data (close to half)
                                         gamma_table = c(2.2, 0.15),
                                         gamma_sample_size = gamma_sample_size, # 
                                         exp_sample_size = TRUE,
                                         min_sample_size = 4, # minimum sample size
                                         dp = dp, # decimal places for rounding
                                         issue = 'too variable') #
  table_data7 = bind_rows(table_data7, sim_data)
}

## h) papers with correlated categorical results, e.g., ethnicity
table_data8 = NULL
for (loop in 1:N_papers){
  sim_data = simulate_table1_categorical(loop = loop,
                                         gamma_table = c(2.2, 0.15),
                                         gamma_sample_size = gamma_sample_size, # 
                                         exp_sample_size = TRUE,
                                         min_sample_size = 4 ) # minimum sample size
  table_data8 = bind_rows(table_data8, sim_data)
}

## i) papers that try to mimic real tables with no issue with only continuous data (see 0_simulate_data.R)
# also with generous rounding
table_data9 = NULL
for (loop in 1:N_papers){
  sim_data = simulate_table1_alternative2(loop = loop,
                                          prop_continuous = 1, # 
                                          gamma_table = c(2.2, 0.15),
                                          gamma_sample_size = gamma_sample_size, # 
                                          exp_sample_size = TRUE,
                                          min_sample_size = 4, # minimum sample size
                                          dp = 3, # decimal places for rounding
                                          issue = 'none continuous') #
  table_data9 = bind_rows(table_data9, sim_data)
}

# combine eight data sets
table_data = bind_rows(table_data1, table_data2, table_data3, table_data4, table_data5, table_data6, table_data7, table_data8, table_data9)

# save
table_data = arrange(table_data, pmcid, row, column) # same as real data
row.names(table_data) = NULL
save(dp, table_data, file='data/simulated_data_bland.RData')

## quick diagram to show difference
# get the t-statistics
library(tidyverse)
source('99_functions.R')
for_model = make_stats_for_bayes_model(indata = table_data) %>%
  mutate(group = case_when( # group for plotting
str_detect(pmcid, pattern='small') ~ 'Small binary',
str_detect(pmcid, pattern='large') ~ 'Large binary',
str_detect(pmcid, pattern='skewed') ~ 'Skewed',
str_detect(pmcid, pattern='none') ~ 'Mimic - none',
str_detect(pmcid, pattern='rounded') ~ 'Mimic - none rounded',
str_detect(pmcid, pattern='precise') ~ 'Mimic - precise',
str_detect(pmcid, pattern='variable') ~ 'Mimic - variable',
str_detect(pmcid, pattern='categorical') ~ 'Mimic - categorical'
  ))

gplot = ggplot(data=for_model, aes(x=t))+
  geom_histogram()+
  facet_wrap(~group, scales='free')
gplot

# check numbers per scenario
dplyr::select(table_data, pmcid, issue) %>%
  unique() %>%
  group_by(issue) %>%
  tally()
