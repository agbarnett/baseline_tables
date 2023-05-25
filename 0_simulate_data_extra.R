# 0_simulate_data_extra.R
# simulate baseline table data under the null hypothesis (of no fraud) and alternative
# additional simulation looking at:
# - highly correlated patient variables
# - 3 groups per paper
# September 2022
library(MASS) # for mvrnorm 
library(dplyr)
library(tidyr)
source('99_functions_simulate.R')
TeachingDemos::char2seed('rochdale')

# key parameters for simulated data
#prop_continuous = 1 # what proportion of statistics are continuous (remainder are percent); not needed, all are continuous for this scenario
gamma_table = c(2.2, 0.15) # gamma shape and rate for the number of table rows (based on real data)
gamma_sample_size = c(10.7, 2.84) # gamma shape and rate for the log-transformed sample size (based on real data)
min_sample_size = 4 # minimum sample size
dp = 2 # decimal places for rounding, use relatively large to avoid rounding issues
n_sim = 500 # number of simulations  

## loop and create individual simulations
# moderately correlated
table_data1 = NULL
for (loop in 1:n_sim){
  sim_data1 = simulate_table1_correlated(loop = loop,
                                         within_corr = 0.2, # within participant correlation in continuous variables
                              gamma_table = gamma_table,
                              gamma_sample_size = gamma_sample_size,
                              min_sample_size = min_sample_size, # minimum sample size
                              dp = dp, # decimal places for rounding
                              issue = 'Correlated 0.2') 
  table_data1 = bind_rows(table_data1, sim_data1)
}

# highly correlated
table_data2 = NULL
for (loop in 1:n_sim){
  sim_data2 = simulate_table1_correlated(loop = loop,
                                         within_corr = 0.6,
                                         gamma_table = gamma_table,
                                         gamma_sample_size = gamma_sample_size,
                                         min_sample_size = min_sample_size, # minimum sample size
                                         dp = dp, # decimal places for rounding
                                         issue = 'Correlated 0.6') 
  table_data2 = bind_rows(table_data2, sim_data2)
}

# typical table with three arms
table_data3 = NULL
for (loop in 1:n_sim){
  sim_data3 = simulate_table1_3arms(loop = loop,
                                    prop_continuous = 0.5, # from real data (close to half)
                                    gamma_table = gamma_table,
                                    gamma_sample_size = gamma_sample_size,
                                    min_sample_size = min_sample_size, # minimum sample size
                                    dp = dp) # decimal places for rounding
  table_data3 = bind_rows(table_data3, sim_data3)
}

# combine 
table_data = bind_rows(table_data1, table_data2, table_data3)

# save
table_data = arrange(table_data, pmcid, row, column) # same as real data
row.names(table_data) = NULL
save(dp, table_data, file='data/simulated_data_extra.RData')

## quick diagram to show difference
source('99_functions.R')
library(stringr)
library(ggplot2)
for_model = make_stats_for_bayes_model(indata = table_data) %>% # see 99_functions.R
  mutate(group = case_when( # group for plotting
    str_detect(pmcid, pattern='0\\.2') ~ '0.2',
    str_detect(pmcid, pattern='0\\.6') ~ '0.6'
  ))
gplot = ggplot(data=for_model, aes(x=t))+
  geom_histogram()+
  facet_wrap(~group)
gplot

