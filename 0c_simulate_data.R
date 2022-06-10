# 0c_simulate_data.R
# third simulation with larger sample sizes and under-dispersed data
# Feb 2022
library(MASS) # for mvrnorm 
library(dplyr)
source('99_functions_simulate.R')

# key parameters for simulated data
prop_copy = 0.5 # proportion of table rows that are copied for under-dispersed data
prop_continuous = 0.5 # what proportion of statistics are continuous (remainder are percent)
big_ten = 10^6 # large number to make small variance for gamma
gamma_table = c(2.2, 0.15) # gamma shape and rate for the number of table rows (based on real data)
min_sample_size = 4 # minimum sample size - does not matter given previous line
dp = 1 # decimal places for rounding

# number of papers
N_too_precise = N_no_issue = N_too_variable = 50 # number too precise

# look at three sample sizes for the number of rows
table_data = NULL
sample_sizes = c(60, 120, 240) 
for (size in sample_sizes){ # loop through sizes
  gamma_sample_size = c(size*big_ten, big_ten) # sample size = 60 (average for trials with a small variance)
  issue_label = paste('too precise, size = ', size, sep='')
  for (loop in 1:N_too_precise){
    loop_plus = paste(loop, '.', size, sep='') # making sure different sizes get different labels
  sim_data3 = simulate_table1_alternative2(loop = loop_plus,
                              prop_continuous = prop_continuous,
                              gamma_table = gamma_table,
                              min_sample_size = min_sample_size, # minimum sample size
                              gamma_sample_size = gamma_sample_size,
                              exp_sample_size = FALSE,
                              dp = dp, # decimal places for rounding
                              issue='too precise') %>%
    select(-issue) %>%
    mutate(size = size, # add size to data
           issue = issue_label) # update issue labe
  table_data = bind_rows(table_data, sim_data3)
  }
}

## add over-dispersed and null data otherwise summary function does not run
# data with no issue
gamma_sample_size = c(sample_sizes[1]*big_ten, big_ten) # sample size = 60 (average for trials with a small variance)
for (loop in 1:N_no_issue){
  sim_data1 = simulate_table1_alternative2(loop = loop,
                              prop_continuous = prop_continuous,
                              gamma_table = gamma_table,
                              gamma_sample_size = gamma_sample_size,
                              min_sample_size = min_sample_size, # minimum sample size
                              exp_sample_size = FALSE,
                              dp = dp, # decimal places for rounding
                              issue = 'none') %>% # issue with the data, `none`, `too variable` for groups are too different, `too precise` for groups are too similar
    mutate(size=99)
  table_data = bind_rows(table_data, sim_data1)
}
# over-dispersed
for (loop in 1:N_too_variable){
  sim_data2 = simulate_table1_alternative2(loop = loop,
                                           prop_copy = prop_copy,
                              prop_continuous = prop_continuous,
                              gamma_table = gamma_table,
                              gamma_sample_size = gamma_sample_size,
                              min_sample_size = min_sample_size, # minimum sample size
                              exp_sample_size = FALSE,
                              dp = dp, # decimal places for rounding
                              issue='too variable') %>%
    mutate(size=99)
  table_data = bind_rows(table_data, sim_data2)
}

# save
table_data = arrange(table_data, pmcid, row, column) # same as real data
row.names(table_data) = NULL
save(dp, table_data, file='data/simulated_data_three.RData')

# check
barplot(table(table_data$row))
barplot(table(table_data$sample_size))
table(table_data$issue)
#
summarise(filter(table_data, pmcid == 'tooprecise25.60'), max(row))
