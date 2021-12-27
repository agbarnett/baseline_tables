# 0_simulate_data.R
# simulate baseline table data under the null hypothesis (of no fraud) and alternative
# assume two groups per paper
# August 2021
library(MASS) # for mvrnorm 
library(dplyr)
source('99_functions_simulate.R')

# key parameters for simulated data
prop_continuous = 1 # what proportion of statistics are continuous (remainder are percent)
gamma_table = c(2.2, 0.15) # gamma shape and rate for the number of table rows (based on real data)
gamma_sample_size = c(10.7, 2.84) # gamma shape and rate for the log-transformed sample size (based on real data)
min_sample_size = 4 # minimum sample size
dp = 1 # decimal places for rounding
#diff = 0.5 # difference between groups (multiplier, 1 = no difference) - used rho instead
rho = 0.9 # correlation between results that are too similar (or negative for results that are too different)
  
# number of papers
N_no_issue = 200 # number with no issue
N_too_variable = 50 # number too variable
N_too_precise = 50 # number too precise

# first simulate papers with no issue
table_data = NULL
for (loop in 1:N_no_issue){
  sim_data1 = simulate_table1(loop = loop,
                              prop_continuous = prop_continuous,
                              gamma_table = gamma_table,
                              gamma_sample_size = gamma_sample_size,
                              min_sample_size = min_sample_size, # minimum sample size
                              dp = dp, # decimal places for rounding
                              issue = 'none') # issue with the data, `none`, `too variable` for groups are too different, `too precise` for groups are too similar
  table_data = bind_rows(table_data, sim_data1)
}

# now add papers that are too variable
for (loop in (N_no_issue+1):(N_no_issue+N_too_variable)){
  sim_data2 = simulate_table1(loop = loop,
                              prop_continuous = prop_continuous,
                              gamma_table = gamma_table,
                              gamma_sample_size = gamma_sample_size,
                              min_sample_size = min_sample_size, # minimum sample size
                              dp = dp, # decimal places for rounding
                              rho = rho,
                              issue='too variable')
  table_data = bind_rows(table_data, sim_data2)
}

# now add papers that are too similar
for (loop in (N_no_issue+N_too_variable+1):(N_no_issue+N_too_variable+N_too_precise)){
  sim_data3 = simulate_table1(loop = loop,
                              prop_continuous = prop_continuous,
                              gamma_table = gamma_table,
                              min_sample_size = min_sample_size, # minimum sample size
                              gamma_sample_size = gamma_sample_size,
                              dp = dp, # decimal places for rounding
                              rho = rho,
                              issue='too precise') 
  table_data = bind_rows(table_data, sim_data3)
}

# save
table_data = arrange(table_data, pmcid, row, column) # same as real data
row.names(table_data) = NULL
save(dp, rho, table_data, file='data/simulated_data.RData')

## quick diagram to show difference
# prepare the data for the Bayesian model
for_model = make_stats_for_bayes_model(indata = table_data) %>% # see 99_functions.R
  mutate(group = case_when( # group for plotting
    str_detect(pmcid, pattern='none') ~ 'None',
    str_detect(pmcid, pattern='precise') ~ 'Too variable',
    str_detect(pmcid, pattern='variable') ~ 'Too precise'
  ))
gplot = ggplot(data=for_model, aes(x=t))+
  geom_histogram()+
  facet_wrap(~group)
gplot
