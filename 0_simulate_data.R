# 0_simulate_data.R
# simulate baseline table data under the null hypothesis (of no fraud) and alternative
# assume two groups per paper
# May 2021
library(dplyr)

# key parameters for simulated data
prop_continuous = 0.33 # what proportion of statistics are continuous (remainder are percent)
lambda_table_rows = 18 # mean number of results (rows) per table (based on real data)
min_sample_size = 4 # minimum sample size
mean_sample_size_log = 3.95 - log(min_sample_size) # mean sample size using log-normal (based on real data)
sd_sample_size_log = 1.23 # SD for sample size using log-normal (based on real data)
dp = 1 # decimal places for rounding

# number of papers
N_no_issue = 50 # number with no issue
N_mean = 2 # number with mean problem
N_variance = 2 # number with variance problem

# first simulate papers with no issue
sim_data = NULL
for (loop in 1:N_no_issue){
  sim_data1 = simulate_table1() %>% 
    mutate(pmcid = loop)
  sim_data = bind_rows(sim_data, sim_data1)
}

# now add papers with a mean difference
for (loop in 1:N_mean){
  sim_data2 = simulate_table1(issue='mean') %>%
    mutate(pmcid = loop + N_no_issue) # increase ID number
  sim_data = bind_rows(sim_data, sim_data2)
}

# now add papers that are too similar - to do
for (loop in 1:N_variance){
  sim_data2 = simulate_table1(issue='var') %>%
    mutate(pmcid = loop + N_no_issue + N_mean) # increase ID number
  sim_data = bind_rows(sim_data, sim_data2)
}


# save
sim_data = arrange(sim_data, pmcid, row, column)
row.names(sim_data) = NULL
save(sim_data, file='data/simulated_data.RData')
