# 99_correlation.R
# investigate correlation in rows of the baseline table
# December 2021
library(dplyr)
library(tidyr)

# parameters
N_sim = 1000 # number of simulations

# simulate table data for just one variable: gender
sim_table_data = NULL
for (pmcid in 1:N_sim){
  gamma_sample_size = c(10.7, 2.84) # gamma shape and rate for the log-transformed sample size (based on real data)
  sample_size = rgamma(n = 1, shape = gamma_sample_size[1], rate=gamma_sample_size[2])
  sample_size = round(exp(sample_size))
  
  # simulate table data
  prob = runif(n=1) # uniform gender distribution
  # group 1
  male1 = sum(rbinom(sample_size, size=1, prob=prob))
  female1 = sample_size - male1
  # group 2
  male2 = sum(rbinom(sample_size, size=1, prob=prob)) # randomised, so same probability
  female2 = sample_size - male2
  # make frame (males on one row, females on the next row)
  row1m = data.frame(row=1, column=1, stat1=male1)
  row2m = data.frame(row=1, column=2, stat1=male2)
  row1f = data.frame(row=2, column=1, stat1=female1)
  row2f = data.frame(row=2, column=2, stat1=female2)
  stats = bind_rows(row1m, row1f, row2m, row2f) %>%
    mutate(pmcid = paste('sim', pmcid, sep=''), 
           dp1 = 0,
           dp2 = 0,
           statistic = 'percent', 
           sample_size = sample_size,
           stat2 = NA) # format of n (%)
  sim_table_data = bind_rows(sim_table_data, stats)
}

# get t-statistics
# prepare the data for the Bayesian model
for_model = make_stats_for_bayes_model(indata = sim_table_data) # see 99_functions.R
to_plot = select(for_model, pmcid, row, t) %>%
  pivot_wider(values_from = t, names_from = row)
# shows strong negative correlation
with(to_plot, plot(`1`, `2`))
