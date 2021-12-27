# 5_check_sample_size.R
# check the sample size difference for flagged studies
# Sep 2021
library(dplyr)
library(janitor)

## get the data; select which source of data to use
sources = c('my_search', 'trialstreamer', 'validation', 'simulation')
source = sources[2]
stage = 'plot'
source('1_which_data_source.R') # uses `source` and `stage`

# flagged studies
flag = filter(all_stats, nicevar=='Precision flag') %>%
  select(pmcid, mean)

# get sample size variance and merge with flag
sizes = select(table_data, pmcid, column, sample_size) %>%
  unique() %>%
  group_by(pmcid) %>%
  summarise(var = var(sample_size)) %>%
  left_join(flag, by='pmcid') %>%
  mutate(zero = var ==0, # no variance in sample size
         flagged = mean > 0.9)

tabyl(sizes, zero, flagged) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
  
