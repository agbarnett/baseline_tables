# 3_compare_mydata_trialstreamer.R
# compare my sample size with trialstreamer
# August 2021
library(dplyr)

# get the two data sets
load('data/trialstreamer.RData') # from 0_read_trialstreamer.R
load('data/analysis_ready.RData') # from 2_process_extracted_data.R
sample_sizes = filter(table_data, row==1) %>%
  group_by(pmid) %>%
  summarise(total = sum(sample_size)) %>% # total across groups
  mutate(pmid = as.integer(pmid)) # for merge

# merge
compare = left_join(sample_sizes, trialstreamer, by='pmid')

