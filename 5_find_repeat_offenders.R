# 5_find_repeat_offenders.R
# find authors with multiple flagged papers
# June 2022
library(dplyr)
library(stringr)

# get the data
source = 'trialstreamer'
stage = 'model'
source('1_which_data_source.R') # uses `source` and `stage`
load(outfile)

# could restrict to under-dispersed using mu.var

# find flagged studies
flagged = as.data.frame(bugs$summary) %>%
  tibble::rownames_to_column() %>%
  filter(str_detect(rowname, pattern='^var')) %>%
  mutate(study = 1:n()) %>%
  filter(mean > 0.95) %>%
  left_join(study_numbers, by='study')
  

# get author data from pubmed - to do
