# 0_read_trialstreamer.R
# read in the data from trialstreamer
# August 2021
library(janitor)
library(dplyr)
library(readxl)

# get number randomised from trialstreamer
trialstreamer = read.csv('data/trialstreamer-update-pubmed-2021-06-21.csv', stringsAsFactors = FALSE) %>%
  clean_names() %>%
  select(pmid, doi, num_randomized)

# save
save(trialstreamer, file='data/trialstreamer.RData')
