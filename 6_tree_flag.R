# 6_tree_flag.R
# use classification tree to see if study design can explain precision flag
# August 2021 
library(dplyr) 
library(rpart)
library(rpart.plot) 

## get the data; select which source of data to use
sources = c('my_search', 'trialstreamer', 'validation', 'simulation')
source = sources[2]
stage = 'tree'
source('1_which_data_source.R') # uses `source` and `stage`

## reduce countries and journals with small numbers
# a) countries
countries = group_by(design, affiliation) %>%
  tally() %>%
  ungroup() %>%
  mutate(country = ifelse(n < 20, 'Other', affiliation))
design = left_join(design, countries, by='affiliation')
# b) journals
journals = group_by(design, journal) %>%
  tally() %>%
  ungroup() %>%
  mutate(Journal = ifelse(n < 10, 'Other', journal)) # captial "J"
design = left_join(design, journals, by='journal')

# merge the design and flag data
threshold = 0.8 # probability beyond which we create a flag
flag_stats = filter(all_stats, nicevar=='Precision flag') %>%
  left_join(design, by='pmcid') %>%
  mutate(outcome = mean >= threshold)
# remove extreme means (likely issues with the algorithm)
to_exclude = filter(all_stats, nicevar=='Precision') %>%
  filter(mean > 40) %>%
  pull(pmcid)
flag_stats = filter(flag_stats, !pmcid %in% to_exclude)

# change control
my.control = rpart.control()
my.control$maxdepth = 4
my.control$xval = 20
TeachingDemos::char2seed('crawley')

# tree
model = rpart(outcome ~ country + Journal + rct + pilot + cluster + sem + block_randomisation + adaptive_randomisation, data = flag_stats, method = 'class', control = my.control)
summary(model)
rpart.plot(model, type=4, extra=103) # display n
rpart::plotcp(model)

# list of countries