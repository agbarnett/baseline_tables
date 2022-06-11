# 0_read_hiruntrakul.R
# read in the baseline table data for the problematic trials by Hiruntrakul
# June 2022
library(janitor)
library(dplyr)
library(readxl)

# separate sheet for each trial
n_trials = 1
trials = paste('h', 1:n_trials, sep='')
all = NULL
for (t in trials){
	raw = read_excel('data/papers_hiruntrakul.xlsx', sheet=t) %>%
		clean_names() %>%
	  mutate(trial = t)
	all = bind_rows(all, raw)
}

# add sample size
sample_sizes = read_excel('data/papers_hiruntrakul.xlsx', sheet='sample_sizes') %>%
  clean_names()
all = left_join(all, sample_sizes, by=c('pmcid','column'))

# check for data entry errors
group_by(all, pmcid, row, column) %>%
  tally() %>%
  filter(n > 1) # should not be any
table(all$statistic)
filter(all, !statistic %in% c('number','continuous'))
summary(select(all, stat1, stat2, sample_size, row, column)) # check for missing data
filter(all, trial != pmcid)

# add variables
table_data = mutate(all,
          stat3 = NA, # needed to match other data
          stat4 = NA,
          dp1 = NA, # not collected 
          dp2 = NA,
          statistic = ifelse(statistic=='number', 'numbers', statistic)) %>% # got name wrong in data entry
  select(-trial, -comment, -starts_with('x')) # no longer needed

# save
date_downloaded = Sys.Date()
save(date_downloaded, table_data, file='data/hiruntrakul.RData')
