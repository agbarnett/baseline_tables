# 2_read_random_select_hand_checks.R
# read in the randomly selected data entered by hand (By Amarzaya)
# December 2021
library(readxl)
library(dplyr)
library(stringr)

## Section 0: get list of 200 randomly selected
# file names where data are
files = c("entered_data_AJ","entered data from 11 to 100","papers_to_check3") # first 10, then 11 to 100, then ...
to_load = paste('checks/', files, ".xlsx", sep='') # 
all_pmcids = NULL
for (k in 1:length(to_load)){
  raw = read_excel(to_load[k], sheet=1, col_types='text') %>% # had to read all in as text and change later
    select(pmc)
  all_pmcids = bind_rows(all_pmcids, raw)
}
all_pmcids = unique(all_pmcids) 
all_pmcids = all_pmcids$pmc
length(all_pmcids) # should be 200

## section 1: table data
tables = sheet_list = NULL
for (k in 1:length(to_load)){
  sheets = readxl::excel_sheets(to_load[k]) # each paper is in a separate sheet
  sheets = sheets[str_detect(sheets, pattern='^PMC[0-9][0-9]')] # just sheets that are table data
  sheets = sheets[nchar(sheets) == 10] # exclude 2 impossible IDs PMC16831439, very odd
  # keep data frame of sheets
  slist = data.frame(file=to_load[k], pmcid=sheets, index=1:length(sheets))
  sheet_list = bind_rows(sheet_list, slist)
  # read in the data
  for (this_sheet in sheets){
    #
    raw = read_excel(to_load[k], sheet=this_sheet, col_types='text') %>% # had to read all in as text and change later
      mutate(source = to_load[k]) # # record where it came from
    tables = bind_rows(tables, raw)
  }
}
# now change column types
tables = mutate_at(tables, vars('row','column','stat1','stat2','stat3','stat4'), as.numeric) %>% # can ignore warnings, does create some odd behaviour with decimal places
  rename('comments' = '...10') %>% # comments from Amarzaya
  filter(!is.na(row),
         !is.na(column)) %>%
  mutate(
    statistic = ifelse(statistic == 'number', 'numbers', statistic), # to match algorithm data
    pmcid = str_replace_all(tolower(pmcid), 'pmc', 'PMC')) %>% # PMC needs to be consistent in upper case
  select('source','pmcid','row','column','stat1','stat2','stat3','stat4','statistic','comments') # drop variables not needed

# tables to exclude by me
excluded = NULL
extra_exclude = read.table(header=TRUE, sep=',', stringsAsFactors = FALSE, text='
pmcid,reason
PMC7660174,Follow-up data in table') %>%
  mutate(pmcid = as.character(pmcid)) # for merge
tables = filter(tables, !pmcid %in% extra_exclude$pmcid)
# add to excluded
excluded = bind_rows(excluded, extra_exclude) %>%
  mutate(reason = str_squish(reason),
         reason = str_replace_all(reason, pattern='notable', replacement = 'no table'),
         reason = str_remove_all(reason, pattern='^-'))

## section 2: sample size data
files = c("entered_data_1st 10_and_SampleSize_and_Pvalues","P value & Sample size for the 2nd 100 studies") # 1 to 100, then ...
to_load = paste('checks/', files, ".xlsx", sep='') # 
sample_size = NULL
for (k in 1:length(to_load)){
  raw = read_excel(to_load[k], sheet='sample size') %>%
    mutate(sample_size = as.numeric(sample_size))
  sample_size = bind_rows(sample_size, raw)
}
# tidy up
sample_size = select(sample_size, 1:4)
names(sample_size) = c('pmcid','column','sample_size','comments') 
# split into exclusions and not
excl = filter(sample_size, !column %in% as.character(1:10)) %>% 
  select(pmcid, column) %>%
  rename('reason2' = 'column')
sample_size = filter(sample_size, 
                     nchar(pmcid) == 10, 
                     column %in% as.character(1:10)) %>%
  mutate(column = as.numeric(column)) %>%
  select(-comments)
# add to previous exclusions
excluded = full_join(excluded, excl, by='pmcid') %>%
  mutate(temporary = coalesce(reason, reason2)) %>%
  select(-starts_with('reason')) %>%
  rename('reason' = 'temporary')

## section 3: p-value data
files = c("entered_data_1st 10_and_SampleSize_and_Pvalues","P value & Sample size for the 2nd 100 studies") # 1 to 100, then ...
to_load = paste('checks/', files, ".xlsx", sep='') # 
pvalues = NULL
for (k in 1:length(to_load)){
  raw = read_excel(to_load[k], sheet='P value')
  pvalues = bind_rows(pvalues, raw)
}
names(pvalues) = c('row','pmcid','reported','comments1','comments2') # nicer names
pvalues = select(pvalues, -row, -starts_with('comments')) %>% # do not need
  mutate(reported = tolower(reported)) %>%
  filter(nchar(pmcid) == 10, 
         reported %in% c('no','yes')) # just yes/no
# check, should all be 1
table(table(pvalues$pmcid))

# do not get pmid data from API, only needed for trialstreamer
s1 = select(tables, pmcid) # use both tables and excluded
s2 = select(excluded, pmcid)
ids = bind_rows(s1, s2) %>% 
  unique() %>% 
  filter(str_sub(pmcid,1,3)=='PMC')

## add sample size to table data
tables = mutate(tables, 
                row = as.numeric(row),
                column = as.numeric(column)) %>%
  select(source, pmcid, row, column, starts_with('stat'), 'statistic')
tables = left_join(tables, sample_size, by=c('pmcid','column'))

# tidy up excluded reason
excluded = mutate(excluded,
  reason = case_when( 
    reason == 'embargo' ~ 'Full text page not available',
    reason == 'no table' ~ 'No baseline table',
    reason == 'no tables' ~ 'No baseline table',
    reason == 'No tables' ~ 'No baseline table',
    reason == 'compared organisation' ~ 'No baseline table',
    reason == 'intervention' ~ 'No baseline table',
    reason == 'control' ~ 'No baseline table',
    reason == 'animal study' ~ 'No baseline table',
    reason == 'no info' ~ 'No baseline table',
    reason == 'no info(animal study)' ~ 'No baseline table',
    reason == 'crossover study' ~ 'Just one column in table',
    reason == 'No comparison' ~ 'Just one column in table',
    reason == 'total sample' ~ 'Just one column in table',
    reason == 'Follow-up data in table' ~ 'Follow-up results in baseline table',
    reason == 'graphical table' ~ 'Graphical table',
    reason == 'study protocol' ~ 'Study protocol',
    reason == 'no control group' ~ 'Just one column in table', # Single-arm study
    TRUE ~ as.character(reason) # otherwise
  ))
excluded = unique(excluded) # remove 3 duplicates
# remove another duplicate
index = excluded$pmcid == 'PMC7246321' & excluded$reason == 'Just one column in table'
excluded = excluded[!index, ]
table(table(excluded$pmcid)) # check, should all be 1

# check that all PMCIDs are valid
tables = filter(tables, pmcid %in% all_pmcids)
excluded = filter(excluded, pmcid %in% all_pmcids)

# Save
save(excluded, pvalues, tables, ids, file='data/hand_entered_data.RData')

# check for trials that are not in data nor excluded
included = unique(c(tables$pmcid, excluded$pmcid))
all_pmcids[all_pmcids %in% included == FALSE]
