# 2_random_select_hand_read_checks.R
# read in the randomly selected data entered by hand (By Amarzaya)
# May 2021
library(readxl)
library(dplyr)
library(stringr)

## section 1: table data
# file names where data are
files = c("entered_data_AJ","entered data from 11 to 100") # first 10, then 11 to 100, then ...
to_load = paste('checks/', files, ".xlsx", sep='') # 
tables = excluded = NULL
for (k in 1:length(to_load)){
  sheets = readxl::excel_sheets(to_load[k]) # each paper is in a separate sheet
  sheets = sheets[str_detect(sheets, pattern='^PMC[0-9][0-9]')]
  for (this_sheet in sheets){
    # has it been excluded
    if(nchar(this_sheet) > 10){
      frame = data.frame(pmcid = str_sub(this_sheet, 1, 10), reason = str_sub(this_sheet, 11, nchar(this_sheet)))
      excluded = bind_rows(excluded, frame)
      next # skip to next
    }
    #
    raw = read_excel(to_load[k], sheet=this_sheet, col_types='text') # had to read all in as text and change later
    tables = bind_rows(tables, raw)
  }
}
# now change column types
tables = mutate_at(tables, vars('row','column','stat1','stat2','stat3','stat4'), as.numeric) %>% # can ignore warnings, does create some odd behaviour with decimal places
  rename('comments' = '...9') %>% # comments from Amarzaya
  select('pmcid','row','column','stat1','stat2','stat3','stat4','statistic','comments') # drop variables not needed

## section 2: sample size data
## section 3: p-value data
files = c("entered_data_1st 10_and_SampleSize_and_Pvalues") # 1 to 100, then ...
to_load = paste('checks/', files, ".xlsx", sep='') # 
sample_size = NULL
for (k in 1:length(to_load)){
  raw = read_excel(to_load[k], sheet='sample size')
  sample_size = bind_rows(sample_size, raw)
}
names(sample_size) = c('pmcid','column','sample_size','comments') 
# split into exclusions and not
excl = filter(sample_size, !column %in% as.character(1:10)) %>% 
  select(pmcid, column) %>%
  rename('reason2' = 'column')
sample_size = filter(sample_size, column %in% as.character(1:10)) %>%
  mutate(column = as.numeric(column))
# add to previous exclusions
excluded = full_join(excluded, excl, by='pmcid')

## section 3: p-value data
files = c("entered_data_1st 10_and_SampleSize_and_Pvalues") # 1 to 100, then ...
to_load = paste('checks/', files, ".xlsx", sep='') # 
pvalues = NULL
for (k in 1:length(to_load)){
  raw = read_excel(to_load[k], sheet='P value')
  pvalues = bind_rows(pvalues, raw)
}
names(pvalues) = c('row','pmcid','reported') # nicer names
pvalues = select(pvalues, -row) %>% # do not need
  mutate(reported = tolower(reported)) %>%
  filter(reported %in% c('no','yes')) # just yes/no
# check, should all be 1
table(table(pvalues$pmcid))

# Save
save(excluded, pvalues, sample_size, tables, file='data/hand_entered_data.RData')
