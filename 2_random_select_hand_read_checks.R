# 2_random_select_hand_read_checks.R
# read in the randomly selected data entered by hand (By Amarzaya)
# August 2021
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
  sheets = sheets[str_detect(sheets, pattern='^PMC[0-9][0-9]')] # just sheets that are table data
  for (this_sheet in sheets){
    # has it been excluded
    if(nchar(this_sheet) > 10){
      frame = data.frame(pmcid = str_sub(this_sheet, 1, 10), reason = str_sub(this_sheet, 11, nchar(this_sheet)))
      excluded = bind_rows(excluded, frame)
      next # skip to next
    }
    #
    raw = read_excel(to_load[k], sheet=this_sheet, col_types='text') %>% # had to read all in as text and change later
      mutate(source = to_load[k]) # # record where it came from
    tables = bind_rows(tables, raw)
  }
}
# now change column types
tables = mutate_at(tables, vars('row','column','stat1','stat2','stat3','stat4'), as.numeric) %>% # can ignore warnings, does create some odd behaviour with decimal places
  rename('comments' = '...10') %>% # comments from Amarzaya
  mutate(
    statistic = ifelse(statistic == 'number', 'numbers', statistic), # to match algorithm data
    pmcid = str_replace_all(tolower(pmcid), 'pmc', 'PMC')) %>% # PMC needs to be consistent in upper case
  select('source','pmcid','row','column','stat1','stat2','stat3','stat4','statistic','comments') # drop variables not needed

# tables to exclude by me
extra_exclude = read.table(header=TRUE, sep=',', stringsAsFactors = FALSE, text='
pmcid,reason
7660174,Follow-up data in table') %>%
  mutate(pmcid = as.character(pmcid)) # for merge
tables = filter(tables, !pmcid %in% extra_exclude$pmcid)

## section 2: sample size data
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
  mutate(column = as.numeric(column)) %>%
  select(-comments)
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

# get pmid data from API
### TO DO, change IDs to all 
library(XML) # for API
base_url = 'https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/'
ids = select(tables, pmcid) %>% 
  unique() %>% 
  filter(str_sub(pmcid,1,3)=='PMC') %>% 
  pull(pmcid)
request = paste(base_url, '?tool=R&email=a.barnett@qut.edu.au&ids=', paste(ids, collapse=','), sep='')
destfile = 'data/xml_validate.txt'
download.file(request, destfile)
xml = xmlParse(destfile) # parse the XML
list = xmlSApply(xml["//record"], xmlAttrs) # convert to a list
conversion = NULL
for (s in 1:dim(list)[2]){ # loop through the list
  this = list[,s]
  d = data.frame(t(this))
  conversion = bind_rows(conversion, d)
}

## add sample size to table data
tables = left_join(tables, sample_size, by=c('pmcid','column'))

# Save
save(excluded, pvalues, tables, conversion, file='data/hand_entered_data.RData')
