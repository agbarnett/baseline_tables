# 0_read_carlisle.R
# read in the baseline table data created by John Carlisle, from DOI: 10.1111/anae.13938
# June 2022
library(janitor)
library(dplyr)
library(readxl)

# Excel sheet has multiple files per journal 
journals = excel_sheets('data/anae13938-sup-0002-appendixs2.xlsx')
all = NULL
for (j in journals){
	raw = read_excel('data/anae13938-sup-0002-appendixs2.xlsx', sheet=j) %>%
		clean_names() %>%
		mutate(trial = paste(j, trial, sep=''),
		       journal = j)
	if(any(names(raw) == 'decm')){
	  raw = rename(raw, 'dec_m' = 'decm') # name varies by sheet
	}
	all = bind_rows(all, raw)
}

# change column names to match my names
table_data = rename(all,
   'pmcid' = 'trial',
   'sample_size' = 'number_in_group',
   'row' = 'measure',
   'column' = 'group',
   'stat1' = 'mean',
   'stat2' = 'sd',
   'dp1' = 'dec_m',
   'dp2' = 'decsd') %>%
   mutate(stat3 = NA, # needed to match other data
          stat4 = NA,
          statistic = 'continuous') # all stats are continuous for this data

# exclude results with just one column
one_column = group_by(table_data, pmcid) %>%
  summarise(maxcol = max(column)) %>%
  ungroup() %>%
  filter(maxcol == 1) %>%
  pull(pmcid)
table_data = filter(table_data, !pmcid %in% one_column)
   
# save
date_downloaded = Sys.Date()
save(date_downloaded, table_data, file='data/carlisle.RData')
