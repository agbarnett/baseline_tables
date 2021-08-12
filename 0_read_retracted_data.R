# 0_read_retracted_data.R
# read in baseline table data from retracted papers or papers with known issues
# May 2021
library(dplyr)
library(readxl)

# download the data entered by hand
sheets = excel_sheets('data/retracted_papers_data.xlsx')
sheets = sheets[sheets != 'sample_sizes']
retracted_data = NULL
for (s in sheets){
  this_data = read_excel('data/retracted_papers_data.xlsx', sheet=s)
  retracted_data  = bind_rows(retracted_data, this_data)
}
retracted_data = select(retracted_data, -comment)

# add the sample sizes
ssize = read_excel('data/retracted_papers_data.xlsx', sheet='sample_sizes')
retracted_data = left_join(retracted_data, ssize, by=c('pmcid','column'))  %>%
  mutate(pmcid = as.character(pmcid)) # to match automated data

# save
save(retracted_data, file='data/retracted_data.RData')
