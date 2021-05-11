# 2_check_extracted.R
# checks of extracted data with some edits
# May 2021
library(dplyr)

# get the data
load('data/extracted.RData') # 1_extract_tables_pmc.R

# are numbers integers?
not_integers = filter(table_data, statistic == 'numbers') %>%
  filter(round(stat1) != stat1 | round(stat2)==stat2)
