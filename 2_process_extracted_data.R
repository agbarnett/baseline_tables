# 2_process_extracted_data.R
# checks and processing of extracted data with some edits and exclusions
# May 2021
library(dplyr)

# get the data
load('data/extracted.RData') # 1_extract_tables_pmc.R
excluded_counts = NULL

# if statistic is 'continuous', but there are three stats, with stat4 empty, and stat1 is in stat2 to stat3 then change to 'median', e.g. PMC7362422
index = is.na(table_data$stat4) & !is.na(table_data$stat3) & table_data$statistic=='continuous' &
  (table_data$stat1>=table_data$stat2) & (table_data$stat1<=table_data$stat3)
cat('Changed ',sum(index),' continuous stats to median.\n', sep='')
table_data$statistic[index] = 'median'
  
# exclude numbers that are not integers
index = table_data$statistic == 'numbers' & 
    (round(table_data$stat1) != table_data$stat1 | round(table_data$stat2) != table_data$stat2)
cat('Excluded ',sum(index),' numbers that are not integers.\n', sep='')
if(sum(index)>0){
  table_data = table_data[!index,]
  eframe = data.frame(reason = 'Numbers that are not integers', count=sum(index))
  excluded_counts = bind_rows(excluded_counts, eframe)
}

# exclude if there is no second statistic (get counts excluded)
index = is.na(table_data$stat2)
cat('Excluded ',sum(index),' rows with no second statistic.\n', sep='')
if(sum(index) > 0){
  table_data = table_data[!index,]
}
# record excluded
eframe = data.frame(reason = 'No second statistic', count=sum(index))
excluded_counts = bind_rows(excluded_counts, eframe)

# exclude percents if stat3 is not missing, see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7113209/
index = !is.na(table_data$stat3) & table_data$statistic == 'percent'
cat('Excluded ',sum(index),' rows that were a percent with a third statistic.\n', sep='')
if(sum(index) > 0){
  table_data = table_data[!index,]
}
eframe = data.frame(reason = 'Percent with third statistic', count=sum(index))
excluded_counts = bind_rows(excluded_counts, eframe)

## test that stat1/sample size is approx equal to stat/100 for 'percent'
check = filter(table_data, statistic=='percent') %>%
  mutate(p = 100*(stat1 / sample_size),
         diff = abs(stat2 - p)) %>%
  filter(diff > 0.5)
# to here, what next?

## look for percents over 100
check = filter(table_data, statistic=='percent', stat2>100)
# to here, what next? exclude?

## save for analysis ##
save(excluded, excluded_counts, pvalues, table_data, design, file='data/analysis_ready.RData')
