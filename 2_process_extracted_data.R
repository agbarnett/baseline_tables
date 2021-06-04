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
index[is.na(index)] = FALSE
cat('Changed ',sum(index),' continuous stats to median.\n', sep='')
table_data$statistic[index] = 'median'
  
## if statistic is percent but stat1 is not integer then change to continuous
# does not work, because also captures %(n) format
#index = is.na(table_data$stat3) & table_data$statistic=='percent' &
#  (table_data$stat1 != round(table_data$stat1))
#index[is.na(index)] = FALSE
#cat('Changed ',sum(index),' percent stats to continuous.\n', sep='')
#table_data$statistic[index] = 'continuous'

# exclude numbers that are not integers
index = table_data$statistic == 'numbers' & 
    (round(table_data$stat1) != table_data$stat1 | round(table_data$stat2) != table_data$stat2)
index[is.na(index)] = FALSE
cat('Excluded ',sum(index),' numbers that are not integers.\n', sep='')
if(sum(index)>0){
  table_data = table_data[!index,]
  eframe = data.frame(reason = 'Numbers that are not integers', count=sum(index))
  excluded_counts = bind_rows(excluded_counts, eframe)
}

# exclude if there is no second statistic for continuous (and count excluded); percents can live with n
index = is.na(table_data$stat2) & (table_data$statistic %in% c('continuous','median','min_max'))
cat('Excluded ',sum(index),' non-percent rows with no second statistic.\n', sep='')
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
# often picks up errors in papers, e.g., https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7106867/, 'B-ALL' percents
check = filter(table_data, statistic=='percent') %>%
  mutate(p = 100*(stat1 / sample_size),
         diff = abs(stat2 - p)) %>%
  filter(diff > 0.5) %>%
  select(pmcid, row) # exclude all rows
# now exclude
table_data = anti_join(table_data, check, by=c('pmcid','row'))
#
eframe = data.frame(reason = 'Not a percent', count=nrow(check))
excluded_counts = bind_rows(excluded_counts, eframe)

## look for percents over 100 - not needed, captured by above
#check = filter(table_data, statistic=='percent', stat2 > 100)

## save for analysis ##
save(excluded, excluded_counts, pvalues, table_data, design, file='data/analysis_ready.RData')
