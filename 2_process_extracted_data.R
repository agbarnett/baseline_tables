# 2_process_extracted_data.R
# checks and processing of extracted data with some edits and exclusions
# all applied by row for consistency (so whole row needs to change)
# August 2021
library(dplyr)
library(tidylog, warn.conflicts = FALSE)

# get the data
load('data/extracted.RData') # 1_extract_tables_pmc.R
excluded_counts = NULL

## a) look for n/N (%) wrongly marked as continuous (must go before median switch)
check = filter(table_data, 
               statistic=='continuous') %>% # must be continuous
  group_by(pmcid, row) %>%
  mutate(p = round(100*(stat1/stat2)),
         match = abs(p - stat3)) %>%
  summarise(total = sum(match), n=n()) %>% # will be missing for some, that's okay as we want all rows
  filter(total < 0.5) # allow for little rounding error
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(statistic = ifelse(is.na(n) == FALSE, 'percent', statistic), # update statistic for those that are a percent
         stat2 = ifelse(is.na(n) == FALSE, stat3, stat2)) %>% # shift N to second statistic
  select(-total, -n) # no longer needed
cat('Changed ', sum(check$n), ' continuous stats to percent.\n', sep='')
remove(check)

## b) if statistic is 'continuous', but there are three+ stats, and stat1 is in stat2 to stat3 then change to 'median', e.g. PMC7362422
# removed "is.na(table_data$stat4) &", because PMC6537409 ...
# but this doesn't help with PMC7469713
check = filter(table_data, 
               statistic=='continuous') %>% # must be continuous
  group_by(pmcid, row) %>%
  mutate(s1 = stat1 >= stat2,
         s2 = stat1 <= stat3,
         both = s1 + s2) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(both), n=n()) %>%
  filter(total == n) # must apply to all rows
# not used this yet, as there are examples where it does not work: "7149340" "7469713"
remove(check)

## c) if statistic is 'continuous', but stat2 is missing and dp1 is 0 then change to percent
check = filter(table_data, 
               statistic=='continuous') %>% # must be continuous
  group_by(pmcid, row) %>%
  mutate(d = as.numeric(dp1==0),
         m = as.numeric(is.na(stat2)),
         both = d + m) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(both), n=n()) %>%
  filter(total == 2*n) # must apply to all rows
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # update statistic for those that are a percent
  select(-total, -n) # no longer needed
cat('Changed ', sum(check$n), ' continuous stats to percent.\n', sep='')
remove(check)

## d) if statistic is percent but stat1 is not integer then change to continuous
# does not work, because also captures %(n) format
#index = is.na(table_data$stat3) & table_data$statistic=='percent' &
#  (table_data$stat1 != round(table_data$stat1))
#index[is.na(index)] = FALSE
#cat('Changed ',sum(index),' percent stats to continuous.\n', sep='')
#table_data$statistic[index] = 'continuous'

## e) if percent with just one stat that has decimal places then assume it is a % and transform to n (%)
check = filter(table_data, 
               statistic == 'percent') %>% # 
  group_by(pmcid, row) %>%
  mutate(m = as.numeric(is.na(stat2)),
         d = as.numeric(dp1>0)) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(m) + sum(d), n=n()) %>%
  filter(total == 2*n) # must be every row
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(stat1 = ifelse(is.na(n) == FALSE, round((stat1/100)*sample_size), stat1), # change % to n
         dp1 = ifelse(is.na(n) == FALSE, 0, dp1)) %>% # and update d.p.
  select(-total, -n) # no longer needed
cat('Changed ', sum(check$n), ' %`s to numerators.\n', sep='')
remove(check)

## exclude numbers or numerators in percents that are not integers
check = filter(table_data, 
               statistic %in% c('number','percent')) %>% # 
  group_by(pmcid, row) %>%
  mutate(s1 = dp1 > 0) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(s1)) %>%
  filter(total > 0) # can apply to any row
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  filter(is.na(total)) %>% # exclude all results from check
  select(-total)
cat('Removed ', sum(check$total), ' rows where numerators in percents were not integers.\n', sep='')
eframe = data.frame(reason = 'Numbers that are not integers', count=sum(check$total))
excluded_counts = bind_rows(excluded_counts, eframe)
remove(check)

# exclude if there is no second statistic for continuous (and count excluded); percents can live with n
# not done by row but for any cell
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
# now exclude ...
table_data = anti_join(table_data, check, by=c('pmcid','row'))
# ... and add to counts
eframe = data.frame(reason = 'Not a percent', count=nrow(check))
excluded_counts = bind_rows(excluded_counts, eframe)

## remove percent if stat1 > n (r > n)
impossible_percent = filter(table_data, statistic=='percent', stat1 > sample_size)
# now exclude ...
table_data = anti_join(table_data, impossible_percent, by=c('pmcid','row'))
# ... and add to counts
eframe = data.frame(reason = 'Not a percent', count=nrow(impossible_percent))
excluded_counts = bind_rows(excluded_counts, eframe)

# check - still working on 
filter(table_data, statistic=='percent', dp1 > 0)

## excluded if just one result per row (nothing to compare with) (run this last)
n_per_row = group_by(table_data, pmcid, row) %>%
  tally() %>%
  ungroup() %>%
  filter(n==1) 
# now exclude ...
table_data = anti_join(table_data, n_per_row, by=c('pmcid','row'))
# ... and add to counts
eframe = data.frame(reason = 'Only one result per row', count=nrow(n_per_row))
excluded_counts = bind_rows(excluded_counts, eframe)

## look for percents over 100 - not needed, captured by above
#check = filter(table_data, statistic=='percent', stat2 > 100)

## save for analysis ##
save(excluded, excluded_counts, pvalues, table_data, design, file='data/analysis_ready.RData')
