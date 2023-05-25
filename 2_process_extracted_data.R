# 2_process_extracted_data.R
# checks and processing of extracted data with some edits and exclusions
# all applied by row for consistency (so whole row needs to change)
# June 2022
library(dplyr)
library(stringr)
#library(tidylog, warn.conflicts = FALSE)

## get the data
# select which source of data to use
sources = c('my_search', 'trialstreamer', 'validation', 'carlisle', 'saitoh')
source = sources[2]
stage = 'processing'
source('1_which_data_source.R') # uses `source` and `stage`

# set changed flag to FALSE for starters
table_data = mutate(table_data, changed = FALSE) 
excluded_counts = NULL

## pre) exclude tables with 8 or more columns, as very likely to be subgroups, e.g., PMC2515903
col_count = group_by(table_data, pmcid) %>%
  summarise(max = max(column)) %>%
  filter(max >= 8) %>%
  select(pmcid)
# remove from data
table_data = anti_join(table_data, col_count, by='pmcid')
cat('Excluded ', nrow(col_count), ' trials with 8 or more columns.\n', sep='')
# record excluded
eframe = data.frame(reason = 'Trials with eight or more columns', count = nrow(col_count))
excluded_counts = bind_rows(excluded_counts, eframe)

## 0) very small sample size
check = filter(table_data,
               sample_size <= 2) %>%
  group_by(pmcid) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  unique()
cat('Excluded ',nrow(check),' trials for sample sizes of two or fewer.\n', sep='')
# record excluded
eframe = data.frame(reason = 'Trials with sample size under three', count = nrow(check))
excluded_counts = bind_rows(excluded_counts, eframe)
# remove from data
if(nrow(check) > 0){
  check = select(check, pmcid)
  table_data = anti_join(table_data, check, by='pmcid')
}
remove(check)

## a) non-integer sample size
check = mutate(table_data,
               diff = as.integer(sample_size) != sample_size) %>%
  filter(diff == TRUE) %>%
  group_by(pmcid) %>%
  summarise(n=n()) %>%
  ungroup() 
cat('Excluded ', sum(check$n), ' rows for non-integer sample sizes from ', nrow(check), ' trials.\n', sep='')
# record excluded
eframe = data.frame(reason = 'Trials with non-integer sample size', count=nrow(check))
excluded_counts = bind_rows(excluded_counts, eframe)
# remove from data
if(nrow(check) > 0){
  check = select(check, pmcid) # remove `n`
  table_data = anti_join(table_data, check, by='pmcid')
}
remove(check)

## b) if numbers but dp2 > 0 then change to percent
check = filter(table_data,
               statistic == 'numbers',
               dp2 != 0) %>%
  group_by(pmcid, row) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  filter(n > 0) %>% # any on the row
  select(pmcid, row, n) %>%
  mutate(n = 99) %>% # change all n to arbitrary number
  unique()
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(
    changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
    statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # update statistic for those that are a percent
  select(-n) # no longer needed
cat('Changed ', nrow(check), ' rows with numbers stats to percent.\n', sep='')
remove(check)

## c) look for n/N (%) wrongly marked as continuous (must go before median switch)
check = filter(table_data, 
               statistic == 'continuous') %>% # must be continuous
  group_by(pmcid, row) %>%
  mutate(p = round(100*(stat1/stat2)), # stat1 = n, stat2=N
         match = abs(p - stat3)) %>% # stat3 = %
  summarise(total = sum(match), 
            n=n()) %>% # will be missing for some, that's okay as we want all rows
  ungroup() %>%
  filter(total < 0.5) %>% # allow for little rounding error
  select(pmcid, row, n) %>%
  mutate(n = 99) %>% # change n to arbitrary number
  unique()
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(
    changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
    statistic = ifelse(is.na(n) == FALSE, 'percent', statistic), # update statistic for those that are a percent
    stat2 = ifelse(is.na(n) == FALSE, stat3, stat2)) %>% # shift % to second statistic
  select(-n) # no longer needed
cat('Changed ', nrow(check), ' rows with continuous stats to percent.\n', sep='')
remove(check)

## d) if statistic is 'continuous', but there are three stats, and stat1 is in stat2 to stat3 then change to 'median', e.g. PMC7362422
# removed "is.na(table_data$stat4) &", because PMC6537409 ...
# but this doesn't help with PMC7469713
# Examples where it does not work: "7149340" "7469713", these have age (SD), range; however we can avoid this by specifying stat4 empty
# but also examples where it does, e.g., PMC3502261
check = filter(table_data, 
               is.na(stat4), # see notes above, must be missing so just three stats
               statistic=='continuous') %>% # must be continuous
  group_by(pmcid, row) %>%
  mutate(s1 = stat1 >= stat2 & !is.na(stat3),
         s2 = stat1 <= stat3 & !is.na(stat3),
         both = (s1 + s2)/2) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(both), n=n()) %>%
  ungroup() %>%
  filter(total == n) %>% # must apply to all rows
  select(pmcid, row, n) %>%
  unique()
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(
    changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
    statistic = ifelse(is.na(n) == FALSE, 'median', statistic)) %>% # update statistic
  select(-n) # no longer needed
cat('Changed ', nrow(check), ' rows with continuous stats that could be median.\n', sep='')
remove(check)

## e) if statistic is 'median', but stat3 is empty then change to continuous, e.g, PMC7315526 ...
check = filter(table_data, 
               statistic=='median') %>% # must be continuous
  group_by(pmcid, row) %>%
  mutate(missing = is.na(stat3)) %>% # 
  summarise(total = sum(missing), n=n()) %>%
  ungroup() %>%
  filter(total == n) %>% # must apply to all rows
  select(pmcid, row, n) %>%
  mutate(n = 99) %>% # change n to arbitrary number
  unique()
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
         statistic = ifelse(is.na(n) == FALSE, 'continuous', statistic)) %>% # update statistic for those that are a median
  select(-n) # no longer needed
cat('Changed ', nrow(check), ' rows with median stats to continuous.\n', sep='')
#changed_to_percent = check # used below
remove(check)

## f) if statistic is 'continuous', but stat2 is missing and dp1 is 0 then change to percent
# does change some means, for example, last row of PMC6966838
check = filter(table_data, 
               statistic=='continuous') %>% # must be continuous
  group_by(pmcid, row) %>%
  mutate(d = as.numeric(dp1==0),
         m = as.numeric(is.na(stat2)),
         both = d + m) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(both), n=n()) %>%
  ungroup() %>%
  filter(total == 2*n) %>% # must apply to all rows
  select(pmcid, row, n) %>%
  mutate(n = 99) %>% # change n to arbitrary number
  unique()
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
         statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # update statistic for those that are a percent
  select(-n) # no longer needed
cat('Changed ', nrow(check), ' rows with continuous stats to percent.\n', sep='')
changed_to_percent = check # used below
remove(check)

## g) if statistic is percent but stat1 is not integer then change to continuous
# does not work, because also captures %(n) format
#index = is.na(table_data$stat3) & table_data$statistic=='percent' &
#  (table_data$stat1 != round(table_data$stat1))
#index[is.na(index)] = FALSE
#cat('Changed ',sum(index),' percent stats to continuous.\n', sep='')
#table_data$statistic[index] = 'continuous'

## g) if statistic is `percent` with just one stat that has decimal places then assume it is a % and transform to n (%)
# works for rows 16 to 18 of PMC6966838
check = filter(table_data, 
               statistic == 'percent') %>% # 
  group_by(pmcid, row) %>%
  mutate(m = as.numeric(is.na(stat2)),
         d = as.numeric(dp1 > 0)) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(m) + sum(d), n=n()) %>%
  ungroup() %>%
  filter(total == 2*n) %>% # must be every row
  select(pmcid, row, n) %>%
  mutate(n = 99) %>% # change n to arbitrary number
  unique()
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
         stat1 = ifelse(is.na(n) == FALSE, round((stat1/100)*sample_size), stat1), # change % to n
         dp1 = ifelse(is.na(n) == FALSE, 0, dp1)) %>% # and update d.p.
  select(-n) # no longer needed
cat('Changed ', nrow(check), ' rows with %`s to numerators.\n', sep='')
remove(check)

## h1a) check if format is % n instead of n %, and switch stats if it is, e.g., PMC5088368
check = filter(table_data, 
               !is.na(stat2), # cannot be missing
               statistic %in% c('number','percent')) %>% # 
  mutate(d = dp2 == 0, # numerator must be integer
         equal = stat1==stat2, # check that n % are the same
         pcheck = 100*(stat2/sample_size),
         pdiff = abs(stat1 - pcheck)) %>%
  group_by(pmcid, row) %>%
  summarise(n = n(),
            sume = sum(equal),
            sumd = sum(d),
            total_diff = sum(pdiff)) %>%
  filter(total_diff < 1/n,
         sume < n, 
         sumd == n) %>% # must all be zero dp
  ungroup() %>%
  select(pmcid, row) %>%
  mutate(n = 99) # dummy flag
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
         temp = stat1, # temporary
         stat1 = ifelse(is.na(n) == FALSE, stat2, stat1), # change first stat
         stat2 = ifelse(is.na(n) == FALSE, temp, stat2), # change second stat
         stat3 = ifelse(is.na(n) == FALSE, NA, stat3), # blank third stat
         dp1 = ifelse(is.na(n) == FALSE, 0, dp1), # change first dp
         statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # and update d.p.
  select(-n, -temp) # no longer needed
cat('Changed ', nrow(check), ' rows that were `% n` to `n %`.\n', sep='')
remove(check)

## h1b) repeat previous for continuous, e.g., PMC7399917
check = filter(table_data, 
               !is.na(stat2), # cannot be missing
               is.na(stat3), # must be missing for continuous (different to above)
               statistic %in% c('continuous','median','ci')) %>% # 
  mutate(d = dp2 == 0, # numerator must be integer
         equal = stat1==stat2, # check that n % are the same (excude if they are)
         pcheck = 100*(stat2/sample_size),
         pdiff = abs(stat1 - pcheck)) %>%
  group_by(pmcid, row) %>%
  summarise(n = n(),
            sume = sum(equal),
            sumd = sum(d),
            total_diff = sum(pdiff)) %>%
  filter(total_diff < 1/n,
         sume < n, 
         sumd == n) %>% # must all be zero dp
  ungroup() %>%
  select(pmcid, row) %>%
  mutate(n = 99) # dummy flag
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
         temp = stat1, # temporary
         stat1 = ifelse(is.na(n) == FALSE, stat2, stat1), # change first stat
         stat2 = ifelse(is.na(n) == FALSE, temp, stat2), # change second stat
         dp1 = ifelse(is.na(n) == FALSE, 0, dp1), # change first dp
         statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # and update d.p.
  select(-n, -temp) # no longer needed
cat('Changed ', nrow(check), ' rows that were `% n` to `n %`.\n', sep='')
remove(check)

## h2) exclude numbers/numerators in percents/numbers that are not integers
check = filter(table_data, 
               statistic %in% c('number','percent')) %>% # 
  group_by(pmcid, row) %>%
  mutate(s1 = dp1 > 0) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(s1)) %>%
  ungroup() %>%
  filter(total > 0) %>% # can apply to any number in the row
  select(pmcid, row, total) %>%
  mutate(total = 99) %>% # change to arbitrary number
  unique()
# now remove in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  filter(is.na(total)) %>% # exclude all results from check
  select(-total)
cat('Removed ', nrow(check), ' rows where numerators in percents were not integers.\n', sep='')
eframe = data.frame(reason = 'Numbers that are not integers', count=nrow(check))
excluded_counts = bind_rows(excluded_counts, eframe)
remove(check)

## j) exclude percents if stat3 is not missing, see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7113209/
# turned off for now PMC7254602
off = function(){
index = !is.na(table_data$stat3) & table_data$statistic == 'percent'
cat('Excluded ',sum(index),' rows that were a percent with a third statistic.\n', sep='')
if(sum(index) > 0){
  table_data = table_data[!index,]
}
eframe = data.frame(reason = 'Percent with third statistic', count=sum(index))
excluded_counts = bind_rows(excluded_counts, eframe)
}

## k) test that stat1/sample size is approx equal to stat/100 for 'percent'
# often picks up errors in papers, e.g., https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7106867/, 'B-ALL' percents
# or picks up where denominators change, e.g. PMC6966838
check = filter(table_data, statistic == 'percent') %>%
  mutate(p = 100*(stat1 / sample_size),
         diff = abs(stat2 - p)) %>%
  filter(diff > 1) %>% # could be more generous here? maybe 1.5?
  select(pmcid, row) %>% # exclude all rows
  unique()
# now exclude ...
table_data = anti_join(table_data, check, by=c('pmcid','row'))
# ... and add to counts
eframe = data.frame(reason = 'Not a percent', count=nrow(check))
excluded_counts = bind_rows(excluded_counts, eframe)

## l) if continuous, median or min max but stat1/N == stat2 then switch to percent
check = filter(table_data, 
               statistic %in% c('min_max','median','continuous')) %>% # must be non-percent
  mutate(p = 100*(stat1 / sample_size),
         diff = abs(stat2 - p), # percent difference, number on % scale
         match = diff <= 1.5 & dp1 == 0) %>% # some round error, allow 1.5%
  group_by(pmcid, row) %>% # do at the row level
  summarise(total = sum(match), n=n()) %>% # will be missing for some, that's okay as we want all rows
  ungroup() %>%
  filter(total == n) %>% # must apply to all rows
  select(pmcid, row) %>%
  mutate(n = 99) %>% # change all n to arbitrary number
  unique()
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(
    changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
    statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # update statistic for those that are a percent
  select(-n) # no longer needed
cat('Changed ', nrow(check), ' rows with continuous stats to percent.\n', sep='')
remove(check)

## m) remove percent if stat1 > n (r > n)
impossible_percent = filter(table_data, statistic=='percent', stat1 > sample_size)
# if was changed to percent because of single stat then change back 
impossible_percent_back = left_join(impossible_percent, changed_to_percent, by=c('pmcid','row')) %>%
  select(pmcid, row, n) %>%
  mutate(n = 99) %>% # change to arbitrary number
  unique()
if(nrow(impossible_percent_back) > 0){
  table_data = left_join(table_data, impossible_percent_back, by=c('pmcid','row')) %>%
    mutate(
      changed = ifelse(is.na(n) == FALSE, FALSE, changed), # change flag back to false
      statistic = ifelse(is.na(n) == FALSE, 'continuous', statistic)) %>% # change back to continuous
    select(-n) # no longer needed
}
impossible_percent_exclude = anti_join(impossible_percent, changed_to_percent, by=c('pmcid','row')) %>%
  select(pmcid, row) %>%
  unique()
# if not then exclude ...
if(nrow(impossible_percent_exclude) > 0){
  table_data = anti_join(table_data, impossible_percent_exclude, by=c('pmcid','row'))
}
# ... and add to counts
eframe = data.frame(reason = 'Guessed percent but n > sample size (rows)', count=nrow(impossible_percent))
excluded_counts = bind_rows(excluded_counts, eframe)

## n) if statistic is percent but actual percent is outside 0 to 100
check = filter(table_data, 
               statistic=='percent') %>% # 
  filter(stat2 < 0 | stat2> 100)
nrow(check) # has been zero, so no need to change

## o) look for negative numerators for percents
check = filter(table_data, statistic=='percent', stat1 < 0) %>%
  select('pmcid','row','column') %>%
  mutate(n = 1) # flag
# now remove in data
table_data = left_join(table_data, check, by=c('pmcid','row','column')) %>%
  filter(is.na(n)) %>% # exclude all results from check
  select(-n)
cat('Removed ', nrow(check), ' results where numerators in percents were negative.\n', sep='')
# ... and add to counts
if(nrow(check)>0){
  eframe = data.frame(reason = 'Negative percent', count=nrow(check))
  excluded_counts = bind_rows(excluded_counts, eframe)
}
remove(check)

## p) if continuous but stat2 (SD) is zero, e.g., PMC8096923, then add small constant
check = filter(table_data, 
               statistic=='continuous', # must be continuous
               stat2 ==0) %>%
  select(pmcid, row, column) %>%
  mutate(n = 1) # change flag
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row','column')) %>%
  mutate(
    changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
    stat2 = ifelse(is.na(n) == FALSE, (stat1+0.0001)*0.01, stat2)) %>% # update zero SD to 1% of mean, add small constant to mean in case mean is zero
  select(-n) # no longer needed
cat('Changed ', nrow(check), ' zero SDs.\n', sep='')
remove(check)

#### confidence intervals
## q1) if CI, but stat3 (upper interval) is less than stat2 (lower interval) then delete
check = filter(table_data, 
               statistic=='ci', # must be ci
               !is.na(stat2),
               !is.na(stat3),
               stat3 < stat2) %>%
  select(pmcid, row, column) %>%
  mutate(n = 1) %>% # arbitrary non-zero flag
  unique()
# now remove in data
table_data = left_join(table_data, check, by=c('pmcid','row','column')) %>%
  filter(is.na(n)) %>% # exclude all results from check
  select(-n)
cat('Removed ', nrow(check), ' results where upper CI interval was less than lower interval.\n', sep='')
# ... and add to counts
if(nrow(check) > 0){
  eframe = data.frame(reason = 'Upper CI interval less than lower (results)', count=nrow(check))
  excluded_counts = bind_rows(excluded_counts, eframe)
}
remove(check)

## q2) if CI, but no stat3 or stat1 not in stat2 to stat3 then change to continuous
check = filter(table_data, 
               statistic=='ci', # must be ci
               is.na(stat3) | stat1 < stat2 | stat1 > stat3) %>%
  select(pmcid, row) %>%
  mutate(n = 1) %>% # change flag
  select(pmcid, row, n) %>%
  unique()
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(
    changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
    statistic = ifelse(is.na(n) == FALSE, 'continuous', statistic)) %>% # update statistic for those that are a percent
  select(-n) # no longer needed
cat('Changed ', nrow(check), ' rows with CI stats to continuous.\n', sep='')
remove(check)

## q3) calculate SD for confidence intervals
keep = filter(table_data, statistic != 'ci') # does not need changing
redone = filter(table_data, statistic == 'ci') %>%
  mutate(z = qnorm(0.975), # assume 95% CI
         stat2 = sqrt(sample_size)*(stat3 - stat2)/(2*z),
         stat3 = NA) %>%
  select(-z)
# put back together
table_data = bind_rows(redone, keep)

## r) if original statistic is a percent, but stat1 in stat2 to stat3 then change to median, e.g., PMC3163659; must be all rows
check = filter(table_data, 
               statistic == 'percent') %>% # must be percent
  mutate(s1 = stat1 >= stat2 & stat1 <= stat3 & !is.na(stat3)) %>%
  group_by(pmcid, row) %>%
  summarise(total = sum(s1), n=n()) %>%
  ungroup() %>%
  filter(total == n) %>% # Must be all rows
  select(pmcid, row, n) %>%
  unique()
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(
    changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
    statistic = ifelse(is.na(n) == FALSE, 'median', statistic)) %>% # update statistic for those that are a percent
  select(-n) # no longer needed
cat('Changed ', nrow(check), ' rows with percent stats to median.\n', sep='')
remove(check)

## z) if continuous but no stat2 and stat1 is < sample_size and stat1>0 and dp1=0 then switch to percent
check = filter(table_data, 
               statistic %in% c('median','continuous')) %>% # 
  mutate(s1 = dp1==0 & stat1 <= sample_size & stat1 >= 0 & is.na(stat2)) %>%
  group_by(pmcid, row) %>%
  summarise(total = sum(s1), 
            n=n()) %>%
  ungroup() %>%
  filter(total == n) %>% # Must be all rows
  select(pmcid, row, n) %>%
  unique()
# now change in data
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(
    changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
    statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # update statistic 
  select(-n) # no longer needed
cat('Changed ', nrow(check), ' rows to percent from median/continuous.\n', sep='')
remove(check)

## w) if continuous but stat2 is negative then remove
check = filter(table_data, 
               !is.na(stat2),
               stat2 <0,
               statistic %in% c('continuous')) %>% # 
  select(pmcid, row) %>%
  unique() %>%
  mutate(n = 99) # dummy flag
# remove from data
table_data = anti_join(table_data, check, by=c('pmcid','row'))
cat('Excluded ', nrow(check), ' rows with negative standard deviation.\n', sep='')
# record excluded
eframe = data.frame(reason = 'Rows with negative standard deviation', count = nrow(check))
excluded_counts = bind_rows(excluded_counts, eframe)
# now remove in data 
table_data = left_join(table_data, check, by=c('pmcid','row')) %>%
  mutate(
    changed = ifelse(is.na(n) == FALSE, TRUE, changed), # change flag
    statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # update statistic 
  select(-n) # no longer needed
cat('Removed ', nrow(check), ' rows with negative standard deviation.\n', sep='')
remove(check)

## b2) if numbers but stat1 is actually %
# error with column here
check = filter(table_data,
               statistic == 'numbers',
               dp1 > 0) %>% # not an integer
  mutate(stat_replace = round(sample_size * stat1/100)) %>% # re-calculate
  select(pmcid, row, column, stat_replace) %>%
  unique()
# now change in data (merged by row and column)
table_data = left_join(table_data, check, by=c('pmcid','row','column')) %>%
  mutate(
    changed = ifelse(is.na(stat_replace) == FALSE, TRUE, changed), # change flag
    dp1 = ifelse(is.na(stat_replace) == FALSE, 0, dp1),
    stat1 = ifelse(is.na(stat_replace) == FALSE, stat_replace, stat1)) %>% # update statistic
  select(-stat_replace) # no longer needed
cat('Changed ', nrow(check), ' statistics that were a number statistic with percents to a number.\n', sep='')
remove(check)

## i) exclude if there is no second statistic for continuous (and count excluded); percents can live with n (just stat1); e.g., PMC6435872
# not done by row but for any cell
index = is.na(table_data$stat2) & (table_data$statistic %in% c('continuous','median','min_max'))
cat('Excluded ', sum(index), ' non-percent results with no second statistic.\n', sep='')
if(sum(index) > 0){
  table_data = table_data[!index,]
}
# record excluded
eframe = data.frame(reason = 'No second statistic for continuous/median (results)', count=sum(index))
excluded_counts = bind_rows(excluded_counts, eframe)

## excluded if just one result per row (nothing to compare with) (run this last)
n_per_row = group_by(table_data, pmcid, row) %>%
  tally() %>%
  ungroup() %>%
  filter(n==1) 
# now exclude ...
if(nrow(n_per_row) > 0){
  table_data = anti_join(table_data, n_per_row, by=c('pmcid','row'))
# ... and add to counts
  eframe = data.frame(reason = 'Only one result per row', count=nrow(n_per_row))
  excluded_counts = bind_rows(excluded_counts, eframe)
}


# results below not avaliable for Carlisle data
if (source %in% c('carlisle','saitoh') == FALSE){

## p-values
pvals = mutate(pvals,
               pvalue = as.numeric(pvalue)) %>% # can ignore warning here
  filter(!is.na(pvalue), # remove missing 
         pvalue <= 1, # only keep those from 0 to 1
         pvalue >= 0)
#hist(pvals$pvalue)

## Excluded
# add algorithm errors to excluded
eframe = data.frame(reason = 'Algorithm error', count=length(errors))
excluded_counts = bind_rows(excluded_counts, eframe)
# combine counts with same exclusion reason
excluded_counts = group_by(excluded_counts, reason) %>%
  summarise(count = sum(count)) %>%
  arrange(-count) %>%
  ungroup()

# consolidate countries
design = mutate(design, 
                affiliation = str_remove_all(pattern='\\s*.*,', affiliation), # only keep text after last comma
                affiliation = str_squish(affiliation),
                #affiliation = str_remove_all(affiliation) # remove anything but letters?
                affiliation = ifelse(str_detect(affiliation, pattern='China') , 'China', affiliation),
                affiliation = case_when(
                  affiliation == 'Brasil' ~ 'Brazil',
                  affiliation == 'The Netherlands' ~ 'Netherlands',
                  affiliation == 'the Netherlands' ~ 'Netherlands',
                  affiliation == 'California' ~ 'USA',
                  affiliation == 'New York' ~ 'USA',
                  affiliation == 'U.S' ~ 'USA',
                  affiliation == 'U.S.' ~ 'USA',
                  affiliation == 'U.S.A.' ~ 'USA',
                  affiliation == 'United States' ~ 'USA',
                  affiliation == 'England' ~ 'UK',
                  affiliation == 'Wales' ~ 'UK',
                  affiliation == 'Scotland' ~ 'UK',
                  affiliation == 'Northern Ireland' ~ 'UK',
                  affiliation == 'United Kingdom' ~ 'UK',
                  affiliation == 'Korea (Republic of)' ~ 'South Korea',
                  affiliation == 'Korea' ~ 'South Korea',
                  is.character(affiliation) == TRUE ~ affiliation # otherwise
                ))

# re-number columns so they are consecutive
table_data = group_by(table_data, pmcid) %>%
  mutate(column = as.numeric(as.factor(column))) %>%
  ungroup()

## save for analysis ##
save(excluded, excluded_counts, pvals, errors, pvalues, table_data, design, file=outfile)
}

if (source %in% c('carlisle','saitoh')){
## save for analysis ##
save(excluded_counts, table_data, file=outfile)
}
