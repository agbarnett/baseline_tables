# 3_compare_algorithm_hand.R
# compare the tables from the algorithm with those entered by hand
# August 2021
library(dplyr)
library(stringr)

# get the hand entered data from Amarzaya
load('data/hand_entered_data.RData') # from 2_random_select_hand_read_checks.R
tables = mutate(tables, pmcid = str_remove('^PMC', string=pmcid)) %>% # for merging
  select(-comments)

# get the algorithm data
load('data/analysis_ready_validation.RData')  # from 2_process_extracted_data.R
table_data = select(table_data, pmcid, row, column, starts_with('stat')) # reduce variables

# one at a time
# to here, need to think about merging

this_pmcid = '7254602'
m_hand = tables
m_algorithm = tables

#m1 = filter(tables, pmcid == this_pmcid)
#m2 = filter(table_data, pmcid == this_pmcid)

# look at differences by paper
stats = full_join(m_hand, m_algorithm, by=c('pmcid','row','column')) %>%
  mutate(match_statistic = statistic.x == statistic.y,
         match_stat1 = (stat1.x - stat1.y) < 0.01,
         match_stat2 = (stat2.x - stat2.y) < 0.01) %>% # match within tolerance
  group_by(pmcid) %>%
  summarise(n = n(),
            match_statistic = sum(match_statistic, na.rm = TRUE),
            match_stat1 = sum(match_stat1, na.rm = TRUE), # missing count as non-matches
            match_stat2 = sum(match_stat2, na.rm = TRUE)) %>%
  ungroup()

# stats on overall differences
overall = summarise(stats,
                    n = sum(n),
                    match_statistic = sum(match_statistic),
                    p_match_statistic = round(100*match_statistic / n),
                    match_stat1 = sum(match_stat1),
                    p_match_stat1 = round(100*match_stat1 / n),
                    match_stat2 = sum(match_stat2),
                    p_match_stat2 = round(100*match_stat2 / n))

# statistics by statistic