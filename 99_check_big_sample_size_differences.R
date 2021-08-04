# 99_check_big_sample_size_differences.R
# check for big differences in sample sizes in the same study
# June 2021

load('data/analysis_ready.RData') # from 2_process_extracted_data.R

#
check = filter(table_data, row==1) %>% # just take top row as representative
  group_by(pmcid) %>%
  mutate(med = median(sample_size),
         diff = abs(sample_size - med)) %>%
  ungroup() %>%
  filter(diff > 100)
