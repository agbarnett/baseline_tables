# get the multiplier 
flag_stats = filter(all_stats, nicevar=='Precision') %>%
  left_join(table_summaries, by='pmcid') %>% # add table features
  dplyr::filter(n_cols ==2,
         lower > 0) %>% # lower positive (high precision)
  left_join(design, by='pmcid')
#
flag_stats$pmcid
