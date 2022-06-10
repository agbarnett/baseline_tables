# 99_compare_pvalues.R
# compare the p-values from a t-test and chi-squared test
# June 2022
library(ggplot2)
library(dplyr)
library(tidyr)
source('99_functions_simulate.R')
source('99_functions.R')

# create binary data, use loop to get differing sample sizes
all_tab = NULL
for (loop in 1:100){
  tab = simulate_table1_alternative(loop=loop, prop_continuous=0) %>%
    mutate(stat2= NA)
  all_tab = bind_rows(all_tab, tab)
}
# get p-values using t-statistics
t_stats = make_stats_for_bayes_model(indata = all_tab) %>% # see 99_functions.R
  select(pmcid, row, t, p, size) %>%
  rename('pt' = 'p')

## get p-values using chi-squared
chisq_stats = NULL
pmcids = unique(all_tab$pmcid)
for (this_pmcid in pmcids){
  this_table = filter(all_tab, pmcid == this_pmcid)
  n_rows = max(this_table$row)
  for (r in 1:n_rows){
    test = filter(this_table, row==r) %>%
      mutate(stat2 = sample_size - stat1) %>%
      select(stat1, stat2) %>%
      as.matrix() %>% # create 2x2 table
      chisq.test()
    frame = data.frame(pmcid = this_pmcid, row = r, pchisq = test$p.value)
    chisq_stats = bind_rows(chisq_stats, frame)
  }
}

# compare
to_compare = full_join(chisq_stats, t_stats, by=c('pmcid','row'))
plot = ggplot(data=to_compare, aes(x=pchisq, y=pt, col=size))+
  geom_point()+
  theme_bw()
plot
# definitely not the same!
