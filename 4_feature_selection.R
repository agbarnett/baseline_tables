# 4_feature_selection.R
# look at multiple features of the baseline tables
# August 2021
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally) # for scatter plot
source('99_functions.R')

## get the data; select which source of data to use
sources = c('my_search', 'trialstreamer', 'validation')
source = sources[2]
stage = 'model'
source('1_which_data_source.R') # uses `source` and `stage`


## calculate the t-tests
for_model = make_stats_for_bayes_model(indata = table_data) # see 99_functions.R

# correlation of t-tests

# standard deviations of t-tests and sample size
sd_t = group_by(for_model, pmcid) %>%
  summarise(sd_t = sd(t), sd_s=sd(size)) %>%
  replace(is.na(.), 0) # make all NAs zero; for small number with just 1 row

# most common stat
common_stat = group_by(for_model, pmcid, statistic) %>%
  tally() %>%
  arrange(-n) %>%
  slice(1) %>%
  ungroup() 

# Number of table rows per paper
nrows = group_by(table_data, pmcid) %>%
  summarise(nrow = length(unique(row))) %>%
  ungroup()

# number of stats per paper
nstats = group_by(table_data, pmcid) %>%
  summarise(nstat = n()) %>%
  ungroup()

## Benford's law
# repeat for second statistic, but exclude missing (no missing for stat1)
# expected numbers
benford_expected = data.frame(first=1:9) %>% mutate(expected = log(1+(1/first), base=10))
benford_expected = merge(nstats, benford_expected, by=NULL) %>% # merge with study sample size, create all possible combinations by merging on null
  mutate(expected = expected * nstat)
# observed numbers
benford_observed = mutate(table_data,
                          first = as.numeric(str_sub(str_remove_all(stat1, '[^1-9]'),1,1))) %>% # remove negative signs, decimal points and zeros
  group_by(pmcid, first) %>%
  tally(first) %>%
  ungroup() %>%
  right_join(benford_expected, by=c('pmcid','first')) %>%
  mutate(n = ifelse(is.na(n), 0 , n), # replace NA where there are none of these digits
         diff = (n - expected )^2 / expected) %>% # Chi-squared
  group_by(pmcid) %>%
  summarise(N=max(n), sum = sum(diff)) %>%
  mutate(stan = qchisq(0.5, df=N-1),
         stan_sum = sum/ stan) %>% # standardise by size
  select(pmcid, stan_sum) %>%
  rename('benford' = 'stan_sum')

# decimal places
dps = group_by(table_data, pmcid) %>%
  summarise(dp = mean(dp1)) # may need to stratify by statistic?

# merge all statistics per paper; code for multiple merge
all_stats = list(sd_t, 
                 nrows, 
                 dps,
                 benford_observed,
                 common_stat) %>% 
  reduce(left_join, by = "pmcid") %>% 
  filter(nrow > 2) %>% # only for studies with 3+ rows
  mutate(nrow = log(nrow), # log-transformed skewed variables
         sd_t = log(sd_t + 0.1),
         sd_s = log(sd_s + 1))

# scatter plot matrix
ggpairs(all_stats, columns=2:6, aes(color=statistic)) +
  theme_bw()
# could add ,  for categorical variable, percents or not?

# look for outliers, to do
f = filter(all_stats, sd_t >5, sd_s>5)
