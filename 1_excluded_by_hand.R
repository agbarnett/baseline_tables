# 1_excluded_by_hand.R
# studies to exclude after looking at paper
# June 2021
library(dplyr)

hand_exclude = read.table(header=TRUE, sep='!', text='
pmc!reason
7660783!Follow-up results in baseline table
7059305!Not an RCT
') %>%
  mutate(pmc = as.character(pmc)) # for merging

save(hand_exclude, file='data/additional_exclusions.RData')
