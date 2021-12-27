# 1_excluded_by_hand.R
# studies to exclude after looking at paper
# August 2021
library(dplyr)

hand_exclude = read.table(header=TRUE, sep='!', text='
pmcid!reason
PMC7660783!Follow-up results in baseline table
PMC7059305!Not an RCT
PMC4518115!Just one column in table
PMC6417853!Complex format
PMC7325781!Not an RCT
PMC7353038!Complex format
PMC4531018!Complex format
PMC6797438!Complex format
') 

save(hand_exclude, file='data/additional_exclusions.RData')
