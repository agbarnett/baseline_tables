# 1_excluded_by_hand.R
# studies to exclude after looking at paper
# April 2021

hand_exclude = read.table(header=TRUE, sep='!', text='
pmc!reason
7660783!Table looks like baseline table, but columns use 1 year outcomes
')

save(hand_exclude, file='data/additional_exclusions.RData')
