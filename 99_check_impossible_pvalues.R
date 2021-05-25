# 99_check_impossible_pvalues.R
# check for comparisons using a 2x2 table where the p-value cannot be below 0.05
# May 2021

results = NULL
for (n_per_group in 2:10){
  most_extreme = matrix(c(0,n_per_group,n_per_group,0), ncol=2) # most extreme split of data
  fisher = fisher.test(most_extreme)
  frame = data.frame(n =n_per_group, pvalue=fisher$p.value)
  results = bind_rows(results, frame)
}
results = mutate(results, below = pvalue<0.05)

# so only groups of size 3 per group
# could extend to look at uneven group sizes