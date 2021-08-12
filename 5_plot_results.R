# 5_plot_results.R
# plot the results from the Bayesian model and flag studies
# August 2021
library(ggplot2)
library(stringr)

load('results/bugs_real.RData') # from 4_model.R
#load('results/bugs_simulated.RData') # from 4_model_simulated.R

# highlight papers with a high probability
# Get stats on probabilities variance and mean flags
# one_mean = Pr(mean not zero)
# one_invar = Pr(precision too low)
# three_mean = Pr(precision too high)
equal.1 = data.frame(apply(bugs$sims.matrix, FUN = is.this, i=1, 2))
names(equal.1) = 'one'; equal.1$var = rownames(equal.1)
equal.3 = data.frame(apply(bugs$sims.matrix, FUN = is.this, i=3, 2))
names(equal.3) = 'three'
stats = bind_cols(equal.1, equal.3) %>%
  mutate(mean = as.numeric(str_detect(var, 'mean')),
         mean = ifelse(mean==1, 'mean','invar'),
         study = as.numeric(str_remove_all(var, '[^0-9]'))) %>% # study number
  select(-var) %>%
  group_by(study) %>%
  pivot_wider(names_from=mean, values_from=c('one','three')) %>%
  select(-three_mean) %>%
  left_join(study_numbers, by='study') %>%
  arrange(three_invar) %>% 
  ungroup()

# show the distributions of selected papers
n_select = 3 # number to select from top and bottom
top_bottom = filter(stats, one_mean < 0.01, one_invar< 0.01) %>% # those without mean issue and not too big a variance
  arrange(three_invar) %>%
  mutate(rown = row_number()) %>%
  filter(rown > max(rown) - n_select | rown <= n_select )
to_plot = left_join(top_bottom, for_model, by=c('study','pmcid')) %>%
  mutate(type = as.numeric(rown <= n_select))
gplot = ggplot(data=to_plot, aes(x=t, fill=factor(type)))+
  scale_fill_manual("Potential\nissue", values=c('goldenrod1','dodgerblue'), labels=c('Yes','No'))+
  geom_histogram()+
  xlab('Standardised difference')+
  ylab('Frequency')+
  facet_wrap(~pmcid)+
  theme_bw()
gplot
jpeg('figures/flagged_papers.jpg', width=5, height=4, units='in', res=400)
print(gplot)
dev.off()

