# 4_model_cumulative.R
# Fits a Bayesian model to the baseline table data extracted from RCTs published on pubmed central
# version that looks at cumulative evidence over trials for Saitoh data
# June 2022
library(gridExtra)
library(ggplot2)
library(ggpubr) # to get the legend
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
library(uniftest) # for test of uniform distribution
library(stringr)
library(dplyr)
library(tidyr)
library(TeachingDemos)
seed = char2seed('cobblers') # random number seed for computational reproducibility
library(R2WinBUGS)
source('4_make_winbugs.R')
source('4_MCMC_basics.R')
source('99_functions.R')
model.file = 'bugs_model_no_hyper_single_study.txt' # bugs model for single file

# get the data
source = 'saitoh'
stage = 'model'
source('1_which_data_source.R') # uses `source` and `stage`

# add trial number
table_data = mutate(table_data,
                    trial_num = as.numeric(str_remove(pattern='saitoh', pmcid))) # make trial numbers
n_trials = max(table_data$trial_num)

# function to make nice results
neaten_results = function(in_res, type, n_table_rows){
  store = data.frame(in_res$summary[,c(1,3,7)])
  names(store)[2:3] =  c('lower','upper')
  store$var = rownames(store)
  store = mutate(store, 
                 n_table_rows = n_table_rows,
                 type = type)
  row.names(store) = NULL
  return(store)
}

# cumulative loop through trials in date order 
bayes_results = unif_results = NULL
for (c in 1:n_trials){
  this_data = filter(table_data, trial_num <= c) %>%
    mutate(row = paste(pmcid, '.', row, sep=''), # need to make separate row numbers to avoid overlap
           row = as.numeric(as.factor(row)), 
      pmcid = 1) # all one big trial

  # prepare the data for the Bayesian model
  for_model = make_stats_for_bayes_model(indata = this_data) # see 99_functions.R
  for_model_continuous_only = filter(for_model, statistic=='continuous')
  
  # run the model (just continuous)
  bugs1 = run_bugs(in_data = for_model_continuous_only, single_study=TRUE, debug=FALSE)
  res1 = neaten_results(in_res = bugs1, type = 'Continuous only', n_table_rows = nrow(for_model_continuous_only)) #
  # run the model (both continuous and number)
  bugs2 = run_bugs(in_data = for_model, single_study=TRUE, debug=FALSE)
  res2 = neaten_results(in_res = bugs2, type = 'Continuous and categorical', n_table_rows = nrow(for_model))
  
  # run the uniform test of p-values
  unif_pvals_continuous_only = group_by(for_model_continuous_only) %>%
    summarise(p = kolmogorov.unif.test(p, nrepl=2000, k=0)$p.value) %>%
    mutate(type = 'Continuous only',
           n_trials = c)
  unif_pvals = group_by(for_model) %>%
    summarise(p = kolmogorov.unif.test(p, nrepl=2000, k=0)$p.value) %>%
    mutate(type = 'Continuous and categorical',
           n_trials = c)
  
  # store the key results
  store = bind_rows(res1, res2) %>% 
          mutate(n_trials = c)
  bayes_results=  bind_rows(bayes_results, store)
  unif_results = bind_rows(unif_results, unif_pvals, unif_pvals_continuous_only)
}



## plot the results
# bayesian flag
colours = c('dark orange','darkseagreen')
cum_plot_flag = ggplot(data=filter(bayes_results, str_detect(var, 'flag')), aes(x=n_trials, y=mean, col=type))+
  geom_point(size=2)+
  geom_line(size=1.05)+
  scale_colour_manual(NULL, values=colours)+
  scale_x_continuous(breaks=1:10)+
  scale_y_continuous(limits=c(0,1))+
  ggtitle('Bayesian probability of\nunder- or over-dispersion')+
  xlab('Cumulative number of trials')+
  ylab('Probability')+
  g.theme+
  theme(legend.position='none',
        plot.title = element_text(size = 9))
cum_plot_flag
# uniform p-value
cum_plot_pval = ggplot(data=unif_results, aes(x=n_trials, y=p, col=type))+
  geom_point(size=2)+
  geom_line(size=1.05)+
  scale_colour_manual(NULL, values=colours)+
  scale_x_continuous(breaks=1:10)+
  scale_y_continuous(limits=c(0,1))+
  ggtitle('Uniform test of p-values')+
  xlab('Cumulative number of trials')+
  ylab('P-value')+
  g.theme+
  geom_hline(yintercept=0.05, lty=2)+
  theme(legend.position='none')
cum_plot_pval
# bayesian precision multiplier (with intervals)
labels = data.frame(x=0.5, y=c(1.3,0.7), lower=0, upper=0, label=c('Under-dispersion','Over-dispersion'))
cum_plot_mult = ggplot(data=filter(bayes_results, !str_detect(var, 'flag')), 
                       aes(x=n_trials, y=exp(mean), ymin=exp(lower), ymax=exp(upper), col=type))+
  geom_point(size=2, position = position_dodge(width=0.3))+
  geom_errorbar(size=1.05, width=0, position = position_dodge(width=0.3))+
  scale_colour_manual('Summary statistics used', values=colours)+
  scale_x_continuous(breaks=1:10)+
  scale_y_log10()+
  geom_line()+
  geom_hline(lty=2, yintercept=1)+
  geom_text(data = labels, aes(x=x, y=y, label=label), col='grey22', size=2.5, adj=c(0,1), angle=90)+
  ggtitle('Bayesian precision')+
  xlab('Cumulative number of trials')+
  ylab('Precision multiplier (log scale)')+
  g.theme
cum_plot_mult
# legend
legend = get_legend(cum_plot_mult)
cum_plot_mult = cum_plot_mult + theme(legend.position='none') # now remove legend

# export
jpeg('figures/saitoh.jpg', width=6, height=6, units='in', res=500)
grid.arrange(cum_plot_flag, cum_plot_pval, cum_plot_mult, legend, nrow=2, ncol=2)
dev.off()


# quick check of t-stats
tplot = ggplot(data= for_model, aes(x=row, y=t, col=statistic))+
  geom_point()+
  g.theme
tplot

# check evidence increase
check_plot = ggplot(data=filter(bayes_results, !str_detect(var, 'flag')), 
                       aes(x=n_trials, y=n_table_rows, col=type))+
  geom_point(size=2)+
  geom_line()+
  scale_colour_manual(NULL, values=colours)+
  scale_x_continuous(breaks=1:10)+
  xlab('Trial number')+
  ylab('Number of table rows')+
  g.theme

# output to table?
filter(bayes_results, str_detect(var, 'flag')) %>% select(n_trials, type, mean)
