# 6_tree_regression_flag.R
# use classification tree and elastic net regression to see if study design can explain precision flag
# January 2022
library(dplyr) 
library(rpart)
library(rpart.plot) 
library(glmnet)
library(broom)
library(stringr)

## get the data; select which source of data to use
sources = c('my_search', 'trialstreamer', 'validation', 'simulation')
source = sources[2]
stage = 'tree'
source('1_which_data_source.R') # uses `source` and `stage`

## get table data to summarise statistic
stage = 'model'
source('1_which_data_source.R') # uses `source` and `stage`
table_summaries = filter(table_data, statistic %in% c('percent','ci','numbers','continuous')) %>% # only stats used in analysis
  group_by(pmcid) %>%
  summarise(d_size = max(sample_size) - min(sample_size), # difference
            s_size = log2(median(sample_size)), # log-transformed (base 2) because of skew
            p_cont = sum(statistic=='continuous')/ n(), # proportion continuous
            n_cols = length(unique(column)),
            n_rows = length(unique(row))) %>%
  mutate(d_size_cat = case_when( # had to categorise due to massive skew
    d_size == 0 ~ 'Equal groups',
    d_size > 0 & d_size < 10 ~ 'Small difference',
    d_size >= 10 ~ 'Large difference'
  ))
# decimal places for continuous
decimal_places = filter(table_data, statistic %in% c('ci','continuous')) %>% # only stats used in analysis
  group_by(pmcid) %>%
  summarise(dp = mean(dp1)) # for the mean

## replace countries and journals with small numbers with "other"
# a) countries
countries = group_by(design, affiliation) %>%
  tally() %>%
  ungroup() %>%
  mutate(
    country = ifelse(n < 20, 'Other', affiliation),
    country = ifelse(is.na(country), 'Other', country))
design = left_join(design, countries, by='affiliation')
# b) journals
journals = group_by(design, journal) %>%
  tally() %>%
  ungroup() %>%
  mutate(Journal = ifelse(n < 10, 'Other', journal)) # capital "J"
design = left_join(design, journals, by='journal')

# merge the design and flag data
threshold = 0.8 # probability beyond which we create a flag
flag_stats = filter(all_stats, nicevar=='Precision flag') %>%
  left_join(design, by='pmcid') %>%
  mutate(outcome = mean >= threshold)
# merge the stats about the table
flag_stats = left_join(flag_stats, table_summaries, by='pmcid') %>%
  left_join(decimal_places, by='pmcid') %>%
  mutate(dp = ifelse(is.na(dp), 1, dp)) # replace missing decimal places with median; 
# remove extreme means (likely issues with the algorithm)
to_exclude = filter(all_stats, nicevar=='Precision') %>%
  filter(mean > 40) %>%
  pull(pmcid)
flag_stats = filter(flag_stats, !pmcid %in% to_exclude)

### part 1: tree - not that useful as loss of power compared with linear model ###

# change control
my.control = rpart.control()
my.control$maxdepth = 4
my.control$xval = 20
TeachingDemos::char2seed('crawley')

# tree
model = rpart(outcome ~ country + Journal + s_size + pilot + cluster + sem + block_randomisation + n_rows + n_cols + p_cont + dp, data = flag_stats, method = 'class', control = my.control)
summary(model)
rpart.plot(model, type=4, extra=103) # display n
rpart::plotcp(model)

### part 2: elastic net for variable selection ###
# too few results for variable `adaptive_randomisation`

# model the precision probability
TeachingDemos::char2seed('mansfield')
formula = 'mean ~ -1 + country + Journal + s_size + d_size_cat + pilot + cluster + sem + block_randomisation + n_rows + n_cols + p_cont + dp'
x = model.matrix(as.formula(formula), data=flag_stats)
y = flag_stats$mean # do not transform, no clear benefit
gfit = glmnet(x, y, alpha=0.95, standardize=TRUE) # elastic net
plot(gfit)
cvfit = cv.glmnet(x, y, alpha=0.95, standardize=TRUE)
plot(cvfit)
coef = coef(cvfit, s = "lambda.1se")

# refit model with selected variables; using elastic net as variable selection
index = attr(coef, 'i') + 1
included_vars = attr(coef, 'Dimnames')[[1]][index]
#
index_no_intercept = index[index>1] - 1 # remove intercept from index
newx = x[, index_no_intercept]
newx = cbind(rep(1,nrow(newx)), newx)# add intercept
colnames(newx) = str_remove(included_vars, pattern="TRUE") # tidy up names
model = lm(y ~ -1 + newx)
summary(model)
hist(resid(model))
ests = tidy(model,conf.int = TRUE) %>%
  mutate(term = str_remove(term, pattern='newx'))

# save for paper and appendix
save(ests, cvfit, gfit, file='results/lasso_results.RData')


## postscript, look at correlation in predictors, in response to reviewers
library(plotly)
library(GGally)
selected = c('s_size','d_size_cat','sem','n_cols','p_cont')
to_plot = select(flag_stats, all_of(selected)) %>%
  filter(!is.na(d_size_cat)) # remove one missing
p <- ggpairs(to_plot) 
p = p + theme_bw() +
  ggtitle('')
# export figure
jpeg('figures/predictors_matrix.jpg', quality=100, width=6, height=6, units='in', res=400)
print(p)
dev.off()

