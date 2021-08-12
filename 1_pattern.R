# 1_pattern.R
# patterns for detecting statistics in the tables; all in lower case
# patterns for excluding papers
# August 2021

### a) continuous (mean and sd)
# common units in lower case
# removed 'min' for minutes because it gets confused with minimum, but added 'per min'
units = c('mg/dl','mg / dl','g/dl','g / dl','iu/l','iu / l','ml','mmhg', 'mm hg','ng/ml','ng / ml', 'cm','kg/m2','kg / m2', 'kg/m<sup>2</sup>','mmol/l', 'per year', 'per month', 'per day', 'minutes', 'minute', 'per min','mins', 'year', 'years', 'yr', 'yrs', 'hours', 'hour', 'hr', 'hrs', 'score', 'scale', 'index','x103 cells/mm3', 'fev', 'fvc')
units = paste('\\b', paste(units, collapse='\\b|\\b'), '\\b', sep='') # must be whole words
# common stats and variables in lower case, removed 'total' because it get confused with total columns
stats = c('mean', 'sd', 's.d', 'standard dev','standard deviation','score', 'index', 'age', 'bmi', 'height', 'weight', 'waist','Hba1c')
stats = paste('\\b', paste(stats, collapse='\\b|\\b'), '\\b', sep='') # must be whole words
# symbols/patterns for plus/minus; some symbols look the same but are different
plus_minus_utf = c(intToUtf8(0x00B1), intToUtf8(0x2213)) # add latex \pm? 
plus_minus = c('±','±','\u00B1','\u2213','\\+[^:alnum:]–', '–[^:alnum:]\\+', '\\+[^:alnum:]-', '-[^:alnum:]\\+', 'plus[^:alnum:]minus', 'minus[^:alnum:]plus') # using any non alpha numeric character between plus and minus; minus signs are different
# combine 
together = c(units, stats, plus_minus_utf, plus_minus)
together = together[order(-nchar(together))] # from longest to shortest
continuous_pattern = paste(together, collapse='|') # 
## footnote
# words to search for in footnote
footnote_continuous = c('mean', 'sd', 's.d', 'standard dev','standard deviation')
footnote_continuous = paste('\\b', paste(footnote_continuous, collapse='\\b|\\b'), '\\b', sep='') # must be whole words
# specific phrases in footnotes that are given a high weighting
starts = c('values are', 'data are' , 'continuous variables are') # selection of starts
continuous_specific_words = paste(starts, '\\s+|\\w+', 'mean') # starts plus words/spaces then key word
continuous_specific = paste(continuous_specific_words, collapse='|') # version without numbers

### b) median
# combinations of patterns that indicate median and IQR:
spacer = c('to','-','–','–') # centre spaces, hyphens are different!
median_numbers = c(paste('[0-9]',spacer,'[0-9]'), paste('[0-9]',spacer,'[0-9]', sep='')) # with and without spaces; also used in cells
median_numbers = c(median_numbers, '!!!') # triple !!! is flag added by me
#
words = c('median', 'iqr') # triple !!! is flag added by me
words = paste('\\b', paste(words, collapse='\\b|\\b'), '\\b', sep='') # words must be whole words
# combine
together = c(words, median_numbers)
together = together[order(-nchar(together))] # from longest to shortest
median_pattern = paste(together, collapse='|') # 
median_words = paste(words, collapse='|') # version without numbers
## footnote
# words to search for in footnote
footnote_median = median_words
# specific phrases in footnotes that are given a high weighting
starts = c('values are', 'data are' , 'continuous variables are') # selection of starts
median_specific_words = paste(starts, '\\s+|\\w+', 'median') # starts plus words/spaces then key word
median_specific = paste(median_specific_words, collapse='|') # version without numbers


### c) percents
# common units in lower case
stats = c('^n %$','n %$','n % ','n \\(%\\)','n %\\.',' % ',' \\(%\\)',' %\\.','< [0-9]','> [0-9]','<[0-9]','>[0-9]','<=','>=','≤','≥','\u2264', '\u2265', '\u2273') # added symbols for >= etc as these often indicate thresholds
# common stats and variables in lower case
words = c('gender', 'sex', 'male','female','men','women','boy','boys','girl','girls','race','white','black','asian','other','yes','percent','percentage','years [0-9] [0-9]','years [0-9][0-9] [0-9][0-9]','[0-9] year') # years and two numbers for age in categories
words = paste('\\b', paste(words, collapse='\\b|\\b'), '\\b', sep='') # words must be whole words
# combine
together = c(words, stats)
together = together[order(-nchar(together))] # from longest to shortest
percent_pattern = paste(together, collapse='|') # 
## footnote
# words to search for in footnote
footnote_percent = c('percent','percentage','%')
footnote_percent = paste('\\b', paste(footnote_percent, collapse='\\b|\\b'), '\\b', sep='') # must be whole words
# specific phrases in footnotes that are given a high weighting
starts = c('values are', 'data are' , 'discrete variables are') # selection of starts
percent_specific_words1 = paste(starts, '\\s+|\\w+', 'count') # starts plus words/spaces then key word
percent_specific_words2 = paste(starts, '\\s+|\\w+', 'number') # starts plus words/spaces then key word
percent_specific = paste(c(percent_specific_words1, percent_specific_words2), collapse='|') # version without numbers

### d) numbers
number_breaks = c(':','/') # symbols that can break numbers
space_patterns_start = c('','',' ',' ')
space_patterns_end = c('',' ','',' ')
number_words = c('girls/boys','boys/girls','m/f','f/m','male:female','female:male','left:right','right:left') # common words
space_patterns = paste(space_patterns_start, rep(number_breaks, each = 4), space_patterns_end, sep='') # each for four spacing types
number_pattern = paste('[0-9]', space_patterns, '[0-9]', sep='') # sandwiched by number, with varying space pattern
number_pattern = paste(c(number_words,number_pattern), collapse='|') # 

### e) p-values, some added based on experience
words = c('^p across','^p$','\\b\\(p\\)\\b','p..value','p.value','pvalue','^p.level','\\bp<','\\bp.<','\\bp=','\\bp.=','p.>.0.05','p>0.05','statistical significan','statistically significan','main effect of group','non.significan','no significant difference','significantly higher','significantly lower','\\bnsd\\b') # first is 'P' as a single value; any character for hyphen in `p-value`; nsd is acronym
words = words[order(-nchar(words))] # from longest to shortest
pval_pattern = paste(words, collapse='|') # 

### f) minimum / maximum
words = c('max', 'minimum','maximum', 'range', 'min.max', 'min..max') # removed min as it gets confused with minutes
words = words[order(-nchar(words))] # from longest to shortest
bwords = paste('\\b', paste(words, collapse='\\b|\\b'), '\\b', sep='') # words must be whole words
min_max_pattern = paste(bwords, collapse='|') # 
# alternative version where match must be whole text, for column headings
words = paste('^', paste(words, collapse='$|^'), '$', sep='') # words must be whole words
min_max_pattern_whole = paste(words, collapse='|') # 
## footnote
# words to search for in footnote
footnote_min_max = min_max_pattern_whole

### g) dates , used for converting dates to numbers
date_separators = c('/','-')
year = c('[0-9][0-9][0-9][0-9]','[0-9][0-9]') # 2 or 4 year
month = c('01','02','03','04','05','06','07','08','09',as.character(1:12)) #
days = c('01','02','03','04','05','06','07','08','09',as.character(1:31)) #
dates_patterns = NULL
for (sep in date_separators){
  for (y in year){
    for (m in month){
      for (d in days){
        # year first
        this_pattern = paste(y, sep, m, sep, d, sep='')
        dates_patterns = c(dates_patterns, this_pattern)
        # year laste
        this_pattern = paste(d, sep, m, sep, y, sep='')
        dates_patterns = c(dates_patterns, this_pattern)
      }
    }
  }
}
dates_patterns = dates_patterns[order(-nchar(dates_patterns))] # from longest to shortest
dates_patterns = paste(dates_patterns, collapse = '|')
# to convert statistics: no longer used
#words = c('date','dates')
#words = paste('\\b', paste(words, collapse='\\b|\\b'), '\\b', sep='') # words must be whole words
#date_pattern = paste(words, collapse='|') # 

### h) RCT
# knock off first letter because of capitalistion; what to keep in capitals because of RCT
rct_patterns = c('andomized trial','andomised trial','andomized controlled trial','andomised controlled trial','RCT','luster randomised trial')
rct_patterns = rct_patterns[order(-nchar(rct_patterns))] # from longest to shortest
rct_patterns = paste(rct_patterns, collapse = '|')

### i) totals and other columns to exclude
total_words = c('all$','whole population','randomi.ed sample','randomi.ed population','sum total','total','overall','all ','men','women','male','female') # need space after 'all'
total_words = total_words[order(-nchar(total_words))] # from longest to shortest
total_words_alt = paste('^[a-z]', paste(total_words, sep=''), sep='', collapse='|') # at start - needed to detect column headings that I've changed to avoid duplicate names
total_words = paste(paste('^', total_words, sep=''), collapse='|') # at start

### j) words/phrases that are used to describe a baseline table and words that rule it out; some from trial and error
words_defined_baseline = c('baseline','characteristic','demographic','basic information','before starting the study','patients background','patient background','at enrolment','at enrollment')
words_defined_baseline = words_defined_baseline[order(-nchar(words_defined_baseline))] # from longest to shortest
words_or = paste(words_defined_baseline, collapse='|')
negative_words = c('nutritional content', # top few from trial and error
                   'criteria for assigning',
                   'covariate analysis',
                   'trial information',
                   'drug information',
                   'qualitative study',
                   'at baseline and after',
                   'randomi.ed cross-over design',
                   'differences with treatment', # PMC6009513
                   'excluded', # may not always work, but avoids including tables that compare included and excluded participants, PMC5765660
                   'declined', # PMC7045045
                   'predicted values',
                   'who did and did not attend', # PMC6636186
                   'open.label.extension', # PMC8252065
                   'network characteristic',
                   'intervention characteristics',
                   'change between baseline', 'changes between baseline',
                   'change from baseline', 'changes from baseline', 
                   'difference from baseline', 'differences from baseline',
                   'baseline adjusted', 'baseline adjustment') # words that rule out a baseline table
negative_words = paste(negative_words, collapse='|')

### k) sample size
sample_words = c('sample size','n:','n :','n=','n =','^n$',' n$','^number$','^number of |^no of |^no. of |^total number of participants|^total number of patients|^total n$|^total number$')
sample_words = sample_words[order(-nchar(sample_words))] # from longest to shortest
sample_patterns = paste(sample_words, collapse = '|')

### l) standard error of the mean
sem_words = c('se','ses','sem','sems','s.e.','s.e.m.','standard error')
sem_words = sem_words[order(-nchar(sem_words))] # from longest to shortest
sem_patterns = paste('\\b', paste(sem_words, collapse='\\b|\\b'), '\\b', sep='') # must be whole words

### m) words for follow-up and other exclusions, eg. propensity‐score
fu_words = c('follow.up','post.trial','followup','posttrial','t1','change following intervention','percent change','% change','and after administration','propensity.score') # T1 from 8003116
fu_words = fu_words[order(-nchar(fu_words))] # from longest to shortest
fu_pattern = paste(fu_words, collapse='|')

### n) test statistic column labels
test_words = c('test.statistic','f.test','ftest','f.value','t.test','ttest','t.value','z.test','ztest','z.value','chi.squared','chisquared','statistics','\\bdf\\b')
test_words = test_words[order(-nchar(test_words))] # from longest to shortest
test_pattern = paste(test_words, collapse='|')

### o) phrases that are used in paper titles that are not an RCT; drop-out for studies of drop-out like PMC5340923
non_rct_words = paste( c('non.randomi.ed','practice.related.variation','combined.post.hoc','machine.learning','deep.learning','effectiveness','effect.of.baseline','regional.differences','secondary.analysis','pooled.analysis','pooled.efficacy','exploratory.study','exploratory.analysis','cross.sectional.study','cross.sectional.analysis','prognostic.model','dropout','drop.out','updated follow.up','subgroup','sub.group'), collapse='|')
non_rct_words = non_rct_words[order(-nchar(non_rct_words))] # from longest to shortest
non_rct_pattern = paste(non_rct_words, collapse='|')

### p) words that are used in pre-post tables
prepost_words = c('pre','post') # whole words
prepost_pattern = paste('\\b', paste(prepost_words, collapse='\\b|\\b'), '\\b', sep='') # must be whole words
