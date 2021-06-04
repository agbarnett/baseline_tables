# 0_patterns.R
# patterns for detecting statistics in the tables; all in lower case
# June 2021

### a) continuous (mean and sd)
# common units in lower case
# removed 'min' for minutes because it gets confused with minimum, but added 'per min'
units = c('g/dl','ml','mmhg', 'mm hg','ng/ml', 'cm','kg/m2', 'kg/m<sup>2</sup>','mmol/l', 'per year', 'per month', 'per day', 'minutes', 'minute', 'per min','mins', 'year', 'years', 'yr', 'yrs', 'hours', 'hour', 'hr', 'hrs', 'score')
units = paste('\\b', paste(units, collapse='\\b|\\b'), '\\b', sep='') # must be whole words
# common stats and variables in lower case, removed 'total' because it get confused with total columns
stats = c('mean', 'sd', 's.d','score', 'index', 'age', 'bmi', 'height', 'weight', 'waist','Hba1c')
stats = paste('\\b', paste(stats, collapse='\\b|\\b'), '\\b', sep='') # must be whole words
# symbols/patterns for plus/minus; some symbols look the same but are different
plus_minus_utf = c(intToUtf8(0x00B1), intToUtf8(0x2213)) # add latex \pm? 
plus_minus = c('±','±','\\+[^:alnum:]–', '–[^:alnum:]\\+', '\\+[^:alnum:]-', '-[^:alnum:]\\+', 'plus[^:alnum:]minus', 'minus[^:alnum:]plus') # using any non alpha numeric character between plus and minus; minus signs are different
# combine
together = c(units, stats, plus_minus_utf, plus_minus)
together = together[order(-nchar(together))] # from longest to shortest
continuous_pattern = paste(together, collapse='|') # 

### b) median
# combinations of patterns that indicate median and IQR:
spacer = c('to','-','–','–') # centre spaces, hyphens are different!
median_numbers = c(paste('[0-9]',spacer,'[0-9]'), paste('[0-9]',spacer,'[0-9]', sep='')) # with and without spaces; also used in cells
#
words = c('median', 'iqr')
words = paste('\\b', paste(words, collapse='\\b|\\b'), '\\b', sep='') # words must be whole words
# combine
together = c(words, median_numbers)
together = together[order(-nchar(together))] # from longest to shortest
median_pattern = paste(together, collapse='|') # 
median_words = paste(words, collapse='|') # version without numbers

### c) percents
# common units in lower case
stats = c('%')
# common stats and variables in lower case
words = c('gender', 'sex', 'male','female','men','women','percent','n')
words = paste('\\b', paste(words, collapse='\\b|\\b'), '\\b', sep='') # words must be whole words
# combine
together = c(words, stats)
together = together[order(-nchar(together))] # from longest to shortest
percent_pattern = paste(together, collapse='|') # 

### d) numbers
number_breaks = c(':','/') # symbols that can break numbers
space_patterns_start = c('','',' ',' ')
space_patterns_end = c('',' ','',' ')
space_patterns = paste(space_patterns_start, rep(number_breaks, each =4), space_patterns_end, sep='') # each for four spacing types
number_pattern = paste('[0-9]', space_patterns, '[0-9]', sep='') # sandwiched by number, with varying space pattern
number_pattern = paste(number_pattern, collapse='|') # 

### e) p-values, some added based on experience
words = c('^p$','p.value','pvalue','p.level','p<','p <','p=','p =','statistical significance','main effect of group','no significant difference','nsd') # first is 'P' as a single value; any character for hyphen in `p-value`; nsd is acronym
words = words[order(-nchar(words))] # from longest to shortest
pval_pattern = paste(words, collapse='|') # 

### f) minimum / maximum
words = c('min', 'max', 'minimum','maximum', 'range', 'min.max', 'min..max')
words = words[order(-nchar(words))] # from longest to shortest
words = paste('\\b', paste(words, collapse='\\b|\\b'), '\\b', sep='') # words must be whole words
min_max_pattern = paste(words, collapse='|') # 

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
total_words = c('whole population','total','overall','all ','men','women','male','female') # need space after 'all'
total_words = total_words[order(-nchar(total_words))] # from longest to shortest
total_words = paste(paste('^', total_words, sep=''), collapse='|') # at start

### j) words that are used to describe a baseline table and words that rule it out
words_defined_baseline = c('baseline','characteristic','demographic','basic information','before starting the study','patients background','patient background')
words_or = paste(words_defined_baseline, collapse='|')
negative_words = c('network characteristic',
                   'change between baseline', 'changes between baseline',
                   'change from baseline', 'changes from baseline', 
                   'difference from baseline', 'differences from baseline',
                   'baseline adjusted', 'baseline adjustment') # words that rule out a baseline table
negative_words = paste(negative_words, collapse='|')

### k) sample size
sample_words = c('sample size','n=','n =',' n$','^number of |^no of |^no. of ')
sample_patterns = paste(sample_words, collapse = '|')
