# 0_patterns.R
# patterns for detecting statistics in the tables; all in lower case
# April 2021

### a) continuous (mean and sd)

# common units in lower case
units = c('mmhg', 'ng/ml', 'kg/m2', 'mmol/l', 'per year', 'per month', 'per day', 'minutes', 'minute', 'min', 'mins', 'year', 'years', 'yr', 'yrs', 'hours', 'hour', 'hr', 'hrs')
units = paste('\\b', paste(units, collapse='\\b|\\b'), '\\b', sep='') # must be whole words
# common stats and variables in lower case, removed 'total' because it get confused with total columns
stats = c('mean', 'sd', 'score', 'index', 'age', 'bmi', 'height', 'weight', 'waist', 'hb1ac')
stats = paste('\\b', paste(stats, collapse='\\b|\\b'), '\\b', sep='') # must be whole words
# symbols/patterns for plus/minus
plus_minus_utf = c(intToUtf8(0x00B1), intToUtf8(0x2213)) # add latex \pm? 
plus_minus = c('\\+[^:alnum:]–', '–[^:alnum:]\\+', '\\+[^:alnum:]-', '-[^:alnum:]\\+', 'plus[^:alnum:]minus', 'minus[^:alnum:]plus') # using any non alpha numeric character between plus and minus; minus signs are different
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
words = c('gender', 'sex', 'male','female','percent')
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

### e) p-values
words = c('^p$','p.value','pvalue','p=','p =') # first is 'P' as a single value; any character for hyphen in `p-value`
words = words[order(-nchar(words))] # from longest to shortest
pval_pattern = paste(words, collapse='|') # 

### f) dates - not yet used
words = c('date','dates')
words = paste('\\b', paste(words, collapse='\\b|\\b'), '\\b', sep='') # words must be whole words
date_pattern = paste(words, collapse='|') # 
