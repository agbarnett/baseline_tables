# 0_patterns.R
# patterns for searching for variables; all in lower case
# April 2021

### a) continuous (mean and sd)

# common units in lower case
units = c('mmhg', 'ng/ml', 'kg/m2', 'mmol/l', 'per year', 'per month', 'per day')
units = paste('\\b', paste(units, collapse='\\b|\\b'), '\\b', sep='') # must be whole words
# common stats and variables in lower case
stats = c('mean', 'sd', 'age', 'years', 'yr', 'yrs', 'bmi')
stats = paste('\\b', paste(stats, collapse='\\b|\\b'), '\\b', sep='') # must be whole words
# symbols/patterns for plus/minus
plus_minus = c(intToUtf8(0x00B1), '+[:punct:]-', '-[:punct:]+', 'plus[:punct:]minus', 'minus[:punct:]plus')# add latex \pm? ,'∓' not working; minus signs are different
# combine
together = c(units, stats, plus_minus)
together = together[order(-nchar(together))] # from longest to shortest
continuous_pattern = paste(together, collapse='|') # 

### b) median
# combinations of patterns that indicate median and IQR:
spacer = c('to','-','–') # centre spaces, hyphens are different!
median_numbers = c(paste('[0-9]',spacer,'[0-9]'), paste('[0-9]',spacer,'[0-9]', sep='')) # with and without spaces; also used in cells
#
words = c('median', 'iqr')
words = paste('\\b', paste(words, collapse='\\b|\\b'), '\\b', sep='') # words must be whole words
# combine
together = c(words, median_numbers)
together = together[order(-nchar(together))] # from longest to shortest
median_pattern = paste(together, collapse='|') # 

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
numbers_pattern = c('/',':') # used to separate numbers
numbers_pattern = paste(numbers_pattern, collapse='|') # 

### e) p-values
words = c('^p$','p.value','pvalue') # 'P' as a single value, any character for hyphen in `p-value`
words = words[order(-nchar(words))] # from longest to shortest
pval_pattern = paste(words, collapse='|') # 
