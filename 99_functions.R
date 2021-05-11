# 99_functions.R
# functions for p-value testing
# April 2021

## extract baseline tables
baseline_table = function(webpage, table_number = 1){
  
  # binary variable tested later; were there p-values in table?
  pvalues_in_table = FALSE
  
  # maximum number of tables
  max_table = length(webpage %>% xml_nodes("table"))
  if(table_number > max_table){ # table likely in appendix or graphic
    to.return = list()
    to.return$reason = 'Table in appendix or table is graphic'
    to.return$table = NULL
    return(to.return) # stop here
  }
  
  # extract baseline table 
  table1 <- webpage %>%
    xml_nodes("table") %>%
    .[table_number] %>% # table number for baseline table
    html_table(header = TRUE, fill = TRUE) # include table headers
  table1 = table1[[1]] # to data frame
  
  ## remove odd characters and change to lower case
  cnames = names(table1)
  if(any(cnames == '')){ # cant have null table numbers
    index = names(table1) == ''
    names(table1)[index] = paste('c',1:sum(index),sep = '')
  }
  # any duplicate table names? 
  cnames = names(table1)
  if(any(duplicated(cnames)) == TRUE){
    # if multiple columns are 'variable' then likely to be a terrible layout
    check = sum(tolower(cnames) == 'variable')
    if(check > 0){
      to.return = list()
      to.return$reason = 'Difficult layout'
      to.return$table = NULL
      return(to.return) # bail out here, will be impossible to unpick
    }
    #
    names(table1) =  paste(LETTERS[1:ncol(table1)], names(table1), sep = '') # avoid repeat names by adding letters to start
  }
  # clean up table text and column headers:
  colnames(table1) = tolower(removeunicode(colnames(table1))) # remove unicode from headers too
  table1 = mutate_all(table1, tolower) %>%
    mutate_all(add_space_function) %>% # add space before and after brackets to stop numbers getting compressed
    mutate_all(removeunicode) %>%
    mutate_all(string_remove_function)

  # could transpose badly formatted tables here (where results are in rows instead of columns)
  
  # combine first two columns if they are both mostly text (double label column)
  textc = sum(str_count(tolower(table1[,2]), pattern='[a-z]'), na.rm=TRUE) # amount of text in 2nd column
  numbersc = sum(str_count(tolower(table1[,2]), pattern='[0-9]'), na.rm=TRUE) # amount of numbers in 2nd column
  if( (textc/(numbersc+textc)) > 0.8 ){ # if over 80% text then must be a label column
    names(table1)[1:2] = c('label1','label2')
    table1 = mutate(table1, 
                    label = ifelse(label1 != label2, paste(label1, label2), label1)) %>% # if repeat then just use first label
      select(-label1, -label2) %>%
      select(label, everything()) # move to first column
  }
  
  # stop if just one column
  if(ncol(table1) == 2){
    to.return = list()
    to.return$reason = 'Just one column in table'
    to.return$table = NULL
    return(to.return) # stop here
  }
  
  ## remove repeated labels across rows
  ncol = ncol(table1)
  repeats = mutate(table1, row = 1:n()) %>%
    reshape::melt(id.vars = 'row') %>%
    group_by(row) %>%
    select(-variable) %>%
    unique() %>%
    tally() %>%
    filter(n ==1 ) %>% # all rows the same
    pull(row)
  if(length(repeats) > 0){
    table1[repeats, 2:ncol] = '' # blank repeat cells
  }

  ## what row do numbers/stats start on?
  cells = table1[,-1] # don't use first column with labels
  long = mutate(cells, row = 1:n())
  numbers_start = reshape::melt(id.vars = 'row', long) %>%
    mutate(value=tolower(value),
           text = str_count(value, '[a-z]'), 
           n = str_detect(value, 'n=|n =|^n$'), # add `n=` to header, third is just "N"
           numbers = str_count(value, '[0-9]')) %>% 
    group_by(row) %>%
    summarise(
      text = sum(text),
      n = sum(n),
      numbers =  sum(numbers)) %>% # amount of text and numbers
    ungroup() %>%
    filter(n < 2) %>% # or with two `n=`
    filter(numbers > text) %>% # first row with more numbers than text
    slice(1) %>%
    pull(row)
  ## Additional check for labels that are "N"
  if(any(table1[,1]=='n')){
    index = which(table1[,1]=='n')
    numbers_start = index + 1
    table1[index,] = paste('n=', table1[index,], sep='') # add `n=` for later detection
  }
  # now add header
  table1 = mutate(table1, header = FALSE)
  if(length(numbers_start) > 0){ table1$header[1:nrow(table1) < numbers_start] = TRUE }
  # create versions without header and only header
  no_header = filter(table1, header == FALSE) %>% # remove header row(s)
    select(-header) # remove header column
  table_header = filter(table1, header == TRUE) %>%
    select(-header)
  
  ## detect use of statistics for each row
  stats_detect = statistics_detect(intable=no_header, header = table_header)
  # remove columns and record p-values in table ...
  if(!is.null(stats_detect$columns_to_remove)){
    no_header = no_header[, -stats_detect$columns_to_remove]
    table_header = table_header[, -stats_detect$columns_to_remove]
    pvalues_in_table = TRUE
  }
  stats_detect = stats_detect$table # ... then change list to table of cells
  # update `p-values in table` if p-values are in rows
  if(any(stats_detect$statistic == 'pvals', na.rm = TRUE)){pvalues_in_table = TRUE}

  ## remove rows in the table that are just text (so just keep rows with some numbers)
  nums = matrix(data = rep(1:nrow(no_header), ncol(no_header)), ncol = ncol(no_header), nrow = nrow(no_header)) # make matrix of numbers
  vector_nums = as.vector(as.matrix(nums))
  table_text = as.vector(as.matrix(no_header))
  table_text[is.na(table_text)] = ''
  any_numbers = str_detect(table_text, pattern = '[0-9]')
  rows_with_numbers = unique(vector_nums[any_numbers])
  rows_with_numbers = rows_with_numbers[order(rows_with_numbers)] # preserve ordering
  no_header = no_header[rows_with_numbers,] # just keep rows with numbers
  stats_detect = stats_detect[rows_with_numbers,] %>% # apply rows to stats detect too
    mutate(row = 1:n()) # and renumber rows
  
  # convert table to vector
  no_header = no_header[, -1] # remove first column which is normally always not statistics
  row_nums = matrix(data = rep(1:nrow(no_header), ncol(no_header)), ncol = ncol(no_header), nrow = nrow(no_header)) # make matrix of numbers
  col_nums = matrix(data = rep(1:ncol(no_header), nrow(no_header)), byrow = TRUE, ncol = ncol(no_header), nrow = nrow(no_header)) # make matrix of numbers
  row_nums = as.vector(as.matrix(row_nums))
  col_nums = as.vector(as.matrix(col_nums))
  table_text = as.vector(as.matrix(no_header))
  table = suppressMessages(bind_cols(row_nums, col_nums, table_text))
  names(table) = c('row','column','text')
  table = full_join(table, stats_detect, by = 'row') # add stats detected variables 
  table = mutate(table, 
                 text = str_replace_all(string = text, pattern = '·', replace = '.'), # remove odd decimal place used by Lancet
                 text = str_replace_all(text, pattern = '[^0-9|.| ]', replacement = ' '), # just keep numbers, decimals and spaces
                 text = str_squish(string = text)) %>% # remove any double spaces added, and spaces at start/end
    filter(!is.na(statistic)) %>% # only proceed if we know the statistic
    separate(text, c('stat1','stat2','stat3','stat4'), sep = ' ', fill = 'right') %>% # extract four statistics
    mutate(stat1 = suppressWarnings(as.numeric(stat1)), # sum numbers not converted because of things like ".16"
           stat2 = suppressWarnings(as.numeric(stat2)),
           stat3 = suppressWarnings(as.numeric(stat3)),
           stat4 = suppressWarnings(as.numeric(stat4))) %>%
    filter(!is.na(stat1)) # knock out missing cells
  
  ## get sample size from top row(s) or cells, then add to table
  sample_sizes = sample_sizes_est(table_header = table_header[,-1], processed_table = table, first_row = table1[1,]) # header without first column 
  if(is.null(sample_sizes)==TRUE){
    to.return = list()
    to.return$reason = 'No sample size'
    to.return$table = NULL
    return(to.return) # stop here
  }
  if(nrow(sample_sizes)==1){
    to.return = list()
    to.return$reason = 'Just one column in table'
    to.return$table = NULL
    return(to.return) # stop here
  }
  
  # add sample sizes to table
  table = full_join(table, sample_sizes, by = 'column') %>% # add sample sizes from above
    filter(!is.na(sample_size)) # knock out missing sample size
  
  # fill in numerator if only percent given? e.g, pmcid
  to_fill = filter(table, is.na(stat2), statistic == 'percent')
  if(nrow(to_fill)>0){
    okay = filter(table, !is.na(stat2) | statistic != 'percent') # rows that are okay
    to_fill = mutate(to_fill,
                     stat2 = stat1, # move percent to second cell
                     stat1 = round((stat2/100)*sample_size)) # estimate numerator 
    table = bind_rows(okay, to_fill)
  }
  
  ## drop any columns that are the total (to do: could expand to cells in header)
  total_words = c('total','overall','all ')
  total_words = total_words[order(-nchar(total_words))] # from longest to shortest
  total_words = paste(paste('^', total_words, sep=''), collapse='|') # at start
  text_to_check = tolower(names(table_header))
  text_to_check = str_squish(str_remove_all(text_to_check, 'n=|n =|[0-9]')) # remove sample size
  to_drop = str_detect(text_to_check, total_words) # looking for just the word 'total', 'all' or 'overall', plus one character because of numbers added above
  if(any(to_drop) == TRUE){
    totals = which(to_drop) - 1 # minus one because of first column with names
    table = filter(table, !column %in% totals)
  }
  
  ## If single integer in first cell then assume it is a count
  table = mutate(table,
              statistic = ifelse(is.na(stat2) & (round(stat1) - stat1)==0, 'percent', statistic), # change type
              stat2 = ifelse(is.na(stat2) & (round(stat1) - stat1)==0, 100*(stat1/sample_size), stat2)) # calculate percent
  
  # arrange table
  table = mutate(table, 
                 row = as.numeric(as.factor(row)),
                 column = as.numeric(as.factor(column)))
  table = arrange(table, row, column)
  
  # check for repeat columns
  duplicates = select(table, -column) %>%
    duplicated()
  if(sum(duplicates) > 3){ # number of duplicate rows, may need trial and error with this threshold
    dcolumns = unique(table[duplicates,] %>% pull(column))
    table = filter(table, !column %in% dcolumns) %>%
      mutate(column = as.numeric(as.factor(column))) # re-number columns
  }
 
  # return final results
  to.return = list()
  to.return$reason = NULL # no reason to exclude
  to.return$table = table
  to.return$pvalues_in_table = pvalues_in_table
  return(to.return)
  
}

## function to detect what statistics are used in each row of the table
# assume 1st column contains labels
statistics_detect = function(intable, 
                             header,
                             weight = 2) # mentions in column label count for more
{
  
  columns_to_remove = NULL # store columns to remove
  
 ## a) look for stats in header
 #header = header[,-1] # do not use first column (usually labels) - turned off again because header can have useful info
 header = rbind(header, names(header)) # add header as text and remove names
 names(header) = NULL
 header = t(header) # transpose
 header = data.frame(cbind(1:nrow(header), header))
 header = reshape::melt(id.vars = 'X1', header)
 stats_header = mutate(header,
                       X1 = as.numeric(X1),
  percent = str_detect(value, pattern = percent_pattern), # see 0_pattern.R for all patterns
  continuous = grepl(x = value, pattern = continuous_pattern),
  numbers = str_detect(value, pattern = number_pattern),
  min_max = str_detect(value, pattern = min_max_pattern),
  pval = str_detect(value, pattern = pval_pattern),
  median = str_detect(value, pattern = median_pattern)) %>% 
  group_by(X1) %>%
  summarise(col_percent = sum(percent, na.rm = TRUE),
       col_continuous = sum(continuous, na.rm = TRUE),
       col_numbers = sum(numbers, na.rm = TRUE),
       col_min_max = sum(min_max, na.rm = TRUE),
       col_pvals = sum(pval, na.rm = TRUE),
       col_median = sum(median, na.rm = TRUE)) %>%
   rename('column' = 'X1')
 # convert column to row scores (apply column scores to every row)
 stats_header_row = uncount(stats_header, weights=nrow(intable)) %>%
   group_by(column) %>%
   mutate(row = 1:n()) %>%
   ungroup() %>%
   mutate(col_percent = col_percent/(ncol(stats_header) - 1), # spread score over rows for later summing
          col_continuous = col_continuous/(ncol(stats_header) - 1),
          col_numbers = col_numbers/(ncol(stats_header) - 1),
          col_min_max = col_min_max/(ncol(stats_header) - 1),
          col_pvals = col_pvals/(ncol(stats_header) - 1),
          col_median = col_median/(ncol(stats_header) - 1))
 
 ## remove columns that are p-values
 pval_cols = as.numeric(filter(stats_header, col_pvals>0) %>% pull(column))
 if(length(pval_cols)>0){
   intable = intable[, -(pval_cols+1)] # remove columns
   columns_to_remove = c(columns_to_remove, pval_cols+1) # add to information to return (plus one because of label columns)
 }
 stats_header_row = filter(stats_header_row, !column %in% pval_cols)

 ## look for columns of p-values based on cells
 # make table cells
 table_cells = make_cells(intable[-1]) # drop label column
 # now look for lots of p-values by column
 column_pvals = group_by(table_cells, column) %>%
   summarise(pval_fraction = sum(pvals) / n()) %>%
   ungroup() %>%
   filter(pval_fraction > 0.5) %>% # remove if over half are p-values
   pull(column) 
 if(length(column_pvals)>0){
   intable = intable[, -(column_pvals+1)] # remove columns, plus one because of label column
   table_cells = make_cells(intable[-1]) # need to redo cells (drop label column)
   columns_to_remove = c(columns_to_remove, column_pvals+1) # add to information to return
 }
 stats_header_row = filter(stats_header_row, !column %in% column_pvals)
 
 ## b) look for stats in first column of non-header (label column)
 percents = str_detect(intable[,1], pattern = percent_pattern) # see 0_pattern.R for all patterns
continuous = grepl(pattern = continuous_pattern, intable[,1]) # had to use grep because of symbols
numbers = str_detect(intable[,1], pattern = number_pattern)
min_max = str_detect(intable[,1], pattern = min_max_pattern)
pvals = str_detect(intable[,1], pattern = pval_pattern)
medians = str_detect(intable[,1], pattern = median_words) # just look for median words, not number patterns
stats_label_column = data.frame(row = 1:nrow(intable), 
             percents = as.numeric(percents) , # using weights, but not for percent because of things like Left ventricular ejection fraction and HbA1c
             continuous = continuous * weight,
             numbers = numbers * weight,
             min_max = min_max * weight,
             pvals = pvals * weight,
             medians = medians * weight*1.1) # tiny increase in median to avoid ties

## c) melt to long to look at stats units in cells
stats_cells = full_join(table_cells, stats_header_row, by=c('row','column')) %>% # add header estimates, using `table_cells` from above
 group_by(row) %>%
 summarise(cell_percents = sum(percents, na.rm = TRUE) + sum(col_percent), # total counts per row
      cell_continuous = sum(continuous, na.rm = TRUE) + sum(col_continuous),
      cell_numbers = sum(numbers, na.rm = TRUE) + sum(col_numbers),
      cell_min_max = sum(min_max, na.rm = TRUE) + sum(col_min_max),
      cell_pvals = sum(pvals, na.rm = TRUE) + sum(col_pvals),
      cell_median = sum(median, na.rm = TRUE) + sum(col_median)) 

# now combine cells and column labels and make best guess about statistics per row    
combine = full_join(stats_cells, stats_label_column, by = 'row') %>%
 mutate(percent_score = as.numeric(cell_percents + percents), # scores that combine cell and column information
     continuous_score = as.numeric(cell_continuous + continuous),
     numbers_score = as.numeric(cell_numbers + numbers),
     min_max_score = as.numeric(cell_min_max + min_max),
     pvals_score = as.numeric(cell_pvals + pvals),
     median_score = as.numeric(cell_median + medians)) %>%
 select(row, ends_with('score')) %>%
 pivot_longer(cols = ends_with('score'), names_to = 'statistic', values_to = 'score') %>%
 group_by(row) %>%
 arrange(-score) %>% # take top scoring type per row
 slice(1) %>%
 mutate(statistic = str_remove_all(statistic, pattern = '_score'),
     statistic = ifelse(score == 0, NA, statistic)) %>% # swap those with zero score to missing
 ungroup()
# carry forward stats from previous row if there's no other type
carry = tidyr::fill(combine, statistic, .direction = 'down') %>%
 select(-score) 

## find percents that are actually continuous  --- to finish
extra = function(){
long = reshape::melt(id.vars = 'row', cells) %>%
  mutate(value = str_replace_all(string = value, pattern = '·', replace = '.'), # remove odd decimal place used by Lancet
         value = str_replace_all(value, pattern = '[^0-9|.| ]', replacement = ' '), # just keep numbers, decimals and spaces
         value = str_squish(string = value)) %>% # remove any double spaces added, and spaces at start/end
 separate(value, c('stat1','stat2','stat3','stat4'), sep = ' |/', fill = 'right') %>%
 mutate(
   stat1 = suppressWarnings(as.numeric(stat1)),
   stat2 = suppressWarnings(as.numeric(stat2)),
     int1 = round(stat1) - stat1 == 0,
     int2 = round(stat2) - stat2 == 0) %>% # check for n (%) format
 filter(!is.na(stat1),
        !is.na(stat2)) %>%
 group_by(row) %>%
 summarise(n = n(), integers1 = sum(int1), integers2 = sum(int2)) %>%
 filter(n > 0,
        integers1 == n) # all first statistics are integers
#carry = left_join(long, )
}

# return
to_return = list()
to_return$columns_to_remove = columns_to_remove
to_return$table = carry
return(to_return)
}

## text cleaning functions
# set up as a functions because used in mutate_all below
string_remove_function = function(x){str_remove_all(string = x, pattern = '[^a-z|0-9| |.|·|%|–|-|-|±|/|:| = ]')} # need function for mutate_all, note multiple difference hyphens
add_space_function = function(x){str_replace_all(string = x, pattern = '\\(|\\)', replacement = ' \\(')} # use opening bracket for both replacements
removeunicode = function(x){gsub(pattern = "[^[:print:]]", " ", x)} # see https://www.petefreitag.com/cheatsheets/regex/character-classes/

## Mode (used by sample size)
Mode <- function(x) {
 ux <- unique(x)
 ux[which.max(tabulate(match(x, ux)))]
}

## function to extract sample size from column headers
extract_n = function(intext){
 intext[is.na(intext)] = '' # replace NAs
 intext = tolower(intext) # convert to lower text
 intext = str_remove_all(string = intext, pattern = '[^a-z|0-9| = | ]') # remove lots of characters
 ns = str_split_fixed(string = intext, pattern = 'n=|n =', n = 2)[,2] # split on `n = `
 ns = as.numeric(str_remove(string = ns, pattern = '\\)')) # extract number
 # alternative where cells are just numbers
 if(any(!is.na(ns)) == FALSE){ # if all missing
  if(str_detect(intext[1], pattern = ' n$')){ # if column heading ends in `n`
   ns = suppressWarnings(as.numeric(intext)) # turn off warning
  }
 }
 # add column number
 f = data.frame(column = 1:length(intext), sample_size = ns) %>%
  filter(!is.na(sample_size))
 return(f)
}

## function to estimate sample sizes per group
sample_sizes_est = function(table_header, # table header with column headings
            processed_table, # statistics per row
            first_row) # first row of table, can have totals
{

# which row of the header has the n's
  n_row = 0
  if(nrow(table_header) > 0){
    n_row = rep(0, nrow(table_header))
    for (r in 1:nrow(table_header)){
      this_row = as.character(table_header[r, ])
      n_row[r] = sum(str_detect(string = this_row, pattern = 'n=|n =| n$'), na.rm = TRUE)
    }
    if(sum(n_row) > 0){ # if there's at least one row with 
      select_row = table_header[n_row == max(n_row),]
      sample_sizes = extract_n(select_row)
    }
  }
  
  if(sum(n_row) == 0){ # use column headers if n's are not in table cells
    sample_sizes = extract_n(names(table_header))
  }

# if this first step (above) has worked then end and return results
if(nrow(sample_sizes) > 0){
 return(sample_sizes)
}
  
# if no sample sizes from column headers guess from: 1) numbers, 2) percents in table cells
 if(any(processed_table$statistic == 'number')){
  numbers = filter(processed_table, statistic == 'numbers',
          !is.na(stat1),
          !is.na(stat2)) %>%
   mutate(n_est =  stat1 + stat2) %>% # estimate sample size from n1 + n2
   filter(!is.na(n_est)) %>% # exclude missing
   group_by(column) %>%
   summarise(sample_size = Mode(n_est)) %>% # get mode as an estimate of sample size
   ungroup()
  if(nrow(numbers) > 0){return(numbers)} # use if there are any results
 }
 # try percents next 
 percents = filter(processed_table, statistic == 'percent',
          !is.na(stat1),
          !is.na(stat2)) %>%
  mutate(n_est = stat1*(100/stat2)) %>% # estimate sample size from n and % (assume statistics are presented as n then %)
  filter(!is.na(n_est)) %>% # exclude missing
  group_by(column) %>%
  summarise(sample_size = Mode(n_est)) %>% # get mode as an estimate of sample size
  ungroup()
 if(nrow(percents) > 0){
  rounding = mean(abs(percents$sample_size - round(percents$sample_size))) # average rounding
  if(rounding < 0.05){ # must be small rounding error, otherwise statistics are not percents
   percents = mutate(percents, sample_size = round(sample_size) )
   return(percents)
  }
 }
  
  # if still nothing try first row
  first_row = select(first_row, -header)
  if(str_squish(first_row[1])%in% c('n','total')){
    numbers = first_row[-1] # remove header column
    tran = data.frame(t(numbers)) # transpose
    names(tran) = paste('C', 1:ncol(tran), sep='')
    tran$labels = rownames(tran)
    tran = filter(tran, labels!='%') %>% # remove cells that are just percents
      mutate(C1=str_squish(C1)) %>% # remove spaces
      separate(C1, c('stat1','stat2'), sep = ' ', fill = 'right') %>% # extract two statistics
      mutate(stat1 = suppressWarnings(as.numeric(stat1)), # sum numbers not converted because of things like ".16"
             stat2 = suppressWarnings(as.numeric(stat2)))
    nums = data.frame(column=1:nrow(tran), sample_size=tran$stat1) # assume n is first statistic
    return(nums)
  }

} # end of function

## function used to examine table cells
make_cells = function(indata){
  names(indata) = 1:ncol(indata) # generic column name
  indata$row = 1:nrow(indata)
  table_cells = reshape::melt(id.vars = 'row', indata, variable_name = "column") %>% 
    mutate(column = as.numeric(column),
           percents = str_detect(string = value, pattern = '%'), # see 0_pattern.R 
           continuous = grepl(x = value, pattern = paste(plus_minus, collapse = '|')),
           numbers = str_detect(string = value, pattern = '/'),
           min_max = str_detect(string = value, pattern = 'min|max'),
           pvals = str_detect(string = value, pattern = 'p.value|p =|p='),
           median = str_detect(string = value, pattern = paste(median_numbers, collapse = '|')))
  return(table_cells)
}