# 99_functions.R
# functions for extracting baseline table data and p-value testing
# June 2021

# simple function used by 4_model.R
is.this = function(x, i){sum(x==i)/length(x)}

## extract baseline tables
baseline_table = function(webpage, # paper tables in xml format 
                          table_number = 1, 
                          footnote,
                          weight = 1) # weight feeds into statistics detect function
  {
  
  # binary variable tested later; were there p-values in table?
  pvalues_in_table = FALSE
  
  # maximum number of tables - probably no longer needed
  max_table = length(webpage %>% xml_nodes("table-wrap"))
  if(table_number > max_table){ # table likely in appendix or graphic
    to.return = list()
    to.return$reason = 'Table in appendix or table is graphic'
    to.return$table = NULL
    return(to.return) # stop here
  }
  
  # extract and tidy footnote
  table_footnote = webpage %>% 
    xml_nodes("table-wrap") %>% 
    xml_nodes('tfoot') %>% 
    xml_text()
  footnote = paste(footnote, table_footnote)
  footnote = str_squish(str_replace_all(string=tolower(footnote), pattern="[^0-9|a-z|=|%| ]", ' '))  # remove all special characters to make matching below easier

  # extract baseline table 
  table1 <- webpage %>%
    xml_nodes("table-wrap") %>%
    .[table_number]  # table number for baseline table
  # check for graphics
  graphic = str_detect(as.character(table1), pattern='<inline-graphic')
  if(graphic == TRUE){ # 
    to.return = list()
    to.return$reason = 'Table is graphic'
    to.return$table = NULL
    return(to.return) # stop here
  }
  # remove footnote within table
  xml_find_all(table1, ".//tfoot") %>% xml_remove()    
  #
  table1 =  xml_nodes(table1, "table") %>% # get first table in wrap
    html_table(header = TRUE, fill = TRUE) # include table headers
  table1 = table1[[1]] # to data frame
  
  # if null table then stop here
  if(nrow(table1) == 0){ # table likely a graphic
    to.return = list()
    to.return$reason = 'Table is graphic'
    to.return$table = NULL
    return(to.return) # stop here
  }

  ## remove blank rows
  index = which(rowSums(table1=='') == ncol(table1))
  if(length(index)>0){
    table1 = table1[-index,]
  }
    
  ## remove odd characters and change to lower case - helps with searching below
  cnames = tolower(names(table1))
  cnames = gsub(cnames, pattern='(?=\\(\\d{1,}\\))', replacement='n=', perl=TRUE) # replace '(numbers)` with '(n=numbers)'
  cnames = str_squish(str_replace_all(cnames, pattern = '[^=|a-z|0-9| ]', replacement=' ')) # just keep letters, numbers, equals
  names(table1) = cnames
  cnames[is.na(cnames)] = ''
  if(any(cnames == '')){ # cant have null table numbers
    index = names(table1) == ''
    index[is.na(index)] = TRUE
    names(table1)[index] = paste('c',1:sum(index),sep = '')
  }
  # any duplicate table names? 
  cnames = names(table1)
  # if both `variable` then just change one
  if(any(tolower(cnames)=='variable')){
    index = which(tolower(cnames)=='variable')[1]
    cnames[index] = 'VARIABLES'
    names(table1) = cnames # replace
  }
  # 
  if(any(duplicated(cnames)) == TRUE){
    # if multiple columns are 'variable' then likely to be a terrible layout
    check = sum(tolower(cnames) == 'variable')
    if(check > 0){
      to.return = list()
      to.return$reason = 'Difficult layout'
      to.return$table = NULL
      return(to.return) # bail out here, will be impossible to unpick
    }
    # avoid repeat names by adding letters to start (just for duplicated)
    dindex = duplicated(names(table1))
    dcount = sum(dindex)
    these_letters = LETTERS[1:dcount]
    names(table1)[dindex] =  paste(these_letters, names(table1)[dindex], sep = '') 
  }

  ## transpose badly formatted tables here (where results are in rows instead of columns)
  # if more columns than rows then assume transposed ...
  rows_columns = nrow(table1) < ncol(table1)
  # ... and if no 'age' or 'gender' in column labels
  common_labels = any(str_detect(pattern='age|height|weight|gender|sex', tolower(table1[,1])))
  # or if common labels are in the table column headings, e.g., PMC5998772
  common_labels_headings = any(str_detect(pattern='age|height|weight|gender|sex', tolower(names(table1))))
  to_transpose =  (rows_columns == TRUE & common_labels==FALSE) | (common_labels_headings == TRUE & nrow(table1) < 10) # added nrow here because of tables without headers, PMC5731727
  if(to_transpose == TRUE){ 
    # only if there's no age or gender in column labels, e.g., PMC7230691
    any_age_sex = str_detect(string=as.character(table1[,1]), pattern='sex|age')
    if(any(any_age_sex)==FALSE){
      table1 = t(table1)
      h = table1[1,]
      no_name = h==''
      if(any(no_name)){ # if names missing then replace with letters
        h[no_name] = LETTERS[1:sum(no_name)]
      }
      table1 = data.frame(table1[-1,]) # remove top row
      names(table1) = h
      # add labels as first column
      table1 = bind_cols(row.names(table1), table1)
      names(table1)[1] = 'label'
    }
  }
  # neaten first column with labels
  table1[,1] = str_squish(table1[,1])
  
  ## if sample size in first two rows then add to header
  sample1 = str_detect(string=tolower(table1[1,]), pattern=sample_patterns)
  sample1[is.na(sample1)] = FALSE
  sample2 = str_detect(string=tolower(table1[2,]), pattern=sample_patterns)
  sample2[is.na(sample2)] = FALSE
  match_prop1 = sum(sample1[2:length(sample1)]) / (length(sample1) - 1)
  match_prop2 = sum(sample2[2:length(sample2)]) / (length(sample2) - 1)
  if(sample1[1] == TRUE | sample2[1] == TRUE  | match_prop1 > 0.5 | match_prop2 > 0.5){ # if left-most label or if enough column labels
    n_row = 2 # start on second row ...
    if(sample1[1]==TRUE | match_prop1 > 0.5){n_row = 1} # ... switch to first
    # create labels to add to header, but only if n is not already in labels
    already = str_detect(string=tolower(names(table1)), pattern=sample_patterns)
    if(any(already) != TRUE){
      labels = paste('n=',table1[n_row, ], sep='')
      labels[labels=='n='] = '' # remove empty labels
      names(table1) = paste(names(table1), labels)
    }
    # remove first or second row
    table1 = table1[-n_row,] 
  }
    
  ## stop if just one column
  if(ncol(table1) <= 2){return(stop_one_column())}

  ## if both plus/minus and `n (%)` formats (but without percent in text) then add % to help stats detector ...
  # ... or if percent mentioned in footnote
  any_plus_minus = any(str_detect(as.character(table1), pattern=paste(plus_minus, collapse='|')))
  any_count_in_footnote = any(str_detect(footnote, pattern=percent_specific))
  if(any_plus_minus == TRUE | any_count_in_footnote ==TRUE){ # some plus/minus or percent used ...
    any_percent = any(str_detect(as.character(table1), pattern='%'))
    if(any_percent == FALSE){ # ... with no percents
      any_num_bracket = any(str_detect(as.character(table1), pattern='[0-9] \\([0-9]'))
      if(any_num_bracket == TRUE){ # then add % to these cells
        table1 = mutate_all(table1, add_percent_function)
      }
    }
  }
  
  ## if "numbers (numbers - numbers)" then flag median
  table1 = mutate_all(table1, flag_median_function)

  # clean up table text and column headers:
  colnames(table1) = tolower(removeunicode(colnames(table1))) # remove unicode from headers too
  table1 = mutate_all(table1, tolower) %>%
    mutate_all(add_space_function) %>% # add space before and after brackets to stop numbers getting compressed
    mutate_all(removeunicode) %>% # 
    mutate_all(string_remove_function)

  ## drop any columns that are the total
  # also drop any columns that are labelled 'men' or 'women' as these are subgroups not randomised groups
  text_to_check = tolower(names(table1))
  text_to_check = str_squish(str_remove_all(text_to_check, 'n=|n =|[0-9]')) # remove sample size
  to_drop_header1 = str_detect(string=text_to_check, total_words) # see 0_patterns.R for words
  to_drop_header2 = str_detect(string=text_to_check, total_words_alt) # because of letters added to column headers to avoid name clashes
  to_drop_header1[is.na(to_drop_header1)] = FALSE
  to_drop_header2[is.na(to_drop_header2)] = FALSE
  to_drop_header = as.logical(pmax(to_drop_header1, to_drop_header2))
  # also check first row - no need for total_words_alt
  text_to_check = tolower(table1[1,])
  text_to_check = str_squish(str_remove_all(text_to_check, 'n=|n =|[0-9]')) # remove sample size
  text_to_check[is.na(text_to_check)] ='' # replace missing
  to_drop_first = str_detect(text_to_check, total_words) # see 0_patterns.R for words
  to_drop_first[is.na(to_drop_first)] = FALSE
  to_drop = as.logical(pmax(to_drop_header, to_drop_first))
  to_drop[1] = FALSE # never drop first column
  if(any(to_drop) == TRUE){
    col_names = names(table1)[to_drop]
    table1 = select(table1, -all_of(col_names))
  }
  ## stop if just one column
  if(ncol(table1) <= 2){return(stop_one_column())}
  
  # combine first two to three columns if they are both mostly text (double label column)
  previous = ncol(table1)
  table1 = combine_cols(table1) # first two columns ...
  if(ncol(table1)< previous){
    table1 = combine_cols(table1) # .. and try third column
  }
  ## stop if just one column
  if(ncol(table1) <= 2){return(stop_one_column())}
  
  ## exclude papers that are using a pre-post comparison as p-values may be valid
  test_cols1 = str_detect(string = names(table1), pattern=prepost_pattern) # check names
  test_cols2 = str_detect(string = table1[1,], pattern=prepost_pattern) # check first row
  test_cols = as.logical(pmax(test_cols1, test_cols2))
  test_cols[is.na(test_cols)] = FALSE
  if(any(test_cols) == TRUE){
    to.return = list() 
    to.return$reason = 'Pre-post comparison'
    to.return$table = NULL
    return(to.return) # stop here
  }
    
  ## remove columns that are test statistics 
  test_cols1 = str_detect(string = names(table1), pattern=test_pattern) # check names
  test_cols2 = str_detect(string = table1[1,], pattern=test_pattern) # check first row
  test_cols = as.logical(pmax(test_cols1, test_cols2))
  test_cols[is.na(test_cols)] = FALSE
  stats_column = NULL
  if(any(test_cols)){
    col_names = names(table1)[test_cols]
    stats_column = select(table1, all_of(col_names)) # keep for later, in case p-values are in here
    table1 = select(table1, -all_of(col_names))
  }
  
  ## remove columns that are just the range, e.g. PMC8073435  ...
  plus = paste(min_max_pattern_whole, '^quintile', sep='|') # ... and add quintile, PMC6761647
  test_cols1 = str_detect(string = tolower(names(table1)), pattern=plus) # check names
  test_cols2 = str_detect(string = tolower(table1[1,]), pattern=plus) # check first row
  test_cols = as.logical(pmax(test_cols1, test_cols2))
  test_cols[is.na(test_cols)] = FALSE
  if(any(test_cols)){
    col_names = names(table1)[test_cols]
    table1 = select(table1, -all_of(col_names))
  }
  ## stop if just one column
  if(ncol(table1) <= 2){return(stop_one_column())}

  ## combine statistics in neighbouring columns
  table1 = combine_columns(table1, stat1='mean', stat2='sd')
  if(ncol(table1) <= 2){return(stop_one_column())} # stop if 1 column
  table1 = combine_columns(table1, stat1='n', stat2=c('\\%','percent'))
  if(ncol(table1) <= 2){return(stop_one_column())}# stop if 1 column
  table1 = combine_columns(table1, stat1=c('\\%','percent'), stat2='n', reverse=TRUE)
  
  ## remove columns that are purely sample size (and put text in header) 
  test_cols1 = str_detect(string = str_squish(names(table1)), pattern=c('^n$|^n/n$|^[a-z]n$')) # check names, last pattern is because i add random letter to avoid duplicate names
  test_cols2 = str_detect(string = str_squish(table1[1,]), pattern=c('^n$|^n/n$')) # check first row
  test_cols1[is.na(test_cols1)] = FALSE
  test_cols2[is.na(test_cols2)] = FALSE
  test_cols = as.logical(pmax(test_cols1, test_cols2)) # combine rows
  if(any(test_cols) & length(test_cols)>2){ # if 2 or fewer columns then will be removed below
    # check the columns are just numbers
    M = as.matrix(table1[,test_cols]) # need to transform into matrix
    M[is.na(M)] = ''
    nums = matrix(data = rep(1:ncol(M), nrow(M)), ncol = ncol(M), nrow = nrow(M), byrow = TRUE) # make matrix of column numbers
    vector_nums = as.vector(as.matrix(nums))
    check_nums = str_detect(pattern='^[0-9]+$|^[0-9]+/[0-9]+$', M) # search for just numbers in cell
    denominators = colSums(M!='') - colSums(M=='n') # exclude empty cells from denominator (also avoid counting 'n')
    prop = table(vector_nums[check_nums]) / denominators
    n_counts = prop > 0.85 # rows are more than 85% numbers
    n_counts[is.na(n_counts)] = FALSE
    if(any(n_counts) == TRUE){ # only proceed if 85% met
      ## put numbers in header for sample size detection, only if there's not any `n=` in header already
      if(!any(str_detect(pattern='n=|n =',names(table1)))){
        # get average sample size
        numbers = matrix(as.numeric(M), ncol = ncol(M)) # ignore NA warnings
        nmean = round(colMeans(numbers, na.rm=TRUE))
        # find first row with numbers
        n_text= paste('n=', nmean, sep='') # make detectable text
        index = which(test_cols)+1 # assume it is next column along
        names(table1)[index] = paste(names(table1)[index], n_text)
      }
      # remove columns from table
      table1 = table1[, -which(test_cols)]
    }
  }
  
  ## stop (again) if just one column
  if(ncol(table1) <= 2){return(stop_one_column())}
  
  ## remove rows with ratios, e.g., 7954267, can't process this statistic
  ratio = str_detect(table1[,1], pattern='\\bratio\\b')
  ratio[is.na(ratio)] = FALSE
  ratio_ok = str_detect(table1[,1], pattern='waist.to.hip.ratio|waist.to.height.ratio') # these are okay
  ratio_ok[is.na(ratio_ok)] = FALSE
  ratio[ratio_ok] = FALSE # remove 'good' ratios from this exclusion
  if(any(ratio) == TRUE){
    table1 = table1[!ratio, ]
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
  numbers_start = numbers_start_function(table1)
  
  ## Additional check for labels that are "N"
  any_n = table1[,1]=='n'
  any_n[is.na(any_n)] = FALSE
  if(any(any_n)){
    index = which(table1[,1]=='n')
    #numbers_start = index + 1 # turned off did not work for PMC7005671
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
  
  ## remove rows that are sample size updates
  nums = matrix(data = rep(1:nrow(no_header), ncol(no_header)), ncol = ncol(no_header), nrow = nrow(no_header)) # make matrix of numbers
  vector_nums = as.vector(as.matrix(nums))
  table_text = str_squish(as.vector(as.matrix(no_header)))
  any_n = str_detect(table_text, pattern='n = [0-9]|n=[0-9]|n =[0-9]|n= [0-9]')
  rows_with_n = vector_nums[any_n]
  tab = table(rows_with_n)
  index = which(tab == ncol(no_header) - 1) # must be all columns
  rows_with_n = as.numeric(names(index))
  no_header = no_header[1:nrow(no_header) %in% rows_with_n == FALSE, ] # remove rows from table

  ## find rows in the table that are just text as these are likely header rows (so just keep rows with some numbers)
  nums = matrix(data = rep(1:nrow(no_header), ncol(no_header)), ncol = ncol(no_header), nrow = nrow(no_header)) # make matrix of numbers
  vector_nums = as.vector(as.matrix(nums))
  table_text = str_squish(as.vector(as.matrix(no_header)))
  table_text[is.na(table_text)] = ''
  # proportion of numbers divided by characters (just exclude spaces)
  prop_numbers = str_count(table_text, pattern = '[0-9]') / str_count(table_text, pattern = '[^\\s]')
  prop_numbers[is.na(prop_numbers)] = 0
  matrix_prop = matrix(prop_numbers, ncol=ncol(no_header)) > 0.5 # more than 50% numbers
  rows_with_numbers = which(rowSums(matrix_prop) > 0)

  ##### detect use of statistics for each row ####
  ## split table if there are multiple header rows (usually a second row mid-way down table)
  diffs = diff(rows_with_numbers)
  n_splits = sum(diffs > 1)
  if(n_splits == 0){ # if no header rows
    stats_detect = statistics_detect(intable=no_header, header = table_header, footnote=footnote, caption=caption, weight = weight)
  }
  if(n_splits > 0){ # if some header rows
    # first split
    stats_detect_multiple = list()
    index = which(diffs>1)[1] # first split location
    this_split = no_header[1:index,]
    stats_detect_multiple[[1]] = statistics_detect(intable=this_split, header = table_header, footnote=footnote, caption=caption, weight = weight)
    
    # then further splits
    for (row_split in 1:n_splits){
      index = which(diffs>1)[row_split] # split location
      if(row_split == n_splits){ 
        rstop = nrow(no_header)
      }
      if(row_split < n_splits){ 
        index_next = which(diffs>1)[row_split+1] # next split location
        rstop = index_next + row_split + (diffs[diffs>1][row_split] - 2)
      }
      rstart = index + row_split + (diffs[diffs>1][row_split] - 1) # last term needed for double-breaks
      this_split = no_header[rstart:rstop, ] # select rows of table
      # use nearest header
      new_header = no_header[rstart-1,] # from table with breaks, get sub-heading as it can contain statistics
      names(new_header) = str_remove_all(names(new_header), pattern='n.=|n=') # n was getting confused with percent
      stats_detect_multiple[[row_split+1]] = statistics_detect(intable=this_split, header = new_header, footnote=footnote, caption=caption, weight = weight)
      stats_detect_multiple[[row_split+1]]$table$row = stats_detect_multiple[[row_split+1]]$table$row + max(stats_detect_multiple[[row_split]]$table$row) # adjust row numbers
    } 
    # now create overall list
    stats_detect = list()
    stats_detect$table = bind_rows(lapply(stats_detect_multiple, '[[', 2))
    # carry forward stats from previous row if there's no other type
    stats_detect$table = tidyr::fill(stats_detect$table, statistic, .direction = 'down')
    any_remove = unique(unlist(lapply(stats_detect_multiple, '[[', 1)))
    stats_detect$columns_to_remove = any_remove
  }

  # now remove header rows from table
  no_header = no_header[rows_with_numbers,] # just keep rows with numbers
  
  # remove columns and record p-values in table ...
  if(any(!is.na(stats_detect$columns_to_remove))){ # at least one not missing
    to_remove = stats_detect$columns_to_remove
    to_remove = to_remove[!is.na(to_remove)]
    no_header = no_header[, -to_remove]
    table_header = table_header[, -to_remove]
    pvalues_in_table = TRUE
  }
  
  stats_detect = stats_detect$table # ... then change list to table of cells
  # update `p-values in table` if p-values are in rows
  if(any(stats_detect$statistic == 'pvals', na.rm = TRUE)){pvalues_in_table = TRUE}

  # exclude if just one column (again), but this time with p-values
  if(ncol(no_header) <= 2){return(stop_one_column(presult=pvalues_in_table, pvalues=TRUE))}
  
  # check footnote and stats columns for p-values
  if(pvalues_in_table == FALSE & !is.na(footnote)){
    footp = str_detect(string=tolower(footnote), pattern=pval_pattern)
    if(footp == TRUE ){pvalues_in_table = TRUE}
    if(!is.null(stats_column)){
      colp = str_detect(string=as.character(stats_column), pattern=pval_pattern)
      if(any(colp) == TRUE ){pvalues_in_table = TRUE}
    }
  }

  # convert table to long format (statistics per row and column)
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
                 text = str_replace_all(text, pattern = '[^-|0-9|.| ]', replacement = ' '), # just keep numbers, decimals, negatives and spaces (order matters, hyphen must be first)
                 text = str_squish(string = text)) %>% # remove any double spaces added, and spaces at start/end
    filter(!is.na(statistic)) %>% # only proceed if we know the statistic
    separate(text, c('stat1','stat2','stat3','stat4'), sep = ' ', fill = 'right') %>% # extract four statistics
    # calculate decimal places
    mutate(dp1 = decimalplaces(stat1),
           dp2 = decimalplaces(stat2)) %>%
    # then convert statistics to numbers
    mutate(stat1 = suppressWarnings(as.numeric(stat1)), # 
           stat2 = suppressWarnings(as.numeric(stat2)),
           stat3 = suppressWarnings(as.numeric(stat3)),
           stat4 = suppressWarnings(as.numeric(stat4))) %>%
    filter(!is.na(stat1)) # knock out missing cells
  
  ## get sample size from top row(s) or cells, then add to table
  sample_sizes = sample_sizes_est(table_header = table_header[,-1], processed_table = table, first_rows = table1[1:5,]) # header without first column 
  if(is.null(sample_sizes)==TRUE){
    to.return = list()
    to.return$pvalues_in_table = pvalues_in_table # can still record this
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

  # if top row of table is sample sizes then remove top row
  n_same = nrow(filter(table, row==1, stat1==sample_size))
  if(n_same == max(table$column)){ # if all statistics are the sample size
    table = filter(table, row > 1) # chop top row
  }
  
  # fill in numerator if only percent given. Turned off because not always working
  #to_fill = filter(table, is.na(stat2), statistic == 'percent')
  #if(nrow(to_fill)>0){
  #  okay = filter(table, !is.na(stat2) | statistic != 'percent') # rows that are okay
  #  to_fill = mutate(to_fill,
  #                   stat2 = stat1, # move percent to second cell
  #                   stat1 = round((stat2/100)*sample_size)) # estimate numerator 
  #  table = bind_rows(okay, to_fill)
  #}
  
  
  ## If single integer in first cell then assume it is a count
  # commented out as this does not work enough of the time
  #table = mutate(table,
  #            statistic = ifelse(is.na(stat2) & (round(stat1) - stat1)==0, 'percent', statistic), # change type
  #            stat2 = ifelse(is.na(stat2) & (round(stat1) - stat1)==0, 100*(stat1/sample_size), stat2)) # calculate percent
  
  # arrange table
  table = mutate(table, 
                 row = as.numeric(as.factor(row)),
                 column = as.numeric(as.factor(column)))
  table = arrange(table, row, column) %>%
    filter(!is.na(row),
           !is.na(column))
  
  # check for repeat columns, happened with PMC7270845, also PMC7298630
  # need to progress by column if there are multiple duplicates - just run multiple times for now!
  table = find_duplicate_columns(table)
  table = find_duplicate_columns(table)
  table = find_duplicate_columns(table)
  table = find_duplicate_columns(table)
  
  ## check for columns that can be combined, e.g., https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8036030/ which has results spread over columns
  tab = with(table, table(row, column)) # table of zeros and ones
  upper_column = max(table$column) - 1 # number of columns to loop through
  re_number = FALSE
  for (loop in seq(1, upper_column, 2)){ # is there a perfect pattern in neighbouring columns? (done in pairs)
    pattern = sum(tab[,loop] == (1-tab[,loop+1])) # count of mirror image
    if(pattern / max(table$row) > 0.9){ # more than 90% mirror image
      table = mutate(table, column = ifelse(column == loop+1, loop, column)) # decrease column number by 1
      re_number = TRUE
    }
  }
  if(re_number==TRUE){table = mutate(table, column = as.numeric(as.factor(column)))} # re-number to avoid gaps in column numbers

  # return final results
  to.return = list()
  to.return$reason = NULL # no reason to exclude
  to.return$table = table
  to.return$pvalues_in_table = pvalues_in_table
  return(to.return)
  
}

## function to detect what statistics are used in each row of the table
# assume 1st column contains labels
# weight: >1 then mentions column headers count for more than mentions in rows; <1 then vice versa
statistics_detect = function(intable, 
                             header,
                             caption, # sometimes clues on statistics are in the title ...
                             footnote, # ... more often in the footnote
                             foot_weight = 0.1, # mentions in title/footnote count less
                             weight = 2) 
{
  
  columns_to_remove = NA # store columns to remove
  
 ## a) look for stats in header
 #header = header[,-1] # do not use first column (usually labels) - turned off again because header can have useful info
 header = rbind(header, names(header)) # add header as text and remove names
 row.names(header) = NULL
 names(header) = NULL
 header = t(header) # transpose
 header = data.frame(cbind(1:nrow(header), header))
 header = reshape::melt(id.vars = 'X1', header)
 stats_header = mutate(header,
                       value = str_squish(value),
                       X1 = as.numeric(X1),
  percent = str_count(value, pattern = percent_pattern), # see 0_pattern.R for all patterns
  continuous = str_count(value, pattern = continuous_pattern), # grepl(x = value, pattern = continuous_pattern),
  numbers = str_count(value, pattern = number_pattern),
  min_max = str_count(value, pattern = min_max_pattern),
  pval = str_detect(value, pattern = pval_pattern), # just detect, no need for count
  median = str_count(value, pattern = median_pattern)) %>% 
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
 pval_cols = as.numeric(filter(stats_header, col_pvals > 0) %>% pull(column))
 plus_one = 0 # should be one because of label columns? Can't work this out!
 if(length(pval_cols)>0){
   intable = intable[, -(pval_cols+plus_one)] # remove columns
   columns_to_remove = c(columns_to_remove, pval_cols+plus_one) # add to information to return (plus one because of label columns)
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
 # first remove 'min/' as it gets confused with minimum
 to_test = intable[,1]
 to_test = str_remove_all(to_test, pattern='min/')
 percent_alone = str_count(to_test, pattern = '\\%|percent') # just % (give higher weighting to this)
 percents = str_count(to_test, pattern = percent_pattern) # see 0_pattern.R for all patterns
 mean_alone = str_count(to_test, pattern = 'mean') # just mean (give higher weighting to this)
 continuous = str_count(to_test, pattern = continuous_pattern) # changed from grepl, may need to change back
numbers = str_count(to_test, pattern = number_pattern)
min_max = str_count(to_test, pattern = min_max_pattern)
pvals = str_detect(to_test, pattern = pval_pattern) # just detect instead of count
median_alone = str_count(to_test, pattern = 'median') # just median (give higher weighting to this)
medians = str_count(to_test, pattern = median_words) # just look for median words, not number patterns
stats_label_column = data.frame(row = 1:nrow(intable), 
             percents = (percent_alone*2) + as.numeric(percents) , # using weights, but not for percent because of things like Left ventricular ejection fraction and HbA1c
             continuous = ((mean_alone*2) + continuous) * weight,
             numbers = numbers * weight,
             min_max = min_max * weight,
             pvals = pvals * weight,
             medians = median_alone*2*weight + medians * weight*1.1) # double if median alone is found, tiny increase in median to avoid ties

## c) melt to long to look at stats units in cells
stats_cells = full_join(table_cells, stats_header_row, by=c('row','column')) %>% # add header estimates, using `table_cells` from above
 group_by(row) %>%
 summarise(cell_percents = sum(percents, na.rm = TRUE) + sum(col_percent, na.rm = TRUE), # total counts per row
      cell_continuous = sum(continuous, na.rm = TRUE) + sum(col_continuous, na.rm = TRUE),
      cell_numbers = sum(numbers, na.rm = TRUE) + sum(col_numbers, na.rm = TRUE),
      cell_min_max = sum(min_max, na.rm = TRUE) + sum(col_min_max, na.rm = TRUE),
      cell_pvals = sum(pvals, na.rm = TRUE) + sum(col_pvals, na.rm = TRUE),
      cell_median = sum(median, na.rm = TRUE) + sum(col_median, na.rm = TRUE)) 

## d) look for stats in footnotes and captio, apply to all cells
# empty stats in case there is no footnote
footnote_stats = data.frame(row = 1:max(stats_cells$row)) %>%
  mutate(
    footnote_percents = 0, # 
    footnote_continuous = 0,
    footnote_min_max = 0,
    footnote_medians = 0
  )
both = tolower(paste(footnote, caption, sep=' ')) # add title and footnote together
if(!is.na(both)){
  if(nchar(both)>1){
    footnote_stats = data.frame(row = 1:max(stats_cells$row)) %>%
      mutate(
        footnote_percents = str_count(both, pattern = footnote_percent), # 
        footnote_continuous = str_count(both, pattern = footnote_continuous), # changed from grepl, may need to change back
        footnote_min_max = str_count(both, pattern = footnote_min_max),
        footnote_medians = str_count(both, pattern = footnote_median)
      ) %>%
      mutate_all(as.numeric)
    # search for specific patterns
    median_specific_detect = str_detect(both, pattern=median_specific)
    continuous_specific_detect = str_detect(both, pattern=continuous_specific)
    percent_specific_detect = str_detect(both, pattern=percent_specific)
    footnote_stats = mutate(footnote_stats, # divide by foot_weight below to make sure weights count big
          footnote_medians = footnote_medians + ((1/foot_weight)*median_specific_detect),
          footnote_continuous = footnote_continuous + ((1/foot_weight)*continuous_specific_detect),
          footnote_percents = footnote_percents + ((1/foot_weight)*percent_specific_detect)
    )
  }
}

# now combine cells and column labels and make best guess about statistics per row    
combine = full_join(stats_cells, stats_label_column, by = 'row') %>%
  full_join(footnote_stats, by='row') %>%
 mutate(percent_score = as.numeric(cell_percents + percents + foot_weight*footnote_percents), # scores that combine cell and column information
     continuous_score = as.numeric(cell_continuous + continuous + foot_weight*footnote_continuous),
     numbers_score = as.numeric(cell_numbers + numbers),
     min_max_score = as.numeric(cell_min_max + min_max + foot_weight*footnote_min_max),
     pvals_score = as.numeric(cell_pvals + pvals),
     median_score = as.numeric(cell_median + medians + foot_weight*footnote_medians)) %>%
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

## text cleaning and detecting functions
# set up as a functions because used in mutate_all below
string_remove_function = function(x){str_remove_all(string = x, pattern = '[^a-z|0-9| |.|·|%|–|-|-|±|/|:| = |!!!]')} # need function for mutate_all, note multiple difference hyphens; triple !!! is my flag for median
add_space_function = function(x){str_replace_all(string = x, pattern = '\\(|\\)|,', replacement = ' \\(')} # use opening bracket for both replacements
removeunicode = function(x){gsub(pattern = "[^[:print:]]", " ", x)} # print = any printable character, see https://www.petefreitag.com/cheatsheets/regex/character-classes/
# next function replaces any number that is "n (%)", do not want to replace "x (y-z)"
add_percent_function = function(x){
  ## add to do all combinations of numbers with decimal places because of my poor regex skills!
  # integers
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9])\\)', replacement='%\\)') # number, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9][0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  # 1 decimal place
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9].[0-9])\\)', replacement='%\\)') # number, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9].[0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9][0-9].[0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  # 2 decimal places
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9].[0-9][0-9])\\)', replacement='%\\)') # number, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9].[0-9][0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9][0-9].[0-9][0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  return(x)
} # 
# flag the median for three numbers with brackets
flag_median_function = function(x){
  # pattern is: number, optional space, bracket, number, optional number, optional dp, optional number, optional space, separator (using or), optional space, number, optional number, optional decimal point, optional number, close bracket
  pattern = '[0-9]\\s?\\([0-9][0-9]?\\.?[0-9]?\\s?(to|-|–|–|,)\\s?[0-9][0-9]?\\.?[0-9]?\\)'
  index = str_detect(string=x, pattern=pattern) # 
  index[is.na(index)] = FALSE
  if(any(index) == TRUE){
    x[index] = str_replace_all(x[index], '\\(', replacement=' !!! ') # add non-ascii flag
  }
  return(x)
}
  
## Mode (used by sample size)
Mode <- function(x) {
 ux <- unique(x)
 ux[which.max(tabulate(match(x, ux)))]
}

## function to extract sample size from column headers
extract_n = function(intext){
 intext[is.na(intext)] = '' # replace NAs
 intext = tolower(intext) # convert to lower text
 intext = str_replace_all(string = intext, pattern = '[^a-z|0-9|=| ]', replacement = ' ') # keep only these (remove lots of characters), replace with spaces because of things like "n:"
 intext = str_squish(intext)
 ns = str_squish(str_split_fixed(string = intext, pattern = 'n=|n =|^n ', n = 2)[,2]) # split on `n = `
 ns = str_split_fixed(string = ns, pattern = ' ', n = 2)[,1] # split again on space (in case of garbage after n=xx)
 ns = as.numeric(str_remove(string = ns, pattern = '\\)')) # extract number - can create warning - ignore
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
            first_rows) # first rows of table, can have totals
{

# which row of the header has the n's
  n_row = 0
  if(nrow(table_header) > 0){
    n_row = rep(0, nrow(table_header))
    for (r in 1:nrow(table_header)){
      this_row = str_squish(as.character(table_header[r, ]))
      n_row[r] = sum(str_detect(string = this_row, pattern = sample_patterns), na.rm = TRUE) # see 0_pattern.R
    }
    if(sum(n_row) > 0){ # if there's at least one row with 
      select_row = table_header[n_row == max(n_row),][1,] # select first row if there are multiple n rows
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
  
  # if still nothing try first available row of stats ...
  first_rows = select(first_rows, -header) # 
  # knock out mostly empty rows (reshape into long)
  first_rows_nums = first_rows[,-1] # drop label column
  names(first_rows_nums) = 1:ncol(first_rows_nums) # generic column name
  first_rows_nums$row = 1:nrow(first_rows_nums)
  any_n = reshape::melt(id.vars = 'row', first_rows_nums, variable_name = "column") %>%
    mutate( value = str_squish(value),
      n_detect = str_detect(string=value, pattern = sample_patterns)) %>%
    group_by(row) %>%
    summarise(sum = sum(n_detect)) %>%
    filter(sum >= 2) %>%
    arrange(sum) %>% # just in case there are multiple take most
    slice(1) %>%
    pull(row)
  if(length(any_n)>0){  
    nums = extract_n(first_rows_nums[any_n,])
    return(nums)
  }

  # ... then try row labels
  n_in_first_rows = str_detect(str_squish(first_rows[,1]), pattern=sample_patterns) # just look at column label
  n_in_first_rows[is.na(n_in_first_rows)] = FALSE
  if(any(n_in_first_rows)){
    top = which(n_in_first_rows)[1] # take highest row number if there are multiple
    to_extract = data.frame(t(first_rows[top,-1])) 
    names(to_extract) = 'value'
    nums = mutate(to_extract, 
                  column=1:n()) %>%
      separate(value, c('stat1','stat2','stat3','stat4'), sep = ' |/', fill = 'right') %>%
      filter(!is.na(stat1)) %>%
      mutate(stat1 = suppressWarnings(as.numeric(stat1))) %>%
      rename('sample_size' = 'stat1') %>%
      select(column, sample_size) %>%
      filter(!is.na(sample_size))
    row.names(nums) = NULL
    if(nrow(nums) > 0 ){  
      return(nums)
    }
  }

  # if no sample sizes from column headers guess from: 1) numbers, 2) percents in table cells
  if(any(processed_table$statistic == 'number')){
    numbers = filter(processed_table, statistic == 'numbers',
                     !is.na(stat1),
                     !is.na(stat2)) %>%
      mutate(n_est =  stat1 + stat2) %>% # estimate sample size from n1 + n2
      filter(!is.na(n_est)) %>% # remove missing
      group_by(column) %>%
      summarise(sample_size = Mode(n_est)) %>% # get mode as an estimate of sample size
      ungroup()
    if(nrow(numbers) > 0){return(numbers)} # use if there are any results
  }
  
    
  ## try reconstructing n from percents 
  # are there rows with the same estimated N?
  est_N = filter(processed_table, statistic == 'percent',
                    !is.na(stat1),
                    !is.na(stat2)) %>%
    mutate(n_est = stat1*(100/stat2), # estimate sample size from n and % (assume statistics are presented as n then %)
           n_est_round = round(n_est)) %>%
    filter(!is.na(n_est)) # exclude missing
  find_same = group_by(est_N, row, n_est_round) %>%
    mutate(N = n()) %>% # count number of non-missing statistics
    group_by(row, n_est_round, N) %>%
    tally() %>%
    ungroup() %>%
    filter(n == N) %>% # all cells give the same N
    pull(row)
  nums = filter(est_N, row==find_same[1]) %>%
    select(column, n_est_round) %>%
    rename('sample_size' = 'n_est_round')
  if(nrow(nums) > 0){ 
      return(nums)
  }

  ## try reconstructing n from numbers (last try)
  # are there rows with the same estimated N?
  est_N = filter(processed_table, statistic == 'numbers',
                 !is.na(stat1),
                 !is.na(stat2)) %>%
    mutate(n_est = stat1 + stat2)
  find_same = group_by(est_N, row) %>%
    mutate(N = n()) %>% # count number of non-missing statistics
    group_by(row, n_est, N) %>%
    tally() %>%
    ungroup() %>%
    filter(n == N) %>% # all cells give the same N
    pull(row)
  nums = filter(est_N, row==find_same[1]) %>%
    select(column, n_est) %>%
    rename('sample_size' = 'n_est')
  if(nrow(nums) > 0){ 
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
           continuous = str_count(string = value, pattern = paste(c(plus_minus_utf, plus_minus), collapse = '|')), # changed from grepl, may need to change back
           numbers = str_detect(string = value, pattern = '/'),
           min_max = str_detect(string = value, pattern = 'min|max'),
           pvals = str_detect(string = value, pattern = 'p.value|p =|p='),
           median = str_detect(string = value, pattern = paste(median_numbers, collapse = '|')))
  return(table_cells)
}


## t-test from summary stats, from https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t.test2 <- function(mean, sd, n, equal.variance=TRUE, return_what = 'difference')
{
  m1 = mean[1]
  m2 = mean[2]
  s1 = sd[1]
  s2 = sd[2]
  n1 = n[1]
  n2 = n[2]
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  mdiff = m1 - m2
  t <- mdiff/se 
  if(return_what=='difference'){return(mdiff)}
  if(return_what=='se'){return(se)}
  if(return_what=='t'){return(t)}
}

# approximation of two-sample t-test for binomial data (see D'Agostino 1998)
t.test2.binomial <- function(a, b, c, d, return_what = 't')
{
  m = a + c
  n = b + d
  p1 = a/m
  p2 = b/n
  var1 = p1*(1-p1) # variance
  var2 = p2*(1-p2)
  pooled_var = ((m-1)*var1 + (n-1)*var2) / (m+n-2) # pooled variance
  denom = sqrt((1/m) + (1/n))
  mdiff = p1 - p2 # mean difference
  se = sqrt(pooled_var) * denom
  t = mdiff / se
  if(return_what=='difference'){return(mdiff)}
  if(return_what=='se'){return(se)}
  if(return_what=='t'){return(t)}
}

### get the table data ready for the Bayesian model
make_stats_for_bayes_model = function(indata){
  
  # create all possible comparisons of two columns
  max_columns = group_by(indata, pmcid) %>%
    summarise(max_columns = max(column)) %>%
    ungroup()
  indata = left_join(indata, max_columns, by='pmcid')
  # those with two columns
  two_max = filter(indata, max_columns==2)
  # those with more than two columns
  over_two_max = filter(indata, max_columns > 2)
  new_data = NULL
  if(nrow(over_two_max) > 0){
    pmcids_to_loop = unique(over_two_max$pmcid)
    for(this_pmcid in pmcids_to_loop){
      this_data = filter(over_two_max, pmcid==this_pmcid) # select one study
      combs = combn(1:this_data$max_columns[1], 2) # all pairs of columns
      for (i in 1:ncol(combs)){
        extra_columns = filter(this_data, column %in% combs[,i]) %>%
          mutate(row = row + ((i-1)/ncol(combs)), # slightly alter row number
                 column = as.numeric(as.factor(column))) # column should be 1 or 2
        new_data = bind_rows(new_data, extra_columns)
      }
    }
  } # end of if
  # now bind data
  if(is.null(new_data)==FALSE){
    bind_data = bind_rows(two_max, new_data) %>%
      select(-max_columns)
  }
  if(is.null(new_data)==TRUE){
    bind_data = select(two_max, -max_columns)
  }

  
# a) continuous
  # could add below?
  #  sd = ifelse(is.na(sd), 0.1, sd),
  #  sd = ifelse(sd==0, 0.1, sd)) # avoid zero sd
  cstats = filter(bind_data,
                   statistic == 'continuous',
                  stat2 > 0) %>% # must have positive SD
  group_by(pmcid, row, statistic) %>%
  summarise(
    size = sum(sample_size), # total sample size
    mdiff = t.test2(mean=stat1, sd=stat2, n=sample_size, return_what = 'difference'),
    sem = t.test2(mean=stat1, sd=stat2, n=sample_size, return_what = 'se'),
    t = t.test2(mean=stat1, sd=stat2, n=sample_size, return_what = 't'),
    sem2 = sem^2) %>% # squared
  filter(!is.na(mdiff),
         !is.na(sem2)) %>%
  ungroup() 

  # percents
  pstats = filter(bind_data,
                  statistic %in% c('percent','numbers')) %>%
    group_by(pmcid, row, statistic) %>%
    pivot_wider(id_cols=c(pmcid,statistic,row), names_from='column', values_from=c('stat1','sample_size'))  %>%
    mutate(a = stat1_1, # successes
           b = stat1_2,
           c = sample_size_1 - stat1_1, # failures
           d = sample_size_2 - stat1_2) %>%
    filter(a>=0, b>=0, c>=0, d>=0) %>%
    summarise(
      size = sample_size_1 + sample_size_2, # total sample size
      mdiff = t.test2.binomial(a=a, b=b, c=c, d=d, return_what = 'difference'),
      sem = t.test2.binomial(a=a, b=b, c=c, d=d, return_what = 'se'),
      t = t.test2.binomial(a=a, b=b, c=c, d=d,  return_what = 't'),
      sem2 = sem^2) %>% # squared
    filter(!is.na(mdiff),
           !is.na(sem2),
           sem2 > 0) %>%
    ungroup() 
  
  # combine
  stats = bind_rows(cstats, pstats) %>%
    arrange(pmcid, row) %>%
    mutate(study = as.numeric(as.factor(pmcid))) # turn into a number
  
return(stats)

} # end of function



### simulate baseline table data from one paper in the same format as extracted data
simulate_table1 = function(
  prop_continuous = 0.33, # what proportion of statistics are continuous (remainder are percent)
  lambda_table_rows = 18, # mean number of results (rows) per table (based on real data)
  min_sample_size = 4, # minimum sample size
  mean_sample_size_log = 3.95 , # mean sample size using log-normal (based on real data)
  sd_sample_size_log = 1.23, # SD for sample size using log-normal (based on real data)
  dp = 1, # decimal places for rounding
  issue = 'none' # issue with the data, `none`, `mean` for mean difference simulating non-randomised groups, `var` for variance issue (groups are too similar)
){
  
  sim_data=NULL
  
  # subtract minimum from mean sample size
  mean_sample_size_log = mean_sample_size_log - log(min_sample_size)

# generate number of rows per baseline table 
n_rows = rpois(n=1, lambda=lambda_table_rows) 

# generate statistic for each row
statistic = rbinom(n=n_rows, size=1, prob=prop_continuous) 
statistic = c('percent','continuous')[statistic+1]

# randomly generate sample size using log-normal for right-skew
sample_size = min_sample_size + round(exp(rnorm(n=1, mean=mean_sample_size_log, sd=sd_sample_size_log))) # use same sample size for both groups

## for percents
n_percent = sum(statistic == 'percent')
if(n_percent > 0){
  p1 = runif(n=n_percent, min=0, max=1)
  if(issue != 'mean'){p2 = p1} # if no difference in mean
  if(issue == 'mean'){p2 = min(p1 + 0.1, 1)} 
  p_1 = rbinom(n=n_percent, size=sample_size, prob=p1) # Proportion in group 1
  p_2 = rbinom(n=n_percent, size=sample_size, prob=p2) # Proportion in group 2 (no difference)
  if(issue == 'var'){
    p_2 = p_1 + runif(n=n_percent, min=-0.01, max=0.01) # copy from 1 with small difference
    p_2 = p_1 + sample(x=c(-1,0,1), replace=TRUE, size=n_percent) # temporary
  }
  # create data with structure to match automatically extracted data
  p_frame_1 = data.frame(statistic='percent', row=1:n_percent, column=1, stat1=p_1, sample_size = sample_size)
  p_frame_2 = data.frame(statistic='percent', row=1:n_percent, column=2, stat1=p_2, sample_size = sample_size)
  ## concatenate data
  sim_data = bind_rows(sim_data, p_frame_1, p_frame_2)
}

## for continuous
n_continuous = sum(statistic == 'continuous')
if(n_continuous > 0){
  # simulate multiple continuous variables at once
  mean1 = rnorm(n=n_continuous, mean=50, sd=100) # mean can be pretty much anywhere
  if(issue != 'mean'){mean2 = mean1} # if no difference in mean
  if(issue == 'mean'){mean2 = mean1 * 1.1} # 10% increase in mean
  sd = rgamma(n=n_continuous, shape=5, rate=1)
  s_1 = matrix(rnorm(n=sample_size*n_continuous), nrow=sample_size) # original sample, using normal but could be any distribution
  s_1 = t(s_1)*sd + mean1 # add column means and SDs
  s_2 = matrix(rnorm(n=sample_size*n_continuous), nrow=sample_size)
  s_2 = t(s_2)*sd + mean2
  m_1 = rowMeans(s_1); sd_1 = apply(s_1, 1, sd)
  m_2 = rowMeans(s_2); sd_2 = apply(s_2, 1, sd)
  if(issue == 'var'){
    m_2 = m_1 * runif(n=n_continuous, min=0.9999, max=1.0001) # copy from 1 with small multiplier
    #m_2 = m_1 # temporary
  }
  # round to mimic journal presentation
  m_1 = round(m_1, dp)
  m_2 = round(m_2, dp)
  s_1 = round(s_1, dp+1)
  s_2 = round(s_2, dp+1)
  # create data with structure to match automatically extracted data
  m_frame_1 = data.frame(statistic='continuous', row=n_percent + (1:n_continuous), column=1, stat1=m_1, stat2=sd_2, sample_size = sample_size)
  m_frame_2 = data.frame(statistic='continuous', row=n_percent + (1:n_continuous), column=2, stat1=m_2, stat2=sd_2, sample_size = sample_size)
  ## concatenate data
  sim_data = bind_rows(sim_data, m_frame_1, m_frame_2)
}

return(sim_data)

} # end of function

# function to combine columns that are labels
combine_cols =  function(intable){
  textc = sum(str_count(tolower(intable[,2]), pattern='[a-z]'), na.rm=TRUE) # amount of text in 2nd column
  numbersc = sum(str_count(tolower(intable[,2]), pattern='[0-9]'), na.rm=TRUE) # amount of numbers in 2nd column
  if( (textc/(numbersc+textc)) > 0.8 ){ # if over 80% text then must be a label column
    names(intable)[1:2] = c('label1','label2')
    intable = mutate(intable, 
                     label = ifelse(label1 != label2, paste(label1, label2), label1)) %>% # if repeat then just use first label
      select(-label1, -label2) %>%
      select(label, everything()) # move to first column
  }
  return(intable)
} # end of function

## function to move on table numbers if the table is text
exclude_text_tables = function(xml_table, table_number){
  complete = FALSE
  max_tables = length (xml_table %>% xml_nodes("table")) # maximum number of tables
  while(complete==FALSE){ # may need to be done for repeated tables
  # extract table
  table1 <- xml_table %>%
    xml_nodes("table") %>% # need have -wrap because of multiple tables in one
    .[table_number] %>%
    xml_text() # 
  
  if(length(table1)>0){
    if(nchar(table1)>0){ # needed two if statements because of graphical tables
      text_count = str_count(table1, '[a-z]')
      number_count = str_count(table1, '[0-9]')
      if((number_count / text_count) < 0.1){table_number = table_number +1} # if less than 10% numbers
      if((number_count / text_count) >= 0.1){complete = TRUE}
    }
  }
  
  # no tables available so end
  if(table_number > max_tables){
    table_number = 998
    complete = TRUE
  }
  
  if(length(table1) == 0){ # no text content in table, likely graphical table (need to check) - could add check for <graphic>
    table_number = 999
    complete = TRUE
  }
  } # end of while
  
  return(table_number)
} # end of function

# from https://github.com/agbarnett/decimal.places
decimalplaces <- function(x) {
  detect = str_detect(pattern='\\.', string=x)
  detect[is.na(detect)] = FALSE
  if(any(detect)==TRUE){ #
    res = nchar(str_split(x, pattern = '\\.', simplify = T)[,2])
  }
  if(any(detect)==FALSE){ # no decimal places
    res = rep(0, length(x))
  }
  return(res)
}

### function to combine columns in table
## if mean/sd or n/% in separate columns that should be joined, e.g., PMC5861546
combine_columns = function(intable, stat1='', stat2='', reverse=FALSE){
  # matching patterns
  pattern1a = paste('\\b', paste(stat1, collapse='\\b|\\b'), '\\b', sep='') # whole words with or
  pattern1b = paste('^', paste(stat1, collapse='$|^'), '$', sep='') # start/end
  pattern1 = paste(pattern1a, pattern1b, sep='|', collapse ='|')
  pattern2a = paste('\\b', paste(stat2, collapse='\\b|\\b'), '\\b', sep='') #
  pattern2b = paste('^', paste(stat2, collapse='$|^'), '$', sep='') # start/end
  pattern2 = paste(pattern2a, pattern2b, sep='|', collapse ='|')
  # not patterns (if both statistics together, then not a match) - negative matches
  # e.g., neighbouring columns are 'mean sd' 'mean sd' (or with plus/minus symbol)
  pattern3 = paste(apply(expand.grid(stat1, stat2), 1, paste, collapse="."), collapse='|')
  # search column header
  test_cols1a = str_detect(string = str_squish(names(intable)), pattern=pattern1) # check names
  test_cols1b = str_detect(string = str_squish(names(intable)), pattern=pattern2) # check names
  # search table rows depending on where numbers start
  numbers_start = numbers_start_function(intable)
  test_cols2a = str_detect(string = str_squish(intable[1,]), pattern=pattern1) # check first row
  test_cols2b = str_detect(string = str_squish(intable[1,]), pattern=pattern2) # check first row
  # negative search
  test_cols1c = str_detect(string = str_squish(names(intable)), pattern=pattern3) # 
  test_cols2c = str_detect(string = str_squish(intable[1,]), pattern=pattern3) # 
  # test if neighbours (lag shifts one to the right)
  neighbours1 = test_cols1b == lag(test_cols1a) & test_cols1b == TRUE # first row
  neighbours2 = test_cols2b == lag(test_cols2a) & test_cols2b == TRUE # second row
  neighbours_negative = as.logical(pmax(test_cols1c, test_cols2c)) # combine negative searches
  neighbours1[is.na(neighbours1)] = FALSE
  neighbours2[is.na(neighbours2)] = FALSE
  neighbours_negative[is.na(neighbours_negative)] = FALSE
  neighbours = as.logical(pmax(neighbours1, neighbours2)) # combine rows
  neighbours[neighbours_negative==TRUE] = FALSE # turn off any negative matches
  if(any(neighbours) == TRUE){ # merge columns
    names_order = NULL # for re-ordering columns
    index = which(neighbours)
    for (i in rev(index)){ # work backwards (from right to left)
      if(reverse==FALSE){# combine columns and column names
        intable[,i-1] = paste(intable[,i-1], intable[,i])
        names(intable)[i-1] = paste(names(intable)[i-1], names(intable)[i])
      } 
      if(reverse==TRUE){ # combine columns (reverse for `% n`)
        intable[,i-1] = paste(intable[,i], intable[,i-1])
        names(intable)[i-1] = paste(names(intable)[i], names(intable)[i-1])
      } 
      names_order = c(names_order, names(intable)[i-1])
      intable = intable[, -i] # now remove column 
    }
    names_order = rev(c(names_order, names(intable)[1])) # , keep first column at left-most position; put last, then reverse
    intable = select(intable, names_order, everything()) # keep original column order and move to front
  }
  return(intable)
  
} # End of function

# function to work out row that numbers start on
numbers_start_function  = function(intable){
  cells = intable[,-1] # don't use first column with labels
  long = mutate(cells, row = 1:n())
  numbers_start = reshape::melt(id.vars = 'row', long) %>%
    mutate(value=tolower(value),
           text = str_count(value, '[a-z]'), 
           n = str_detect(value, 'n=|n =|^n$'), # add `n=` to header, third is just "N"
           numbers = str_count(value, '[0-9]')) %>% 
    group_by(row) %>%
    summarise(
      text = sum(text, na.rm = TRUE),
      n = sum(n, na.rm = TRUE),
      numbers =  sum(numbers, na.rm = TRUE)) %>% # amount of text and numbers
    ungroup() %>%
    filter(n < 2) %>% # or with two `n=`
    filter(numbers > text) %>% # first row with more numbers than text
    slice(1) %>%
    pull(row)
  return(numbers_start)
} # end of function

## find duplicate columns in long table
find_duplicate_columns = function(intable){
  duplicates = select(intable, row, stat1, stat2) %>%
    duplicated()
  if(any(duplicates)==FALSE){
    return(intable)
  }
  # if there are duplicates  
  duplicate_rows = intable[duplicates,]
  max_row = max(intable$row) # table size
  # more than 75% match
  find_columns = (table(duplicate_rows$column) / max_row) > 0.75
  if(any(find_columns)==TRUE){
    to_remove = as.numeric(names(find_columns))[1] # just first column (do one at a time)
    intable = filter(intable, !column %in% to_remove) %>%
      mutate(column = as.numeric(as.factor(column))) # re-number columns
  }
  return(intable)
}

# function to stop processing because table is just one column, e.g., PMC3071307
stop_one_column = function(presult=NULL, pvalues=FALSE){ # FALSE if p-values not detected
  to.return = list() 
  if(pvalues==TRUE){to.return$pvalues_in_table = presult} # mostly do not record this as there are no groups to compare
  to.return$reason = 'Just one column in table'
  to.return$table = NULL
  return(to.return) # stop here
}

# function for rounding numbers with zeros kept
roundz = function(x, digits=0){
  dformat = paste('%.', digits, 'f', sep='')
  x = sprintf(dformat, round(x, digits))
  return(x)
}