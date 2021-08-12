# now remove <front> (can also get confused with labels, like <header>)
xml_find_all(webpage, ".//front") %>% xml_remove()
# remove boxed text, gets confused with tables, e.g. "Research in context"
xml_find_all(webpage, ".//boxed-text") %>% xml_remove()
# remove supplementary material as tables in here are not accessible
xml_find_all(webpage, ".//supplementary-material") %>% xml_remove()


## test combine columns function
#
intable = data.frame(var = c('age','sex'), mean = c(88,11), sd=c(10,10))
res = combine_columns(intable, stat1='mean', stat2='sd')
res
names(res)
#
intable = data.frame(var = c('age','sex'), n = c(88,11), percent=c(10,10))
names(intable)[3] = '%'
res = combine_columns(intable, stat1='n', stat2=c('%','percent'))
res
names(res)
#
intable = data.frame(var = c('age','sex'), "%"=c(10,10), n = c(88,11))
names(intable)[2] = '%'
res = combine_columns(intable, stat1=c('%','percent'), stat2='n', reverse=TRUE)
res
names(res)

## no longer needed, using tfoot?
## detect footnote in table
last_row = as.character(table1[nrow(table1),])
text = str_count(string=last_row[1], pattern='[a-z]')
blanks = last_row[2:length(last_row)]==''
blanks[is.na(blanks)] = TRUE
missing = is.na(last_row[2:length(last_row)])
total = sum(pmax(missing, blanks))
match1 = text >= 80 & total == length(last_row) - 1 # if lots of text in first cell and others blank
match2 = text >= 80 & sum(duplicated(last_row)) == length(last_row) - 1 # footnote repeated because of colspan
if(match1|match2){ 
  footnote = paste(footnote, last_row[1]) # add footnote to existing
  table1 = table1[-nrow(table1), ] # remove last row
}

### From 99_functions.R - created this function twice!!! ####

# if single stats in side-by-side columns then combine
table1 = merge_single_stats(table1)

## function to merge side-by-side columns of extracted tables
merge_single_stats = function(in_table){
  # find where n and percent are next to each other
  percents = which(in_table[1,] == '%')
  ns = which(tolower(in_table[1,]) == 'n')
  matches_p = (ns+1) == percents
  # find where mean and sd are next to each other
  mean = which(tolower(in_table[1,]) == 'mean')
  sd = which(tolower(in_table[1,]) == 'sd')
  matches_mean = (mean+1) == percents
  matches = c(matches_p, matches_mean)
  # merge columns
  if(any(matches) == TRUE){
    cols_to_merge = rev(c(ns[matches_p], mean[matches_mean])) # reversed to knock off columns at the end
    new_columns = NULL
    for (c in cols_to_merge){  # could not get unite to work
      two_cols = select(in_table, c:(c+1)) # find two columns
      col_names = names(two_cols)
      col_names = str_sub(col_names, 2, nchar(col_names)) # remove first letter which I added and is different
      if(col_names[1] == col_names[2]){
        new_name = col_names[1] # if column names are the same then just use one name
      }
      if(col_names[1] != col_names[2]){
        new_name = paste(col_names, collapse = ' ') # paste together column names
      }
      new = unite(two_cols, col='c1', everything(), sep=' ') # bind two columns
      names(new) = new_name
      new_columns = bind_cols(new_columns, new)
    }
    in_table = select(in_table, -cols_to_merge, -(cols_to_merge+1)) # remove columns
    in_table = bind_cols(in_table, new_columns) # and add bound columns
  }
  #
  return(in_table)
}
