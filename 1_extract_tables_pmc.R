# 1_extract_tables_pmc.R
# automatically extract the data from the baseline tables in the trials
# see http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
# use PMC ftp service https://www.ncbi.nlm.nih.gov/pmc/tools/ftp/
# June 2021
library(rvest)
library(stringr)
library(lubridate) # for dealing with multiple date formats
library(dplyr)
library(tidyverse)
library(metareadr) # to read papers from PMC
source('0_pattern.R') 
source('99_functions.R')

# get the trial IDs 
load('data/pmid_trials.RData') # from 0_find_trials_pmc.R
# Collect data in random order
data = mutate(data, runif=runif(n())) %>%
  arrange(runif) %>%
  select(-runif)

## big loop ##
N = nrow(data)
#N = 100 # temporary
table_data = pvalues = excluded = design = NULL # set up data
for (k in 1:N){
pmcid = str_remove(pattern='PMC', string=data$pmc[k]) # have to remove numbers
# get full text as web page, need try catch because some are embargoed or otherwise not available
out_nlm = NULL
out_nlm = tryCatch(mt_read_pmcoa(pmcid = pmcid, file_format = "pmc", file_name="web/full.xml"), # save to external XML file
                   error = function(e) print(paste('NLM did not work', pmcid))) # flag for error
# try Europe if no luck with NLM - not worth it, gives same result
#if(length(out_nlm) > 0){ 
#  out_europe = tryCatch(mt_read_pmcoa_europe(pmcid = pmcid, file_name="web/full.xml"), # function in 99_functions.R
#                        error = function(e) print(paste('Europe did not work', pmcid)))
#}
if(length(out_nlm) > 0){ # if neither NLM or Europe work
  this = data.frame(pmc = pmcid, reason = 'Full text page not available')
  excluded = bind_rows(excluded, this)
  next # skip to next
}

## get study title and other meta-information from full paper
webpage = read_xml("web/full.xml", encoding='UTF-8') 
title = xml_find_all(webpage, ".//article-title") %>% .[1] %>%xml_text() # tables and figures
# exclude cross-sectional studies, exploratory studies, etc (could exclude others here)
non_rct_pattern = paste( c('exploratory.study','exploratory.analysis','cross.sectional.study','cross.sectional.analysis'), collapse='|')
cross_sec = any(str_detect(tolower(title), pattern = non_rct_pattern))
if(cross_sec==TRUE){
  this = data.frame(pmc = pmcid, reason = 'Not an RCT')
  excluded = bind_rows(excluded, this)
  next # skip to next
}
# get abstract
abstract = xml_find_all(webpage, ".//abstract") %>% xml_text() # tables and figures
# exclude single arm studies
single_arm = any(str_detect(abstract, pattern='single.arm'))
if(single_arm==TRUE){
  this = data.frame(pmc = pmcid, reason = 'Single-arm study')
  excluded = bind_rows(excluded, this)
  next # skip to next
}
# search for randomised trial in title or abstract
title_abstract = paste(title, abstract, collapse=' ')
rct = str_detect(title_abstract, pattern=rct_patterns) # pattern from 0_pattern.R
# search for cluster in title or abstract
cluster = str_detect(title_abstract, pattern='cluster|Cluster') # 

## remove some sections from XML
# now remove <front> (can also get confused with labels, like <header>)
xml_find_all(webpage, ".//front") %>% xml_remove()
# remove boxed text, gets confused with tables, e.g. "Research in context"
xml_find_all(webpage, ".//boxed-text") %>% xml_remove()
# remove supplementary material as tables in here are not accessible
xml_find_all(webpage, ".//supplementary-material") %>% xml_remove()
# remove graphic, causes problems in table
xml_find_all(webpage, ".//graphic") %>% xml_remove()
# remove copyright stuff
xml_find_all(webpage, ".//permissions") %>% xml_remove()
# remove cross-references (mucked up caption comparison)
xml_find_all(webpage, ".//xref") %>% xml_remove()
## Over write XML with above changes
xml2::write_xml(webpage, file='web/full.xml', encoding = "UTF-8")

## find table captions, look just in tables
table_captions <- webpage %>%
  xml_nodes("table-wrap")  %>%
  xml_nodes("caption") %>%
  xml_text()
table_captions = tolower(table_captions) %>%
  str_remove_all("[^0-9|a-z| ]") %>% # remove all special characters to make matching below easier
  str_squish()

# are there any baseline tables based on captions?
captions_baseline = str_detect(string=tolower(table_captions), pattern=words_or) # find captions with baseline words, see 0_patterns.R
captions_negative = str_detect(string=tolower(table_captions), pattern=negative_words) # find captions with negative words
if(any(captions_negative) == TRUE){
  captions_baseline[which(captions_negative)] = FALSE # flip these captions to false
}
any_baseline_tables = any(captions_baseline)
if(any_baseline_tables == FALSE){
  this = data.frame(pmc = pmcid, reason = 'No baseline table')
  excluded = bind_rows(excluded, this)
  next # skip to next
}
## find table number
table_number = min(which(captions_baseline)) # first table

## next read XML as a text file and tidy up ...
in_text = scan(file = "web/full.xml", what='character', sep='\n', quiet=TRUE, encoding='UTF-8') # no separators, one big text
in_text = in_text[!is.na(in_text)] # remove missing
# a) replace any breaks
in_text = str_remove_all(string=in_text, pattern='<break/>') # remove breaks
# b) remove commas from large numbers, see https://stackoverflow.com/questions/67329618/replacing-commas-in-thousands-millions-but-not-smaller-numbers
in_text = gsub(",(?=\\d{3,})", "", in_text, perl = TRUE) 
# c) replace `high` decimal places used by Lancet
in_text = gsub("·", ".", in_text, perl = FALSE) 
in_text = gsub("·", ".", in_text, perl = FALSE) 
# d) replace negative signs with hyphens (else they get cut as unicode)
in_text = gsub(pattern='\u2212', replacement='-', in_text) # see https://stackoverflow.com/questions/67830110/replacing-non-ascii-dash-with-hyphen-in-r
# e) remove formatting that impacts on searching in captions
in_text = str_remove_all(string=in_text, pattern='<italic>') # 
in_text = str_remove_all(string=in_text, pattern='</italic>') # 
in_text = str_remove_all(string=in_text, pattern='<bold>') # 
in_text = str_remove_all(string=in_text, pattern='</bold>') # 
in_text = str_remove_all(string=in_text, pattern='<sup>') # 
in_text = str_remove_all(string=in_text, pattern='</sup>') # 
in_text = str_remove_all(string=in_text, pattern='<sub>') # 
in_text = str_remove_all(string=in_text, pattern='</sub>') # 
# e) convert dates to numbers
dates_index = str_detect(in_text, pattern=dates_patterns) # find places of dates
if(any(dates_index) == TRUE){
  for (location in which(dates_index)){
    places = str_locate_all(string=in_text[location], pattern=dates_patterns)[[1]] # now find specific locations in paragraphs
    dates = NULL
    for (i in 1:nrow(places)){  # first find locations
      date = str_sub(in_text[location], places[i,1], places[i,2])
      dates = c(dates, date)
    }
    # now replace (can't do together because locations get mucked up)
    for (date in unique(dates)){
      daten = as.character(as.numeric(as.Date(parse_date_time(date, orders = c('mdy', 'dmy', 'ymd'), quiet = TRUE)))) # do not flag errors
      if(is.na(daten)==FALSE){ # only replace if there's a valid date
        #cat(date,', ', daten, '\n', sep='')
        in_text[location] = str_replace_all(in_text[location], date, daten) # replace date with number
      }
    }
  }
}

## extract just the tables using the simplified table captions
# first create simple version of in_text
simple_in_text = tolower(in_text) %>%
  str_replace_all('&gt;', '>') %>% # must be a better way, but for now just removing special characters as they come!
  str_replace_all('&lt;', '<') %>%
  str_remove_all("[^0-9|a-z| ]") %>% # remove all special characters to make matching below easier
  str_squish()
tables_in_text = list()
for (j in 1:length(table_captions)){
  end = ifelse(nchar(table_captions[j])>50, 50, nchar(table_captions[j]))
  short = str_sub(table_captions[j], 1, end)
  tables_caption_index = str_detect(simple_in_text, pattern=short) #
  # find closest start of table to caption (can be multiple results on both sides)
  tables_start_index = which(str_detect(in_text, pattern='\\<table-wrap '))
  tables_start_index_closest = expand.grid(tables_start_index, which(tables_caption_index)) %>%
    mutate(diff = Var2 - Var1) %>%
    filter(diff >= 0) %>% # caption after table start
    arrange(diff) %>%
    slice(1) %>% # take smallest difference
    pull(Var1)
  # find closest end of table to caption (can be multiple results on both sides)
  tables_end_index = which(str_detect(in_text, pattern='\\</table-wrap\\>'))
  tables_end_index_closest = expand.grid(tables_end_index, which(tables_caption_index)) %>%
    mutate(diff = Var2 - Var1) %>%
    filter(diff <= 0) %>% # caption after table start
    arrange(desc(diff)) %>%
    slice(1) %>% # take smallest difference
    pull(Var1)
  #  
  to_add = tables_start_index_closest:tables_end_index_closest
  tables_caption_index[to_add] = TRUE  # add on more text as table might be split
  text_to_work_with = paste(in_text[tables_caption_index], collapse='\n')
  tables_start_index = str_locate(text_to_work_with, pattern='\\<table-wrap ') # 
  tables_end_index = str_locate(text_to_work_with, pattern='\\</table-wrap\\>') # 
  tables_in_text[[j]] = substr(text_to_work_with, tables_start_index[1], tables_end_index[2])
}
## Write just the tables to external document 
to_write = paste(in_text[1], '\n<just-tables>\n', # add top row and create overall node
                 paste(tables_in_text, collapse = '\n'),
                 '\n</just-tables>', sep='')
just_tables_xml = tryCatch(as_xml_document(to_write),
                           error = function(e) print('XML error for tables'))
xml2::write_xml(just_tables_xml, file='web/just_tables.xml', encoding = "UTF-8")

## read in just tables into R
just_tables = read_xml("web/just_tables.xml", encoding='UTF-8') 

## check table is not mostly text (usually a 'what this paper adds study')
table1 <- just_tables %>%
  xml_nodes("table") %>% # need have -wrap because of multiple tables in one
  .[table_number] %>%
  xml_text() # 
if(length(table1)>0){
    if(nchar(table1)>0){ # needed two if statements because of graphical tables
    text_count = str_count(table1, '[a-z]')
    number_count = str_count(table1, '[0-9]')
    if((number_count / text_count) < 0.1){table_number = table_number +1} # if less than 10% numbers
  }
}
if(length(table1) == 0){ # no text content in table, likely graphical table (need to check) - could add check for <graphic>
  this = data.frame(pmc = pmcid, reason = 'Graphical table')
  excluded = bind_rows(excluded, this)
  next # skip to next
}
# get table footnotes
footnotes = xml_find_all(just_tables, ".//table-wrap-foot") %>% xml_text() 
footnote = footnotes[table_number] # get the table footnote for checking p-values

## exclude if follow-up data in table
caption = table_captions[table_number] #
fu_words = c('follow.up','post.trial')
fu_pattern = paste(fu_words, collapse='|')
follow_up = str_detect(string=tolower(caption), pattern = fu_pattern)
if(follow_up == TRUE){
  this = data.frame(pmc = pmcid, reason = 'Follow-up results in baseline table')
  excluded = bind_rows(excluded, this)
  next # skip to next
}

# function that does heavy lifting:
results = baseline_table(just_tables, table_number, footnote)
table = results$table

# exclude if data could not be extracted
if(is.null(table) == TRUE){
  this = data.frame(pmc = pmcid, reason = results$reason)
  excluded = bind_rows(excluded, this)
  next # skip to next
}

## search text for key study design (exclude abstract search in main paper)
all_text = xml_find_all(webpage, ".//body") %>% xml_text() # tables and figures
# a) block randomisation 
b_search1 = str_detect(all_text, regex('block\\W+(?:\\w+ ){0,5}random', ignore_case = TRUE))
b_search2 = str_detect(all_text, regex('block\\W+(?:\\w+ ){0,5}randomi.ation', ignore_case = TRUE))
b_search3 = str_detect(all_text, regex('block\\W+(?:\\w+ ){0,5}randomi.ed', ignore_case = TRUE))
b_search4 = str_detect(all_text, regex('randomi.ation\\W+(?:\\w+ ){0,5}block', ignore_case = TRUE))
b_search5 = str_detect(all_text, regex('randomi.ed\\W+(?:\\w+ ){0,5}block', ignore_case = TRUE))
b_search6 = str_detect(all_text, regex('random\\W+(?:\\w+ ){0,5}block', ignore_case = TRUE))
block_randomisation = any(c(b_search1, b_search2, b_search3, b_search4, b_search5, b_search6))
# b) matched case-control study?
#cc_search1 = str_detect(all_text, regex('match\\W+(?:\\w+ ){0,2}case\\W+(?:\\w+ ){0,2}control', ignore_case = TRUE))
#matched_case_control = any(cc_search1)

## concatenate 
# main data
table = mutate(table, pmcid = pmcid)
table_data = bind_rows(table_data, table)
# p-values
pvalues_in_table = data.frame(pmcid=pmcid, in_table = results$pvalues_in_table) #, in_table = results$pvalues_in_text) # record p-values in table
pvalues = bind_rows(pvalues, pvalues_in_table)
# study design (starts with details from pubmed)
this_design = data[k,] %>%
  mutate(
    rct = rct,
    cluster = cluster,
    block_randomisation = block_randomisation) # record p-values in table
design = bind_rows(design, this_design)

# tidy up
file.remove('web/full.xml')
file.remove('web/just_tables.xml')

# update progress
if(k%%50 ==0 ){cat('up to ',k,'\r', sep='')}

} # end of big loop

## add final exclusions
load('data/additional_exclusions.RData') # from 1_excluded_by_hand.R
# a) add to excluded data
excluded = bind_rows(excluded, hand_exclude)
# b) knock out of data sets
table_data = filter(table_data, !pmcid %in% hand_exclude$pmc)
pvalues = filter(pvalues, !pmcid %in% hand_exclude$pmc)
design = filter(design, !pmcid %in% hand_exclude$pmc)

# add study design meta-data to excluded data
data = mutate(data, pmc = str_remove(pattern='^PMC', string=pmc)) # for merging
excluded = mutate(excluded, pmc = str_remove(pattern='^PMC', string=pmc)) # for merging
excluded = left_join(excluded, data, by='pmc')

## save
save(excluded, pvalues, table_data, design, file='data/extracted.RData')
