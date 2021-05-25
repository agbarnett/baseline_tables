# 1_extract_tables_pmc.R
# automatically extract the data from the baseline tables in the trials
# see http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
# use PMC ftp service https://www.ncbi.nlm.nih.gov/pmc/tools/ftp/
# May 2021
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
N = 100 # temporary
table_data = pvalues = excluded = design = NULL # set up data
for (k in 1:N){
pmcid = str_remove(pattern='PMC', string=data$pmc[k]) # have to remove numbers
# get full text as web page, need try catch because some are embargoed or otherwise not available
out_nlm = out_europe = NULL
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

## get study title
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

## next read as a text file and ...
in_text = scan(file = "web/full.xml", what='character', sep='\n', quiet=TRUE, encoding='UTF-8') # no separators, one big text
in_text = in_text[!is.na(in_text)] # remove missing
# a) replace any breaks
in_text = str_remove_all(string=in_text, pattern='<break/>') # remove breaks
# b) remove commas from large numbers, see https://stackoverflow.com/questions/67329618/replacing-commas-in-thousands-millions-but-not-smaller-numbers
in_text = gsub(",(?=\\d{3,})", "", in_text, perl = TRUE) 
# c) remove `high` decimal places used by Lancet
in_text = gsub("·", ".", in_text, perl = FALSE) 
in_text = gsub("·", ".", in_text, perl = FALSE) 
# d) convert dates to numbers
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

# write altered text to external file with encoding 
as_xml = as_xml_document(paste(in_text, collapse = '\n'))
xml2::write_xml(as_xml, file='web/full.xml', encoding = "UTF-8")
# now read into R if page is available 
webpage = read_xml("web/full.xml", encoding='UTF-8') 
# remove <header> (can get confused with labels)
xml_find_all(webpage, ".//header") %>% xml_remove()

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

# now remove <front> (can also get confused with labels, like <header>)
xml_find_all(webpage, ".//front") %>% xml_remove()
# remove boxed text, gets confused with tables, e.g. "Research in context"
xml_find_all(webpage, ".//boxed-text") %>% xml_remove()
# remove supplementary material as tables in here are not accessible
xml_find_all(webpage, ".//supplementary-material") %>% xml_remove()

# find table/figure labels
labels = xml_find_all(webpage, ".//label") %>% xml_text() # tables and figures
labels = labels[str_detect(tolower(labels), pattern='fig|tab')] # order of tables and figures
# any tables
any_tables = any(str_detect(tolower(labels), '^tab'))
if(any_tables == FALSE){
  this = data.frame(pmc = pmcid, reason = 'No tables')
  excluded = bind_rows(excluded, this)
  next # skip to next
}

## get table/figure captions:
captions = xml_find_all(webpage, ".//caption") %>% xml_text() # tables and figures, does include supplement
# remove captions for figures (assumes labels are only for figures and tables)
figures = str_detect(string = tolower(labels), pattern='^fig')
#tables = str_detect(string = tolower(labels), pattern='^tab')
captions = captions[!figures]
# remove captions for supplement, appendix, additional, etc
captions = captions[!str_detect(tolower(captions), pattern='^additional|^appendix|^supplement|additional data file|response to reviewer')]
footnotes = xml_find_all(webpage, ".//table-wrap-foot") %>% xml_text() # ... could be useful?
# are there any baseline tables?
words_defined_baseline = c('baseline','characteristic','demographic','basic information','before starting the study')
words_or = paste(words_defined_baseline, collapse='|')
negative_words = c('change from baseline', 'baseline adjusted') # words that rule out a baseline table
negative_words = paste(negative_words, collapse='|')
captions_baseline = str_detect(string=tolower(captions), pattern=words_or) # find captions with baseline words
captions_negative = str_detect(string=tolower(captions), pattern=negative_words) # find captions with negative words
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

## check table is not mostly text (usually a 'what this paper adds study')
table1 <- webpage %>%
  xml_nodes("table-wrap") %>% # need have -wrap because of multiple tables in one
  .[table_number] %>%
  xml_text() # 
if(length(table1)>0){
    if(nchar(table1)>0){ # needed two if statements because of graphical tables
    text_count = str_count(table1, '[a-z]')
    number_count = str_count(table1, '[0-9]')
    if((number_count / text_count) < 0.1){table_number = table_number +1} # if less than 10% numbers
  }
}
footnote = footnotes[table_number] # get the table footnote for checking p-values

## exclude if follow-up data in table
caption = captions[table_number]
fu_words = c('follow.up','post.trial')
fu_pattern = paste(fu_words, collapse='|')
follow_up = str_detect(string=tolower(caption), pattern = fu_pattern)
if(follow_up == TRUE){
  this = data.frame(pmc = pmcid, reason = 'Follow-up results in baseline table')
  excluded = bind_rows(excluded, this)
  next # skip to next
}

# function that does heavy lifting:
results = baseline_table(webpage, table_number, footnote)
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
pvalues_in_table = data.frame(pmcid=pmcid, in_table = results$pvalues_in_table) # record p-values in table
pvalues = bind_rows(pvalues, pvalues_in_table)
# study design (starts with details from pubmed)
this_design = data[k,] %>%
  mutate(
    rct = rct,
    cluster = cluster,
    block_randomisation = block_randomisation) # record p-values in table
design = bind_rows(design, this_design)

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
excluded = left_join(excluded, data, by='pmc')

## save
save(excluded, pvalues, table_data, design, file='data/extracted.RData')
