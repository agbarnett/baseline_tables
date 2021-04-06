# 1_extract_tables_pmc.R
# automatically extract the data from the baseline tables in the trials
# see http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
# use PMC ftp service https://www.ncbi.nlm.nih.gov/pmc/tools/ftp/
# April 2021
library(rvest)
library(stringr)
library(dplyr)
library(tidyverse)
library(metareadr) # to read papers from PMC
source('0_pattern.R') 
source('99_functions.R')

# get the trial IDs 
load('data/pmid_trials.RData') # from 0_find_trials_pmc.R

excluded = NULL

# get the web page
k = 34
pmcid = str_remove(pattern='PMC', string=data$pmc[k]) # have to remove numbers
# get full text as web page, need try catch because some are embargoed
out = tryCatch(mt_read_pmcoa(pmcid = pmcid, file_name="web/full.xml"), # save to external XML file 
         error = function(e) print('did not work')) # flag for error
if(length(out) > 0){
  this = data.frame(pmc = data$pmc[k], reason = 'Page not available')
  excluded = bind_rows(excluded, this)
  next # skip to next
}
# next read as a text file and replace any breaks
in_text = scan(file = "web/full.xml", what='character', sep='\n') # no separators, one big text
in_text = str_remove_all(string=in_text, pattern='<break/>') # remove breaks
write(in_text, file='web/full.xml', sep='') # write altered text to external file
# now read into R if page is available 
webpage = read_xml("web/full.xml", encoding='UTF-8') 

# get abstract
abstract = xml_find_all(webpage, ".//abstract") %>% xml_text() # tables and figures
# exclude single arm studies
single_arm = str_detect(abstract, pattern='single.arm')
if(single_arm==TRUE){
  this = data.frame(pmc = data$pmc[k], reason = 'Single-arm study')
  excluded = bind_rows(excluded, this)
  next # skip to next
}

# find table titles
labels = xml_find_all(webpage, ".//label") %>% xml_text() # tables and figures
labels = labels[str_detect(tolower(labels), pattern='fig|tab')] # order of tables and figures
# any tables
any_tables = any(str_detect(tolower(labels), '^tab'))
if(any_tables == FALSE){
  this = data.frame(pmc = data$pmc[k], reason = 'No tables')
  excluded = bind_rows(excluded, this)
  next # skip to next
}

## get table/figure captions:
captions = xml_find_all(webpage, ".//caption") %>% xml_text() # tables and figures
# remove captions for supplement, appendix, additional, etc
captions = captions[!str_detect(tolower(captions), pattern='^additional|^appendix|^supplement')]
# footnotes = xml_find_all(webpage, ".//table-wrap-foot") %>% xml_text() # ... could be useful?
# any baseline tables?
words_defined_baseline = c('baseline','characteristic','demographic','basic information')
words_or = paste(words_defined_baseline, collapse='|')
captions_baseline = str_detect(string=tolower(captions), pattern=words_or)
any_baseline_tables = any(captions_baseline)
if(any_baseline_tables == FALSE){
  this = data.frame(pmc = data$pmc[k], reason = 'No baseline table')
  excluded = bind_rows(excluded, this)
  next # skip to next
}
# find table number
table_number = min(which(captions_baseline)) # first table
to_subtract = sum(str_detect(string = tolower(labels[1:table_number]), pattern='^fig'))
table_number = table_number - to_subtract # subtract any figures from table number
           
# function that does heavy lifting:
table = baseline_table(webpage, table_number)

# exclude if data could not be extracted
if(is.null(table) == TRUE){
  this = data.frame(pmc = data$pmc[k], reason = 'Table data could not be extracted')
  excluded = bind_rows(excluded, this)
  next # skip to next
}

# search for block randomisation in text
all_text = xml_text(webpage)
b_search1 = str_detect(all_text, regex('block\\W+(?:\\w+ ){0,5}random', ignore_case = TRUE))
b_search2 = str_detect(all_text, regex('block\\W+(?:\\w+ ){0,5}randomi.ation', ignore_case = TRUE))
b_search3 = str_detect(all_text, regex('block\\W+(?:\\w+ ){0,5}randomi.ed', ignore_case = TRUE))
b_search4 = str_detect(all_text, regex('randomi.ation\\W+(?:\\w+ ){0,5}block', ignore_case = TRUE))
b_search5 = str_detect(all_text, regex('randomi.ed\\W+(?:\\w+ ){0,5}block', ignore_case = TRUE))
b_search6 = str_detect(all_text, regex('random\\W+(?:\\w+ ){0,5}block', ignore_case = TRUE))
block_randomisation = any(c(b_search1, b_search2, b_search3, b_search4, b_search5, b_search6))

