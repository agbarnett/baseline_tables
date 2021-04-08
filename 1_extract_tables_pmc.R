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
# temporary
data = mutate(data, runif=runif(n())) %>%
  arrange(runif) %>%
  select(-runif)

## big loop ##
table_data = pvalues = excluded = design = NULL # set up data
N = nrow(data)
N = 100 # temporary
for (k in 1:N){
pmcid = str_remove(pattern='PMC', string=data$pmc[k]) # have to remove numbers
# get full text as web page, need try catch because some are embargoed
out = tryCatch(mt_read_pmcoa(pmcid = pmcid, file_name="web/full.xml"), # save to external XML file 
         error = function(e) print('did not work')) # flag for error
if(length(out) > 0){
  this = data.frame(pmc = pmcid, reason = 'Page not available')
  excluded = bind_rows(excluded, this)
  next # skip to next
}
# next read as a text file and replace any breaks
in_text = scan(file = "web/full.xml", what='character', sep='\n', quiet=TRUE) # no separators, one big text
in_text = str_remove_all(string=in_text, pattern='<break/>') # remove breaks
write(in_text, file='web/full.xml', sep='') # write altered text to external file
# now read into R if page is available 
webpage = read_xml("web/full.xml", encoding='UTF-8') 

# get abstract
abstract = xml_find_all(webpage, ".//abstract") %>% xml_text() # tables and figures
# exclude single arm studies
single_arm = any(str_detect(abstract, pattern='single.arm'))
if(single_arm==TRUE){
  this = data.frame(pmc = pmcid, reason = 'Single-arm study')
  excluded = bind_rows(excluded, this)
  next # skip to next
}

# find table titles
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
# remove captions for figures
figures = str_detect(string = tolower(labels), pattern='^fig')
captions = captions[!figures]
# remove captions for supplement, appendix, additional, etc
captions = captions[!str_detect(tolower(captions), pattern='^additional|^appendix|^supplement|additional data file|response to reviewer')]
# footnotes = xml_find_all(webpage, ".//table-wrap-foot") %>% xml_text() # ... could be useful?
# are there any baseline tables?
words_defined_baseline = c('baseline','characteristic','demographic','basic information')
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
# find table number
table_number = min(which(captions_baseline)) # first table

# function that does heavy lifting:
results = baseline_table(webpage, table_number)
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
# study design (uses details from pubmed)
this_design = data[k,] %>%
  mutate(block_randomisation = block_randomisation) # record p-values in table
design = bind_rows(design, this_design)

# update progress
if(k%%50 ==0 ){cat('up to ',k,'\r', sep='')}

}

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
