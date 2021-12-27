# 1_extract_tables_pmc.R
# automatically extract the data from the baseline tables in the trials from pubmed central
# see http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
# use PMC ftp service https://www.ncbi.nlm.nih.gov/pmc/tools/ftp/
# August 2021
library(rvest)
library(stringr)
library(stringi)
library(lubridate) # for dealing with multiple date formats
library(dplyr)
library(tidyverse)
library(magrittr)
library(xml2)
library(metareadr) # to read papers from PMC; installed from github
source('1_pattern.R') 
source('99_functions.R')
load('data/emails_and_states.RData') # from 0_country_email.R, used for affiliations
  
# select which source of data to use
sources = c('my_search', 'trialstreamer', 'validation')
source = sources[3]
stage = 'data'
source('1_which_data_source.R') # uses `source` and `stage`

#  set up data
table_data = errors = pvals = pvalues = excluded = design = full_list = NULL #

# exclude some papers prior to running algorithm
load('data/additional_exclusions.RData') # from 1_excluded_by_hand.R
hand_exclude = filter(hand_exclude, pmcid %in% data$pmcid) # any exclusions in this data?
if(nrow(hand_exclude) > 0){
  data = filter(data, !pmcid %in% hand_exclude$pmcid)
  excluded = bind_rows(excluded, hand_exclude)
}

## big loop ##
N = 10000 # final processing size
N = min(N, nrow(data))
restart = 1
for (k in restart:N){
  pmcid = data$pmcid[k] # 
  full_list = bind_rows(full_list, data.frame(num=k, pmcid=pmcid)) # useful to have a complete list of all PMCIDs
  pmcid_function = str_remove(pattern='PMC', string=pmcid) # have to remove numbers for function
# get full text as web page, need try catch because some are embargoed or otherwise not available
out_nlm = NULL
out_nlm = tryCatch(mt_read_pmcoa(pmcid = pmcid_function, file_format = "pmc", file_name="web/full.xml"), # save to external XML file
                   error = function(e) print(paste('NLM did not work', pmcid))) # flag for error
if(length(out_nlm) > 0){ # if neither NLM (national library medicine) or Europe work
  this = data.frame(pmcid = pmcid, reason = 'Full text page not available')
  excluded = bind_rows(excluded, this)
  next # skip to next
}

## get study title and other meta-information from full paper
webpage = read_xml("web/full.xml", encoding='UTF-8') 
title = xml_find_all(webpage, ".//article-title") %>% .[1] %>%xml_text() # tables and figures
# get abstract
abstract = xml_find_all(webpage, ".//abstract") %>% xml_text() # tables and figures
# exclude single arm studies
single_arm = any(str_detect(abstract, pattern='single.arm|singlearm'))
if(single_arm==TRUE){
  this = data.frame(pmcid = pmcid, reason = 'Single-arm study')
  excluded = bind_rows(excluded, this)
  next # skip to next
}
# search for randomised trial in title or abstract
title_abstract = paste(title, abstract, collapse=' ')
rct = str_detect(title_abstract, pattern=rct_patterns) # pattern from 1_pattern.R
# search for cluster in title or abstract
cluster = str_detect(title_abstract, pattern='\\bcluster|\\bCluster') # 
# exclude cross-sectional studies, exploratory studies, pooled analysis, etc (could exclude others here)
cross_sec_title = any(str_detect(tolower(title), pattern = non_rct_pattern_title))
cross_sec_abstract = any(str_detect(tolower(abstract), pattern = non_rct_pattern_abstract))
if((cross_sec_title ==TRUE | cross_sec_abstract == TRUE) & exclude_non_rct==TRUE){ # exclude_non_rct flag from 1_which_data_source.R
  this = data.frame(pmcid = pmcid, reason = 'Not an RCT')
  excluded = bind_rows(excluded, this)
  next # skip to next
}
# exclude pre-post studies, must have pre and post, turned off for now, use table to detect pre-post
turned_off = function(){
  pre = any(str_detect(tolower(title_abstract), pattern = 'pre-'))
  post = any(str_detect(tolower(title_abstract), pattern = 'post-'))
  if(pre==TRUE & post==TRUE){
    this = data.frame(pmcid = pmcid, reason = 'Not an RCT')
    excluded = bind_rows(excluded, this)
    next # skip to next
  }
}

# get the first author affiliation
affiliation = get_affiliation(webpage)

## get the journal
journal = xml_find_all(webpage, ".//journal-meta//journal-title-group//journal-title") %>% xml_text()
if(length(journal) == 0){
  journal = xml_find_all(webpage, ".//journal-meta//journal-title") %>% xml_text()
}

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
## remove cross-references (mucked up caption comparison) but keep text
# had to change to text, see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6660508/
# see https://stackoverflow.com/questions/68616875/using-xml-replace-leaves-behind-some-formatting?noredirect=1#comment121326430_68616875
page_char <- as.character(webpage)
page_remove_xref <- gsub("<xref[^>]*>|<\\/xref>", " ", page_char)
page_add_breaks <- gsub("<table-wrap", "\n<table-wrap", page_remove_xref) # add breaks before tables, helps with p-value in text by making sure table is separate to paragraph
# replace breaks with spaces (sometimes used in tables)
page_add_breaks <- gsub("<break\\/>|<break>", " ", page_add_breaks)
# replace badly formatted plus/minus, e.g., PMC7607674
page_add_breaks <- gsub("<underline>\\+<\\/underline>", "±", page_add_breaks)
# replace `n1=` with `n=`, e.g. PMC6731465
page_add_breaks <- gsub("\\bn\\s?[0-9]=", "n=", page_add_breaks)
# remove colspan, causes havoc in tables with labels; but removing it caused problems too, so commented out
#page_add_breaks = str_replace_all(pattern='colspan="."', replacement = 'colspan="1"', string=page_add_breaks)
webpage <- read_xml(page_add_breaks)

## remove paragraph returns in captions (see https://stackoverflow.com/questions/67864686/xml-in-r-remove-paragraphs-but-keep-xml-class/67865367#67865367)
# find the caption
caption <- xml_find_all(webpage, './/caption')
if(length(caption)>0){
  # store existing text
  replacement = rep('', length(caption))
  for (q in 1:length(caption)){
    any_p = xml_find_all(caption[[q]], './/p') # are there paragraphs
    if(length(any_p)>0){
      with_p = caption[[q]] %>% xml_find_all( './/p') %>% xml_text() %>% paste(collapse = " ")
      without_p = caption[[q]] %>% xml_find_all( './/title') %>% xml_text() %>% paste(collapse = " ")
      replacement[q] <- paste(without_p, with_p) # need both because some first headers have no <p>
    }
    if(length(any_p) == 0){
      replacement[q] <- caption[[q]] %>% xml_text() %>% paste(collapse = " ")
    }
    replacement[q] = gsub(replacement[q], pattern='\n', replacement = ' ') # remove other style of breaks
  }
  # remove the desired text
  caption %>% xml_find_all( './/p') %>% xml_remove()
  #replace the caption
  xml_text(caption) <- replacement
}
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
captions_baseline = str_detect(string=tolower(table_captions), pattern=words_or) # find captions with baseline words, see 1_patterns.R
captions_negative = str_detect(string=tolower(table_captions), pattern=negative_words) # find captions with negative words
if(any(captions_negative) == TRUE){
  captions_baseline[which(captions_negative)] = FALSE # flip these captions to false
}
any_baseline_tables = any(captions_baseline)
if(any_baseline_tables == FALSE){
  this = data.frame(pmcid = pmcid, reason = 'No baseline table')
  excluded = bind_rows(excluded, this)
  next # skip to next
}
## find table number for baseline table
if(sum(captions_baseline) == 1){
  table_number = min(which(captions_baseline)) 
}
if(sum(captions_baseline) > 1){
  base_count = str_count(string=tolower(table_captions), pattern=words_or)
  table_number = which(base_count == max(base_count))[1] # assume first table if there is a tie 
}

## next read XML as a text file and tidy up ...
in_text = scan(file = "web/full.xml", what='character', sep='\n', quiet=TRUE, encoding='UTF-8') # no separators, one big text
in_text = in_text[!is.na(in_text)] # remove missing
# a) replace any breaks
in_text = str_remove_all(string=in_text, pattern='<break/>') # remove breaks
# b) remove commas, spaces and hyphens from or between numbers, see https://stackoverflow.com/questions/67329618/replacing-commas-in-thousands-millions-but-not-smaller-numbers
in_text = gsub("(?<=\\(\\d{1}),(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # change badly formatted numbers, e.g. "(87,105)" that is actually two numbers -- must be in brackets
in_text = gsub("(?<=\\(\\d{2}),(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # same as above with larger number, have to increment because of `look-backwards`
in_text = gsub("(?<=\\(\\d{3}),(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # same as above with larger number
in_text = gsub("(?<=\\(\\d{4}),(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # same as above with larger number
in_text = gsub("(?<=\\(\\d{5}),(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # same as above with larger number
in_text = gsub("(?<=\\(\\d{1})-(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # as above for hyphen
in_text = gsub("(?<=\\(\\d{2})-(?=\\d{1,}\\))", ' ', in_text, perl=TRUE)
in_text = gsub("(?<=\\(\\d{3})-(?=\\d{1,}\\))", ' ', in_text, perl=TRUE)
in_text = gsub("(?<=\\(\\d{4})-(?=\\d{1,}\\))", ' ', in_text, perl=TRUE)
in_text = gsub("(?<=\\(\\d{5})-(?=\\d{1,}\\))", ' ', in_text, perl=TRUE)
in_text = gsub(",(?=\\d{3,})", "", in_text, perl = TRUE) # strip commas in numbers
in_text = gsub("(?<=\\d{1})\\W(?=000)", '', in_text, perl=TRUE) # strip spaces (including special spaces) in thousands (space before 000). \W is anything that isn't a letter, digit, or an underscore
# c) replace `high` decimal places used by Lancet
in_text = gsub("·", ".", in_text, perl = FALSE) 
in_text = gsub("·", ".", in_text, perl = FALSE) 
# d) replace negative signs with hyphens (else they get cut as unicode)
in_text = gsub(pattern='\u2212', replacement='-', in_text) # see https://stackoverflow.com/questions/67830110/replacing-non-ascii-dash-with-hyphen-in-r
# e) replace N/A as slash gets confused with number
in_text = gsub(pattern='N/A|n/a', replacement=' ', in_text) #
# f) remove formatting that impacts on searching in captions
in_text = str_remove_all(string=in_text, pattern=' toggle="yes"') # appears in italic
in_text = str_remove_all(string=in_text, pattern='<italic>') # 
in_text = str_remove_all(string=in_text, pattern='</italic>') # 
in_text = str_remove_all(string=in_text, pattern='<bold>') # 
in_text = str_remove_all(string=in_text, pattern='</bold>') # 
in_text = str_remove_all(string=in_text, pattern='<sup>') # 
in_text = str_remove_all(string=in_text, pattern='</sup>') # 
in_text = str_remove_all(string=in_text, pattern='<sub>') # 
in_text = str_remove_all(string=in_text, pattern='</sub>') # 
# g) convert dates to numbers (so they can be processed as summary stats)
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
# h) remove hyphens that are not negative signs
in_text = gsub("-\\W(?=\\d{1})", " ", in_text, perl = TRUE) # things like "22- 33"; hyphen, any non-numeric/letter char (\W), then number
# could also remove "numbers-numbers" here?

## extract just the tables using the simplified table captions
# first create simple version of in_text
#in_text= in_text %>% str_replace_all(pattern='&lt;', replacement = '<') # not working
simple_in_text = tolower(in_text) %>%
  str_replace_all('&amp;', '&') %>% 
  str_replace_all('&gt;', '>') %>% # must be a better way, but for now just removing special characters as they come!
  str_replace_all('&lt;', '<') %>%
  str_remove_all("[^0-9|a-z| ]") %>% # remove all special characters to make matching below easier
  str_squish()
tables_in_text = list()
for (j in which(captions_baseline)){ # just export tables that are possible baseline tables
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
just_tables_xml = tryCatch(as_xml_document(read_html(to_write)),
                           error = function(e) print('XML error for tables'))
xml2::write_xml(just_tables_xml, file='web/just_tables.xml', encoding = "UTF-8")

## read in just tables into R
just_tables = read_html("web/just_tables.xml", encoding='UTF-8') 
## check table is not mostly text (usually a 'what this paper adds study')
old_table_number = table_number # needed for text search for p-values
table_number = exclude_text_tables(just_tables, table_number=1) # restart table number at one because non-baseline tables have been dropped
if(table_number == 998 | table_number == 999){ # function above also checks for availability
  this = data.frame(pmcid = pmcid, reason = 'No baseline table')
  excluded = bind_rows(excluded, this)
  next
}
# update old table number used for searching text
old_table_number = old_table_number + (table_number-1)
# get table footnotes
tabs = xml_find_all(just_tables, ".//table-wrap")
footnotes = rep('', length(tabs))
for (l in 1:length(tabs)){ # need to loop through tables because some tables have no footnotes
  any_foot = xml_find_all(tabs[[l]], ".//table-wrap-foot") %>% xml_text() 
  if(length(any_foot)>0){footnotes[l] = paste(any_foot, collapse=' ')}
}
footnote = footnotes[table_number] # get the table footnote for checking p-values

## exclude if follow-up data in table
table_captions = table_captions[which(captions_baseline)] # just keep baseline tables
caption = table_captions[table_number] #
follow_up = str_detect(string=tolower(caption), pattern = fu_pattern)
if(follow_up == TRUE){
  this = data.frame(pmcid = pmcid, reason = 'Follow-up results in baseline table')
  excluded = bind_rows(excluded, this)
  next # skip to next
}

# function that does heavy lifting, trial and error with weight:
results = tryCatch(baseline_table(webpage = just_tables, pmcid=pmcid, table_number, footnote, weight=2), # 
                   error = function(e) print(paste('Function not working for', pmcid))) # flag for error
if(class(results) == 'character'){
  errors = c(errors, pmcid)
  next #skip to next if there's an error
}
table = results$table

## check for p-values in text in the results section
# find paragraph that mentions table (using `cross` as I added this to avoid confusion with captions)
table_text_arabic = paste(c('tab','table','tabs','tables'), old_table_number, sep='.')
table_text_roman = paste(c('tab','table','tabs','tables'), tolower(as.roman(old_table_number)), sep='.')
table_text = c(table_text_arabic, table_text_roman) # use both roman and arabic numbers
table_search =  paste('\\b', paste(table_text, collapse='\\b|\\b'), '\\b', sep='') # just search for whole words
index = grep(pattern=table_search, tolower(str_squish(in_text))) # str squish so that tables in text work after xref removal
results_index = grep(pattern='results<\\/title>|<title>results', tolower(str_squish(in_text))) # find results heading
to_remove = grep(pattern='table-wrap|<label>|supplementary table|supplement table|appendix table', tolower(in_text)) # remove tables, lables and supplement tables
index = setdiff(index, to_remove) # 
if(length(results_index)>0){index = index[index>=results_index]} # must come after results heading
pvalues_in_text = NA
if(length(index)>0){
  paragraph = in_text[index]
  pvalues_in_text = any(str_detect(paragraph, pval_pattern))
}

# record p-values in table; this can be valid even where table could not be extracted because of sample size problems
add_pval = FALSE
if(is.null(results$reason) == FALSE){
  if(results$reason %in% c('No sample size')){
    add_pval = TRUE
  }
}
# if not excluded, or if results have been added then add p-value
if(is.null(results$reason) == TRUE | is.null(results$pvalues_in_table) == FALSE){
  add_pval = TRUE
}
if(add_pval == TRUE){
  pvalues_in_table_text = data.frame(pmcid=pmcid, in_table = results$pvalues_in_table, in_text = pvalues_in_text) # record p-values in table
  pvalues = bind_rows(pvalues, pvalues_in_table_text)
  if(any(names(results) == 'pvalues') == TRUE){
    this_pvals = mutate(results$pvalues, pmcid=pmcid) # add PMCID
    pvals = bind_rows(pvals, this_pvals)
  }
}

# exclude from table analysis if data could not be extracted
if(is.null(results$reason) == FALSE){
  this = data.frame(pmcid = pmcid, reason = results$reason)
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
# b) adaptive randomisation
a_search1 = str_detect(all_text, regex('adaptive.randomi.ation', ignore_case = TRUE))
adaptive_randomisation = any(a_search1)
# c) matched case-control study?
#cc_search1 = str_detect(all_text, regex('match\\W+(?:\\w+ ){0,2}case\\W+(?:\\w+ ){0,2}control', ignore_case = TRUE))
#matched_case_control = any(cc_search1)
# c) search for standard error
to_search = paste(tolower(footnote), paste(table1, collapse = ' '), sep= ' ') # put all text together
sem = str_detect(string=to_search, pattern=sem_patterns)
  
## concatenate 
# main data
pilot = str_detect(tolower(title), pattern='\\bpilot\\b')
table = mutate(table, pmcid = pmcid, pmid = data$pmid[k]) # pmid for merge with trialstreamer
table_data = bind_rows(table_data, table)
# study design (starts with details from pubmed)
this_design = data[k,] %>%
  mutate(
    affiliation = affiliation,
    journal = journal,
    rct = rct,
    cluster = cluster,
    pilot = pilot,
    block_randomisation = block_randomisation,
    adaptive_randomisation = adaptive_randomisation, 
    sem = sem) # add meta-data
design = bind_rows(design, this_design)

# tidy up
file.remove('web/full.xml')
file.remove('web/just_tables.xml')

# update progress
if(k%%50 ==0 ){cat('up to ',k,'\r', sep='')}

} # end of big loop

## remove any duplicates, possible because of restarts
table_data = unique(table_data)

# add study design meta-data to excluded data
excluded = left_join(excluded, data, by='pmcid')

## save
save(excluded, errors, full_list, pvals, pvalues, table_data, design, file=outfile) # outfile from 1_which_data_source.R
