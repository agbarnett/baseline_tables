# 0_read_trialstreamer.R
# read in the ID data from the trialstreamer web page
# August 2021
library(janitor)
library(dplyr)
library(readxl)
library(stringr)
library(XML) # for API

# get all the csv files on the trialstreamer data webpage
page = "https://zenodo.org/record/5153084"
destfile = 'data/trialstreamer_page.txt'
download.file(page, destfile)
text = as.character(read.table(destfile, header=FALSE, sep='~'))
csv = str_locate_all(string=text, pattern='\\.csv')[[1]][,2] # just use the ends
href = str_locate_all(string=text, pattern='href=')[[1]][,2] # just use the ends
matches = expand.grid(href, csv) %>% # get all combinations
  mutate(diff = Var2 - Var1) %>%
  filter(diff > 0) %>% # must be positive length string
  group_by(Var1) %>%
  arrange(Var1, diff) %>%
  slice(1) %>% # get closest match 
  ungroup() %>%
  filter(diff < 200) # assume long matches are not files
# now loop through to find csv files
csv_files = NULL
for (k in 1:nrow(matches)){
  f = str_sub(text, matches$Var1[k]+1, matches$Var2[k]) # +1 to remove equals
  if(str_detect(f, '^https') == TRUE){csv_files = c(csv_files, f)} # only full keep addresses
}

# now loop through CSV files and download from the web
destfile = 'data/temp.csv'
all = NULL
for (this_file in csv_files){
  #
  download.file(this_file, destfile)
  # get number randomised from trialstreamer
  this_in = read.csv(destfile, stringsAsFactors = FALSE) %>%
    clean_names()
  # rename for consistency
  if(any(names(this_in) == 'dois')){
    index = names(this_in) == 'dois'
    names(this_in)[index] = 'doi'
  }
  if(nrow(this_in) > 0){
    this_in = select(this_file, pmid, doi,  num_randomized)
    all = bind_rows(all, this_in)
  }
}

# remove any duplicates
trialstreamer = unique(all)

## add pmcid (pubmed central IDs); see https://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
base_url = 'https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/'
all_ids = trialstreamer$pmid
# run in batches
batch_size = 100
max_batch = floor(length(all_ids) / batch_size)
conversion = NULL
for (loop in 2:max_batch){
  start = (loop-1)*batch_size + 1
  stop = min(loop*batch_size, length(all_ids))
  ids = all_ids[start:stop]
  ids = paste(ids, collapse=',') # Use commas to separate multiple ID
  request = paste(base_url, '?tool=R&email=a.barnett@qut.edu.au&ids=', ids, sep='')
  destfile = 'data/xml.txt'
  download.file(request, destfile)
  xml = xmlParse(destfile) # parse the XML
  list = xmlSApply(xml["//record"], xmlAttrs) # convert to a list
  for (s in 1:length(list)){ # loop through the list
    this = list[[s]]
    d = data.frame(t(this))
    conversion = bind_rows(conversion, d)
  }
} # end of batch loop
# tidy up ID data:
conversion = filter(conversion, !is.na(pmcid)) %>% # remove missing PMC
  select(pmid, pmcid) %>% # slim down variables
  mutate(pmid = as.integer(pmid)) # for merge

## add pmcid to trialstreamer
to_export = left_join(trialstreamer, conversion, by='pmid') %>%
  filter(!is.na(pmcid)) %>% # must have PMC so I can read in the table
  rename('pmc' = 'pmcid') # to match other data

# save
trialstreamer = to_export
date_downloaded = Sys.Date()
n_files = length(csv_files)
save(date_downloaded, n_files, trialstreamer, file='data/trialstreamer.RData')
