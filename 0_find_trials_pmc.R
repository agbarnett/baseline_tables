# 0_find_trials_pmc.R
# find randomised trials with full text on PMC
# the best way to find trials is by publication type
# takes a while
# May 2021
library(dplyr)
library(rentrez)
library(tidyverse)
library(janitor)
source('0_my_pubmed_key.R')

# types of article to exclude (just trials):
source('0_publication_types.R') # for publication types
types_to_exclude = filter(pub_types, include==0) %>%
  pull(type)
types_to_include = filter(pub_types, include==1) %>%
  pull(type)

## using rentrez to get find trials on pubmed (publication type is not available on PMC)
# this just gives PMIDs
years = 2020:2021
years_search = paste(years, '[PDAT]', collapse=' OR ', sep='')
# loop through publication types
search_results = NULL
for (type in types_to_include){
  search_term = paste('(', years_search ,') AND ', type, '[PTYP]', sep='')
  trial.search <- entrez_search(db='pubmed', term = search_term, retmax=30000, api_key =my.api.key)
  this_search = data.frame(id = trial.search$ids, type=type)
  search_results = bind_rows(search_results, this_search)
}

## now specifically search for protocols and exclude
search_term_protocol = paste('(', years_search ,') AND Clinical Trial Protocol[PTYP]', sep='')
protocol.search <- entrez_search(db='pubmed', term = search_term_protocol, retmax=30000)
search_results = filter(search_results, !id %in% protocol.search$ids)

# spread publication types into binary as trials can have more than one type
search_results_wide = mutate(search_results, dummy=1) %>%
  pivot_wider(id_cols='id', names_from='type', values_from='dummy') %>%
  clean_names() %>%
  replace(is.na(.), 0) %>% # replace all NAs, so each type is 0=No, 1=Yes
  rename('pmid' = 'id')

# now get more detailed data using PMID
data = excluded = NULL
N = nrow(search_results_wide)
to_keep = c("title", "source", "articleids", 'history') # variables to keep
for (k in 610:N){ # big loop
  details = 'Did not work'
  count_try = 0
  while(class(details)[1] !='esummary'){ # keep trying until there's no error
    if(count_try > 0){Sys.sleep(60)} # sleep if not first try
    details <- tryCatch(entrez_summary(db="pubmed", id=search_results_wide$pmid[k], api_key =my.api.key),
                      error = function(e) print(paste('Did not work')))
    count_try = count_try + 1
  }
  ex = extract_from_esummary(details, elements=to_keep)
  pmc = filter(ex$articleids, idtype=='pmc')$value 
  if(any(ex$articleids$idtype == 'pmc') == FALSE){ # exclude if no PMC
    ex_frame = data.frame(pmid = search_results_wide$pmid[k])
    excluded = bind_rows(excluded, ex_frame)
  }
  if(any(ex$articleids$idtype == 'pmc') == TRUE){ # only keep if PMC
    # extract doi and date
    doi = filter(ex$articleids, idtype=='doi')$value # extract IDs
    if(length(doi) == 0 ){doi=NA}
    date = str_split(filter(ex$history, pubstatus=='pubmed')$date, ' ')[[1]][1] # use pubmed date, most consistently formatted
    #
    ex_frame = mutate(search_results_wide[k,], date = date,
                      title=ex$title, source=ex$source,
                      doi = doi, pmc = pmc)
    data = bind_rows(data, ex_frame)
  }
  if(k%%200==0){cat('Up to',k,'\r')}
}

# remove protocols based on title as well as study type is not 100% proof
data = filter(data, !str_detect(string=tolower(title), pattern='protocol'))
  
# save
save(excluded, data, file='data/pmid_trials.RData')
