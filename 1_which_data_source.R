# 1_which_data_source.R
# which data source to get PMCIDs from, used by 1_extract_tables_pmc.R
# 1) my search, 2) trialstreamer, 3) hand entered to verify
# August 2021

## 1) my search
if (source == 'my_search'){
  # get the trial IDs 
  to_load = dir('data', pattern='pmid_trials')
  excluded_comb = data_comb = NULL
  for (l in to_load){
    infile = paste('data/', l, sep='')
    load(infile) # from 0_find_trials_pmc.R
    data_comb = bind_rows(data_comb, data)
    excluded_comb = bind_rows(excluded_comb, excluded) # concatenate data
  }
  data = data_comb; excluded=excluded_comb # rename back
  outfile = 'data/extracted.RData'
  exclude_non_rct = TRUE # flag to exclude non-RCT or not
}

## 2) trialstreamer
if (source == 'trialstreamer'){
  load('data/trialstreamer.RData') # from 0_read_trialstreamer.R
  data = trialstreamer
  outfile = 'data/extracted_trialstreamer.RData'
  exclude_non_rct = TRUE # flag to exclude non-RCT or not
}

## 3) validation
if (source == 'validation'){
  load('data/hand_entered_data.RData') # from 2_random_select_hand_read_checks.R
  remove(excluded, pvalues, sample_size, tables) # tidy up
  data = select(conversion, pmid, pmcid) # just IDs
  outfile = 'data/extracted_validation.RData'
  exclude_non_rct = FALSE # flag to exclude non-RCT or not, can use all data as we are just checking the tables
}
