# 1_which_data_source.R
# which data source to get PMCIDs from, used by 1_extract_tables_pmc.R
# 1) my search, 2) trialstreamer, 3) hand entered to verify, 4) simulated data sets
# November 2021

### Section 1: data stage ###

if(stage == 'data'){

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
  load('data/hand_entered_data.RData') # from 2_read_random_select_hand_checks.R
  remove(excluded, pvalues, tables) # tidy up
  data = select(ids, pmcid) %>% # just IDs
    mutate(pmid=999) # no pmid so add dummy (needed for merging; pmid needed for trialstreamer)
  outfile = 'data/extracted_validation.RData'
  exclude_non_rct = FALSE # flag to exclude non-RCT or not, can use all data as we are just checking the tables
}

}

### Section 2: processing (do not need to process simulated data) ###

if(stage == 'processing'){
  if (source == 'my_search' ){
    infile = 'extracted'
    outfile = 'analysis_ready'
  }
  if (source == 'trialstreamer' ){
    infile = 'extracted_trialstreamer' # from 1_extract_tables_pmc.R
    outfile = 'analysis_ready_trialstreamer'
  }
  if (source == 'validation' ){
    infile = 'extracted_validation'
    outfile = 'analysis_ready_validation'
  }
  if (source == 'carlisle' ){
    infile = 'carlisle' # from 0_read_carlisle.R
    outfile = 'analysis_ready_carlisle'
  }
  if (source == 'saitoh' ){
    infile = 'saitoh' # from 0_read_saitoh.R
    outfile = 'analysis_ready_saitoh'
  }
  infile = paste('data/', infile, '.RData', sep='')
  outfile = paste('data/', outfile, '.RData', sep='')
  load(infile) 
}


### Section 3: model stage ###

if(stage == 'model'){
  
  if(source == 'my_search'){
    infile = 'data/analysis_ready.RData' # from 2_process_extracted_data.R
    outfile = 'results/bugs_real.RData'
  }
  if(source == 'trialstreamer'){
    infile = 'data/analysis_ready_trialstreamer.RData' # from 2_process_extracted_data.R
    outfile = 'results/bugs_real_trialstreamer.RData'
  }
  if(source == 'validation'){
    infile = 'data/analysis_ready_validation.RData' # from 2_process_extracted_data.R
    outfile = 'results/bugs_real_validation.RData'
  }
  if(source == 'simulation'){
    infile = 'data/simulated_data.RData' # from 0_simulated_data.R
    outfile = 'results/bugs_real_simulation.RData'
  }
  if(source == 'simulation_two'){
    infile = 'data/simulated_data_two.RData' # from 0b_simulate_data.R
    outfile = 'results/bugs_real_simulation_two.RData'
  }
  if(source == 'simulation_three'){
    infile = 'data/simulated_data_three.RData' # from 0b_simulate_data.R
    outfile = 'results/bugs_real_simulation_three.RData'
  }
  if(source == 'bland'){
    infile = 'data/simulated_data_bland.RData' # from 0_simulated_data_bland.R
    outfile = 'results/bugs_real_bland.RData'
  }
  if(source == 'carlisle'){
    infile = 'data/analysis_ready_carlisle.RData' # 2_process_extracted_data.R
    outfile = 'results/bugs_real_carlisle.RData'
  }
  if(source == 'saitoh'){
    infile = 'data/analysis_ready_saitoh.RData' # 2_process_extracted_data.R
    outfile = 'results/bugs_real_saitoh.RData'
  }
  load(infile) 
  
}

### Section 4: plot ###

if(stage == 'plot'){
  
  if(source == 'my_search'){
    infile = 'results/bugs_real.RData' # from 4_model.R
    outjpg = 'figures/flagged_papers.jpg'
    outfile = '5_summary_results_my.docx'
    tree_outfile = 'data/tree_ready.RData'
    results_outfile = 'results/uniform_real.RData'
    simulation = FALSE
  }
  if(source == 'trialstreamer'){
    infile = 'results/bugs_real_trialstreamer.RData' # from 4_model.R
    outjpg = 'figures/flagged_papers_trialstreamer.jpg'
    outfile = '5_summary_results_trialstreamer.docx'
    tree_outfile = 'data/tree_ready_trialstreamer.RData'
    results_outfile = 'results/uniform_trialstreamer.RData'
    example_outfile = 'results/example_trialstreamer.RData'
    supplement_outfile = 'results/supplement_trialstreamer.RData'
    precision_outfile = 'results/precision_trialstreamer.RData'
    simulation = FALSE
  }
  if(source == 'validation'){
    infile = 'results/bugs_real_validation.RData' # from 4_model.R
    outjpg = 'figures/flagged_papers_validation.jpg'
    outfile = '5_summary_results_validation.docx'
    tree_outfile = 'data/tree_ready_validation.RData'
    results_outfile = 'results/uniform_validation.RData'
    example_outfile = 'results/example_validation.RData'
    supplement_outfile = 'results/supplement_validation.RData'
    precision_outfile = 'results/precision_validation.RData'
    simulation = FALSE
  }
  if(source == 'simulation'){
    infile = 'results/bugs_real_simulation.RData' # from 4_model_simulated.R
    outjpg = 'figures/flagged_papers_simulation.jpg'
    outfile = '5_summary_results_simulation.docx'
    tree_outfile = 'data/tree_ready_simulation.RData'
    results_outfile = 'results/uniform_simulation.RData'
    example_outfile = 'results/example_simulation.RData'
    precision_outfile = 'results/precision_simulation.RData'
    simulation = TRUE
  }
  if(source == 'simulation_two'){
    infile = 'results/bugs_real_simulation_two.RData' # from 4_model.R
    outjpg = 'figures/flagged_papers_simulation_two.jpg'
    outfile = '5_summary_results_simulation_two.docx'
    tree_outfile = 'data/tree_ready_simulation_two.RData'
    results_outfile = 'results/uniform_simulation_two.RData'
    example_outfile = 'results/example_simulation_two.RData'
    precision_outfile = 'results/precision_simulation_two.RData'
    supplement_outfile = 'results/supplement_simulation_two.RData'
    simulation = TRUE
  }
  if(source == 'simulation_three'){
    infile = 'results/bugs_real_simulation_three.RData' # from 4_model.R
    outjpg = 'figures/flagged_papers_simulation_three.jpg'
    outfile = '5_summary_results_simulation_three.docx'
    tree_outfile = 'data/tree_ready_simulation_three.RData'
    results_outfile = 'results/uniform_simulation_three.RData'
    example_outfile = 'results/example_simulation_three.RData'
    precision_outfile = 'results/precision_simulation_three.RData'
    supplement_outfile = 'results/supplement_simulation_three.RData'
    simulation = TRUE
  }
  if(source == 'bland'){
    infile = 'results/bugs_real_bland.RData' # from 4_model.R
    outjpg = 'figures/flagged_papers_simulation_bland.jpg'
    outfile = '5_summary_results_bland.docx'
    tree_outfile = 'data/tree_ready_bland.RData'
    results_outfile = 'results/uniform_bland.RData'
    example_outfile = 'results/example_bland.RData'
    supplement_outfile = 'results/supplement_bland.RData'
    precision_outfile = 'results/precision_bland.RData'
    simulation = TRUE
  }  
  if(source == 'carlisle'){
    infile = 'results/bugs_real_carlisle.RData' # from 4_model.R
    outjpg = 'figures/flagged_papers_simulation_carlisle.jpg'
    outfile = '5_summary_results_carlisle.docx'
    tree_outfile = 'data/tree_ready_carlisle.RData'
    results_outfile = 'results/uniform_carlisle.RData'
    example_outfile = 'results/example_carlisle.RData'
    supplement_outfile = 'results/supplement_carlisle.RData'
    precision_outfile = 'results/precision_carlisle.RData'
    simulation = FALSE
  }  
  load(infile) 
}

### Section 5: tree ###

if(stage == 'tree'){
  
  if(source == 'trialstreamer'){
    infile1 = 'data/tree_ready_trialstreamer.RData' # from 5_summary_results.Rmd
    infile2 = 'data/analysis_ready_trialstreamer.RData' # from 2_process_extracted_data.R
    outtree = 'figures/precision_tree_trialstreamer.jpg'
  }
  load(infile1) 
  load(infile2) 
  remove(excluded, excluded_counts, errors, pvalues) # tidy-up
}