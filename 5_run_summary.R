# 5_run_summary.R
# run the summary dependent on the input data (e.g., simulation or trialstreamer)
# January 2022
library(markdown)

## get the data; select which source of data to use
sources = c('my_search', 'trialstreamer', 'validation', 'simulation', 'bland', 'simulation_two', 'simulation_three', 'carlisle')
source = sources[2]
stage = 'plot'
source('1_which_data_source.R') # uses `source` and `stage`

# run markdown
rmarkdown::render(input = "5_summary_results.Rmd",
                  output_format = "word_document",
                  output_file = outfile) # outfile from 1_which_data_source.R
