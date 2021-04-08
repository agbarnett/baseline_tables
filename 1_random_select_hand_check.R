# 1_random_select_hand_check.R
# randomly select papers for checking by hand
# April 2021
library(dplyr)
library(TeachingDemos)
char2seed('trials')

# get the data from 0_find_trials_pmc.R 
load('data/pmid_trials.RData')
data = mutate(data.frame(data), runif = runif(n=n())) %>%
  arrange(runif) %>% # randomly order
  ungroup() %>%
  slice(1:10) %>% # just take 10
  select(pmc) %>%
  mutate(link = paste('https://www.ncbi.nlm.nih.gov/pmc/articles/', pmc, sep=''))

# export to Excel
library(openxlsx)
filename = "data/papers_to_check.xlsx"
wb = createWorkbook(creator='Adrian Barnett')
addWorksheet(wb, sheetName = "PMCID")
for (sheet in data$pmc){
  addWorksheet(wb, sheetName = sheet)
}
freezePane(wb, sheet = 1, firstRow = TRUE) ## freeze first column
writeDataTable(wb, sheet = 1, x = data,
               colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, filename, overwrite = TRUE)
