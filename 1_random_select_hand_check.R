# 1_random_select_hand_check.R
# randomly select papers for checking by hand
# May 2021
library(openxlsx)
library(dplyr)
library(TeachingDemos)
char2seed('trials')

# get the data from 0_find_trials_pmc.R 
load('data/pmid_trials.RData')
numbers_to_take = 101:200 # number to check (done in batches)
data = mutate(data.frame(data), runif = runif(n=n())) %>%
  arrange(runif) %>% # randomly order
  ungroup() %>%
  slice(numbers_to_take) %>% # 
  select(pmc) %>%
  mutate(link = paste('https://www.ncbi.nlm.nih.gov/pmc/articles/', pmc, sep=''))

# export to Excel
filename = "checks/papers_to_check3.xlsx"
wb = createWorkbook(creator='Adrian Barnett')
addWorksheet(wb, sheetName = "PMCID")
for (sheet in data$pmc){
  addWorksheet(wb, sheetName = sheet)
}
addWorksheet(wb, sheetName = 'sample size')
addWorksheet(wb, sheetName = 'p-value')
freezePane(wb, sheet = 1, firstRow = TRUE) ## freeze first column
writeDataTable(wb, sheet = 1, x = data,
               colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, filename, overwrite = TRUE)
