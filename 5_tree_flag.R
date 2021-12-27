# 5_tree_flag.R
# use classification tree to see if study design can explain precision flag
# August 2021 
library(dplyr) 
library(rpart)

# get the design data and model flag
load() #

model = 