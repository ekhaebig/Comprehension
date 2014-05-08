# This file collects functions used by both tasks. 

library(plyr)
library(reshape2)
library(lubridate, warn.conflicts = FALSE)
library(tools)
library(stringr)
library(ggplot2)
library(grid)
#library(httr)

# Load all of the code from the lookr repo
print("Reading from a local clone of the lookr git-repo:")
lookr.files <- file.path('lookr','R',list.files('lookr/R/'))
for (f in lookr.files) {
	print(paste('sourcing',f))
	source(f)
}
print("DONE!")
