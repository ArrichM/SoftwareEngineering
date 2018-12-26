toload <- c("magrittr","reshape2","lubridate","zoo","tseries","dyn","testthat")
toinstall <- toload[which(toload %in% installed.packages()[,1] == F)] #which packages are not already installed?
lapply(toinstall, install.packages, character.only = TRUE) #install missing packages
lapply(toload, require, character.only = TRUE) #load packages

source("resources.R")
source("test_resources.R")

test_results <- test_dir(getwd(), reporter="summary")

print(test_results)

