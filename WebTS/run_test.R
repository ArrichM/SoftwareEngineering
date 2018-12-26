toload <- c("magrittr","reshape2","lubridate","zoo","tseries","dyn","testthat","rstudioapi")
toinstall <- toload[which(toload %in% installed.packages()[,1] == F)] #which packages are not already installed?
lapply(toinstall, install.packages, character.only = TRUE) #install missing packages
lapply(toload, require, character.only = TRUE) #load packages

# set correct wd
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

# source script containing functions to test
source("resources.R")

# run tests
test_results <- test_dir(getwd(), reporter="summary")

# test results
print(test_results)

