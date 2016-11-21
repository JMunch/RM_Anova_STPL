### This script contains the data preparation of the RM Anova data
### Run this script prior to all other quantlets


# required packages


rma_data <- read.csv("data/noisedata.csv")

rma_data$GENDER <- NULL
rma_data$X <- NULL

