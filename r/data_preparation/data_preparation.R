##### This script contains the data preparation of the RM Anova data
  # Run this script prior to all other quantlets


# Read data ---------------------------------------


rma_data <- read.csv("data/noisedata.csv")


# Remove idle variables ---------------------------


rma_data$GENDER <- NULL
rma_data$X <- NULL


# -------------------------------------------------
