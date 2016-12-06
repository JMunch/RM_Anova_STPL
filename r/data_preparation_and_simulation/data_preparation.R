##### This script contains the data preparation of the RM Anova data
  # Run this script prior to all other quantlets


# Read data ---------------------------------------


ow_rma_data <- read.csv("data/noisedata.csv")


# Remove idle variables ---------------------------


ow_rma_data$GENDER <- NULL
ow_rma_data$X <- NULL


# -------------------------------------------------

