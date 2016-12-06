##### This script contains the data preparation of the RM Anova data
  # Run this script prior to all other quantlets


# Read data ---------------------------------------


rma_data <- read.csv("data/noisedata.csv")


# Remove idle variables ---------------------------


rma_data$GENDER <- NULL
rma_data$X <- NULL


# Listwise deletion in case of missing values ------------------

deletionvector = vector(mode="numeric", length=0)

for(i in 1:nrow(rma_data)) {
  if(any(is.na(rma_data[i,])) == TRUE) {
    deletionvector = union(deletionvector, i)
  }
}

rma_data = rma_data[-deletionvector,]



