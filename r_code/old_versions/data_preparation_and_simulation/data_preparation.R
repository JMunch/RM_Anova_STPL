##### This script contains the data preparation of the RM Anova data
  # Run this script prior to all other quantlets
  # Requirement for the datat: each entity is measured under each factor level (otherwise listwise deletion); wide format; ordered equidistant factor levels


# Read data ---------------------------------------


ow_rma_data = read.csv("data/noisedata.csv")
ow_rma_data[2,4] = NA

# Remove idle variables ---------------------------


ow_rma_data$GENDER = NULL
ow_rma_data$X = NULL


# Listwise deletion in case of missing values -----


deletionvector = vector(mode="numeric", length=0)

for(i in 1:nrow(ow_rma_data)) {
    if(any(is.na(ow_rma_data[i,])) == TRUE) {
      deletionvector = union(deletionvector, i)
      print(paste("Missing value(s) for subject", i))
    }
}

ow_rma_data = ow_rma_data[-deletionvector,]
