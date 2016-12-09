# Function to simulate data for repeated measurement ANOVA


sim_rma_data = function(n, k, means = NULL, poly_order = NULL, noice_sd = 10, between_subject_sd = 40){
  
# Create empty n x k matrix 
rma_data = matrix(, nrow = n, ncol = k + 1)

# Add column with subject_id
  rma_data[, 1] = 1:n
  

  if(!is.null(means)){
    con_means = means
    
# Check if length of mean vector corresponds to k
    if(length(means) != k){
      k = length(means)
      print("Number of factors (k) was changed, because the length of means vector and argument k do not correspond.")
    }
  } else {
    
# Simulate conditional means
    if(is.null(poly_order)){
  con_means = runif(k, min = 100, max = 300)
    } else{
      
# Generate polinomial conditional means
      factors = runif((poly_order + 1), min = 0, max = 1)
      x = order(runif(k, min = 100, max = 300), decreasing = FALSE)
      con_means = matrix(factors[1], nrow = k)
      
      for(p in (2: (poly_order + 1))){
        con_means = con_means + factors[p] * x ^ p 
      }
      
    }
  }
  
# Add con_mean to the rma_data matrix
  rma_data[, 2:(k+1)] = matrix(rep(con_means, each = n), nrow = n)
  
# Simulate subject means
# Calculate the deviation from the conditional mean for each subject
  mean_deviation = rnorm(n, mean = 0, sd = between_subject_sd)
  rma_data[, 2:(k+1)]  = rma_data[, 2:(k+1)] + mean_deviation

# Check if only one noice_sd was passed (no sphericity) and create vector 
  if(length(noice_sd) == 1){
  noice_sd = rep(noice_sd, times =  k)
  }
  
# Error if noice_sd vector not a vector of length k -----------------------
  if(length(noice_sd) != k){
    print("The vector passed for noice_sd does not have the length k. Please pass a vector of the length k.")
    return(NULL)
  }
  

# Add noice to data
  noice = matrix(NA, nrow = n, ncol = k) 
  for(i in 1:k){
    noice[,i] = rnorm(n, mean = 0, sd = noice_sd[i])
  }
  rma_data[, 2:(k+1)]  = rma_data[, 2:(k+1)] + noice

# Naming columns
  factor_names = character(k+1)
  factor_names[1] = "Subject_id"
  for(i in 1:k){
    factor_names[i+1] = paste("Factor",i)
  }
  colnames(rma_data) = factor_names
  
  return(data.frame(rma_data))
}

# ------------------------------------------------------------
# Testing:

source("r/quantlet1_rm_anova.R")
rma_data = sim_rma_data(1000, 4, means = NULL, poly_order = 5, noice_sd = c(10, 20, 30), between_subject_sd = 40)
rma_data

