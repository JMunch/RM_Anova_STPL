##### Function to simulate data for repeated measurement ANOVA


sim_rma_data = function(n, k, noise_sd = 10, between_subject_sd = 40){
  
  
  # Create empty n x k matrix -------------------------------------------------- 
  
  
  rma_data = matrix(, nrow = n, ncol = k + 1)
  
  
  # Add column with subject_id -------------------------------------------------
  
  
  rma_data[, 1] = 1:n
  
  
  # Simulate conditional means -------------------------------------------------
  
  
  con_means = runif(k, min = 100, max = 300)
  rma_data[, 2:(k+1)] = matrix(rep(con_means, each = n), nrow = n)
  
  
  # Simulate subject means -----------------------------------------------------
  # Calculate the deviation from the conditional mean for each subject
  
  
  mean_deviation = rnorm(n, mean = 0, sd = between_subject_sd)
  rma_data[, 2:(k+1)]  = rma_data[, 2:(k+1)] + mean_deviation
  
  
  # Add noise to data ----------------------------------------------------------
  
  
  noise = matrix(rnorm(n * k, mean = 0, sd = noise_sd), nrow = n)
  rma_data[, 2:(k+1)]  = rma_data[, 2:(k+1)] + noise
  
  
  # Naming columns
  
  
  factor_names = character(k+1)
  factor_names[1] = "Subject_id"
  for(i in 1:k){
    factor_names[i+1] = paste("Factor",i)
  }
  colnames(rma_data) = factor_names
  
  return(data.frame(rma_data))
}


# -----------------------------------------------------------------------------
# Testing:

source("r/quantlet1_rm_anova.R")
rma_data = sim_rma_data(10,4, 10)
rma(rma_data = rma_data)


source("r/quantlet2_adj_and_unadj_ci_error_bar_graphs.R")
rma_CI(rma_data)

source("r/quantlet3_orth_poly_contrasts.R")
orth_poly_contrast(rma_data)

