##### Computation of one-way repeated measures ANOVA (rma)


ow_rma = function(ow_rma_data){
  
  
# Libraries needed --------------------------------------------------------

  
  require(dplyr)


# Define needed constants and the dependent variable ----------------------

  # Number of entities
  n = nrow(ow_rma_data)

  # Number of factor levels 
  k = ncol(ow_rma_data) - 1

  dependent_variable = as.matrix(ow_rma_data[, -1])


# Define basic anova components -------------------------------------------


  grand_mean = mean(as.matrix(ow_rma_data[,2: (k + 1)])) 
  baseline_components = matrix(rep(grand_mean, times = k*n), nrow = n)
  
  conditional_means = apply(dependent_variable, 2, mean)
  factor_level_components = matrix(rep(conditional_means - grand_mean, each = n), nrow = n)
  
  subject_means = apply(dependent_variable, 1, mean)
  subject_components = matrix(rep(subject_means - grand_mean, times = k), nrow = n)
  
  error_components = dependent_variable - baseline_components - factor_level_components - subject_components


# Prepare decomposition matrix --------------------------------------------
  # Matrix with k * n rows and 5 columns
  # One column for: original values, baseline component, factor level component, subject component, error component


  decomposition_matrix = data.frame("dependent_variable" = numeric(n*k),
                                    "baseline" = numeric(n*k),
                                    "factor_level" = numeric(n*k),
                                    "subject_level" = numeric(n*k),
                                    "error" = numeric(n*k)
                                    )


# Construct decomposition matrix ------------------------------------------

  
  decomposition_matrix$dependent_variable = as.vector(dependent_variable)
  decomposition_matrix$baseline = as.vector(baseline_components)
  decomposition_matrix$factor_level = as.vector(factor_level_components)
  decomposition_matrix$subject_level = as.vector(subject_components)
  decomposition_matrix$error = as.vector(error_components)


# Compute sums of squares -------------------------------------------------


  ss = as.data.frame(t(colSums(decomposition_matrix^2)))
  rownames(ss) = "sums_of_squares"


# Set degrees of freedom --------------------------------------------------

  
  dof = data.frame("dependent_variable" = n*k,
                  "baseline" = 1,
                  "factor_level" = k-1,
                  "subject_level" = n-1,
                  "error" = (n*k)-1-(k-1)-(n-1)
                  )


# Compute mean squares ----------------------------------------------------


  ms = ss / dof
  rownames(ms) = "mean_squares"


# Compute corrected total sum of squares (variance) -----------------------


  corrected_sst = ss$dependent_variable - ss$baseline
  variance = corrected_sst / (dof$dependent_variable - dof$baseline)
  

# Compute F-values --------------------------------------------------------
  
  
  F_value_factor = ms$factor_level / ms$error
  
  F_value_baseline = ms$baseline / ms$subject_level 


# Set p-values of F distribution ------------------------------------------

  
  p_factor = 1 - pf(F_value_factor, dof$factor_level, dof$error)
  p_baseline = 1 - pf(F_value_baseline, dof$baseline, dof$subject_level)

  
# Create the output table for rmANOVA--------------------------------------


  # Specify source variable
  source = c("Baseline", "Factor", "Subject", "Error", "Total", "Corrected total")
  
  # Create table
  ANOVA_table = data.frame(check.names = FALSE,
                           "Source" = source,
                           "Sum of squares" = c(ss %>% select(2:5,1) %>% unlist(), corrected_sst),
                           "Degrees of freedom" = c(dof %>% select(2:5,1) %>% unlist(), (n*k)-1),
                           "Mean squares" = c(ms %>% select(2:5) %>% unlist(), NA, variance),
                           "F-value" = c(F_value_baseline, F_value_factor, NA, NA, NA, NA),
                           "p-value" = c(p_baseline, p_factor, NA, NA, NA, NA)
                           )
  rownames(ANOVA_table) = NULL
  
 
# Return ANOVA-table ------------------------------------------------------
  
   
  return(list("one_way_repeated_measures_ANOVA_table" = ANOVA_table))
}


# -------------------------------------------------------------------------
# Testing:

#source("r/simulate_rma_data.R")
#rma_data = sim_rma_data(n = 1000, k = 5, between_subject_sd = 50)
ow_rma_results = ow_rma(ow_rma_data)

ow_rma_results


