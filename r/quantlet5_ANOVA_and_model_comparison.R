##### Computation of standard ANOVA and comparison with RM-ANOVA ###


# Computaion of ANOVA model -------------------------------------------------


anova = function(rma_data){

  
# Libraries needed for compuiting rma ---------------------------------------
  
  
  require(dplyr)
  
  
# Define needed constants and the dependent variable ------------------------
  
  
  # Number of entities in one group
  n_group = nrow(rma_data)
  
  # Number of factor levels
  k = ncol(rma_data) - 1
  
  # Number of enteties
  n = (k * n_group)
  
  dependent_variable = as.matrix(rma_data[, -1])
  
  
# Define basic anova components ---------------------------------------------
  
  
  grand_mean = mean(as.matrix(rma_data[,2: (k + 1)])) 
  baseline_components = matrix(rep(grand_mean, times = n), nrow = n_group)
  
  conditional_means = apply(dependent_variable, 2, mean)
  factor_level_components = matrix(rep(conditional_means - grand_mean, each = n_group), nrow = n_group)


  # Computation of the error component 
  error_components_ANOVA = dependent_variable - baseline_components - factor_level_components
  
  
# Prepare decomposition matrix ----------------------------------------------
  # Matrix with n rows and 4 columns
  # One column for: original values, factor level component, error component
  
  
  decomposition_matrix_ANOVA = data.frame("dependent_variable" = numeric(n),
                                          "baseline" = numeric(n),
                                          "factor_level" = numeric(n),
                                          "error" = numeric(n)
                                          )
  
  decomposition_matrix_ANOVA$dependent_variable = as.vector(dependent_variable)
  decomposition_matrix_ANOVA$baseline = as.vector(baseline_components)
  decomposition_matrix_ANOVA$factor_level = as.vector(factor_level_components)
  decomposition_matrix_ANOVA$error = as.vector(error_components_ANOVA)
  
  
# Compute sums of squares ---------------------------------------------------
  
  
  ss_ANOVA = as.data.frame(t(colSums(decomposition_matrix_ANOVA^2)))
  rownames(ss_ANOVA) = "sums_of_squares"
  
  
# Set degrees of freedom ----------------------------------------------------
  #Different dof's for the ss_error than in rmANOVA
  
  
  dof_ANOVA = data.frame("dependent_variable" = n,
                         "baseline" = 1,
                         "factor_level" = (k - 1),
                         "error" = (n - k)
                         )
  
  
# Compute mean squares ------------------------------------------------------

    
  ms_ANOVA = ss_ANOVA / dof_ANOVA
  rownames(ms_ANOVA) = "mean_squares"
  
  
# Calculate the F-Value -----------------------------------------------------
  
  
  F_value_factor_ANOVA = ms_ANOVA$factor_level / ms_ANOVA$error
  F_value_baseline_ANOVA = ms_ANOVA$baseline / ms_ANOVA$error
  
  
# Set p-value of F-distribution ---------------------------------------------

    
  p_factor_ANOVA = 1 - pf(F_value_factor_ANOVA, dof_ANOVA$factor_level, dof_ANOVA$error)
  p_baseline_ANOVA = 1 - pf(F_value_baseline_ANOVA, dof_ANOVA$baseline, dof_ANOVA$error)
  

# Compute corrected total sum of squares (variance) -------------------------
  
  
  corrected_sst = ss_ANOVA$dependent_variable - ss_ANOVA$baseline
  variance = corrected_sst / (dof_ANOVA$dependent_variable - dof_ANOVA$baseline)
  
    
# Create the output table ---------------------------------------------------
  # Corrected total sum of squares could be taken from the RM ANOVA-Model as they coincide 
  
  
  # Specify source variable
  source_ANOVA = c("Baseline", "Factor", "Error", "Total", "Corrected total")
  
  # Create table
  ANOVA_table = data.frame(check.names = FALSE,
                           "Source" = source_ANOVA,
                           "Sum of squares" = c(ss_ANOVA %>% select(2:4, 1) %>% unlist(),corrected_sst),
                           "Degrees of freedom" = c(dof_ANOVA %>% select(2:4, 1) %>% unlist(), (n - 1)),
                           "Mean squares" = c(ms_ANOVA %>% select(2:4) %>% unlist(), NA, variance),
                           "F-value" = c(F_value_baseline_ANOVA, F_value_factor_ANOVA, NA, NA, NA),
                           "p-value" = c(p_baseline_ANOVA, p_factor_ANOVA, NA, NA, NA)
                           )
  rownames(ANOVA_table) = NULL
  
  
# Return ANOVA-table --------------------------------------------------------
  
  
  return("ANOVA_table_without_repeaded_measures" = ANOVA_table)
}


# ---------------------------------------------------------------------------
# Testing:

#source("r/simulate_rma_data.R")
#rma_data = sim_rma_data(10, 5)
anova_results = anova(rma_data)

anova_results


# ---------------------------------------------------------------------------


# Comparison of error terms in both models ----------------------------------
  # Function 'rma' is required (quantlet1_rm_anova.R)!
  # Function 'anova' is required (quantlet5_ANOVA_and_model_comparison)


compare_anovas = function(rma_data){

  
# Libraries needed for compuiting rma ---------------------------------------
  
  
  require(ggplot2)
  require(reshape)
  
  
# Defining variables --------------------------------------------------------
  
  
  # ANOVA-tables of rmANOVA and ANOVA without repeated measures
  anova_results = anova(rma_data)
  rma_results = rma(rma_data)
  
  sse_anova = anova_results[3, 2]
  ss_subject_anova = 0 # Always zero because the subject effect is not considered in an ANOVA without repeated measures
  
  sse_rma = rma_results[4, 2]
  ss_subject_rma = rma_results[3, 2]
  
  
# Defining a table containing the different errortypes-----------------------
  # Dependency: SSE in RM ANOVA is equal to SSE ANOVA minus SS Subjects
  
  
  error_ss_comparison_table = data.frame(check.names = FALSE,
                                         "Sum of squares" = c("Error", "Subject"),
                                         "ANOVA"   = c(sse_anova, ss_subject_anova),
                                         "rmANOVA" = c(sse_rma, ss_subject_rma)
                                        )
  rownames(error_ss_comparison_table) = NULL
  

# Preparing the error sum of squares comparison plots ------------------------
  
  
  # Convert into matrix for stackplot
  ss_comp = as.matrix(error_ss_comparison_table[, -1])
  rownames(ss_comp) = error_ss_comparison_table[, 1]
  
  # All plots in one figure
  par(mfrow = c(1, 3))
  
  
# Stackplot (version 1) ------------------------------------------------------
  
  
  barplot1 = barplot(ss_comp, 
                     col = c("navyblue", "orange"), 
                     xlab = "Model", 
                     ylab = "Sum of squares (error)",
                     legend.text = TRUE,
                     main = "Comparison of error terms between standard ANOVA and Repeated Measures ANOVA")
  
  
# Pie Charts-Version 1 -------------------------------------------------------
  
  
  slices = c(sse_anova, ss_subject_anova) 
  lbls = c("SS error", "SS subject level")
  pct = round(slices/sum(slices)*100)
  lbls = paste(lbls, pct) # add percents to labels 
  lbls = paste(lbls,"%",sep="") # ad % to labels 
  pie1_ANOVA = pie(slices, lbls, col = c("navyblue", "orange"),
                   main="ANOVA", init.angle = 90) 
  
  slices_2 = c(sse_rma, ss_subject_rma) 
  lbls_2 = c("SS error", "SS subject level")
  pct_2 = round(slices_2/sum(slices_2)*100)
  lbls_2 = paste(lbls_2, pct_2) # add percents to labels 
  lbls_2 = paste(lbls_2,"%",sep="") # ad % to labels 
  pie1_RM_ANOVA = pie(slices_2, lbls_2, col = c("navyblue", "orange"),
                      main="rmANOVA", init.angle = 90) 
  
  
# Stackplot Version 2 (with ggplot) ------------------------------------------
  
  
  #Defining a dataframe for model comparison
  ##ss_table_gg = melt(ss_table)
  
  ##colnames(ss_table_gg) = c("Error_type", "ANOVA_type", "value")
  ##ss_table_gg$value = as.numeric(ss_table_gg$value)
  ##ss_table_gg$ANOVA_type = as.factor(ss_table_gg$ANOVA_type)
  ##ss_table_gg$Error_type = as.factor(ss_table_gg$Error_type)
  
  ##barplot2 = ggplot(ss_table_gg, aes(x = ANOVA_type, y = (value/(sum(value)/2)*100), fill = Error_type)) + 
  ##                  geom_bar(stat = "identity", width = 0.8) +
  ##                  ggtitle("Comparison of error terms between standard ANOVA and Repeated Measures ANOVA") + 
  ##                  xlab("Model") +
  ##                  ylab("Percentages") +
  ##                  geom_text(aes(label = (value/(sum(value)/2))*100), position = "identity"
  ##                  ) 
  ##barplot2
  
  
# Pie Chart Version 2 (with ggplot) ------------------------------------------
  ## !!! What is the object bar_chart?

    
  ##pie2 = bar_chart + 
  ##       coord_polar(theta = "y", direction = -1)  + 
  ##       facet_grid(.~model_comparison$ANOVA_type) +
  ##       theme_void() +
  ##    ggtitle("Comparison of error terms between standard ANOVA and Repeated Measures ANOVA")
  ##pie2


# Return comparison table
  
  
  return("error_ss_comparison_table" = error_ss_comparison_table)
}


# ---------------------------------------------------------------------------
# Testing:

#source("r/simulate_rma_data.R")
#rma_data = sim_rma_data(10, 5)

compare_anovas(rma_data)

