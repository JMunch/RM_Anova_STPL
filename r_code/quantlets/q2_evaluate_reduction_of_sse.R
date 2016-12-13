##### Evaluate reduction of error sum of squares due to the consideration of variance between entities
  # Function 'ow-rma' is required (quantlet1_rm_anova.R)!


ow_rma_sse_reduct = function(ow_rma_data, plot_type = "pie"){
  
  
# Libraries needed ----------------------------------------------------------
 
  
  require(dplyr)
  require(ggplot2)


# Computaion of ANOVA model -------------------------------------------------
    
  # Note that the one-way ANOVA without repeated measures is for illustration purposes 
  # only since the data structure is correlated across the factor  levels because 
  # of the dependent measurements.
  
  # The ANOVA without repeated measures treates the data as if they are independend 
  # i.e. as if there are different entities in each group, which is in fact not the case.
  
  

  ow_a = function(ow_rma_data){
  
  
# Define needed constants and the dependent variable ------------------------
  
  
    # Number of entities in one group
    n_group = nrow(ow_rma_data)
    
    # Number of factor levels
    k = ncol(ow_rma_data) - 1
    
    # Number of entities
    n = (k * n_group)
    
    dependent_variable = as.matrix(ow_rma_data[, -1])
  
  
# Define basic ANOVA components ---------------------------------------------
  
  
    grand_mean = mean(as.matrix(ow_rma_data[,2: (k + 1)])) 
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
  # Different dof's for the ss_error than in rmANOVA
  
  
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
  # Corrected total sum of squares could be taken from the repeated measures ANOVA-Model as they coincide 
  
  
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
  
  
    return(list("ANOVA_table_without_repeaded_measures" = ANOVA_table))
  }


# Comparison of error terms in both models ----------------------------------

  
# Defining variables --------------------------------------------------------
  # rmANOVA function 'ow-rma' is required!
  # ANOVA function 'ow-a' is required!
  
  # ANOVA-tables of rmANOVA and ANOVA without repeated measures
  ow_a_results = ow_a(ow_rma_data)[[1]]
  ow_rma_results = ow_rma(ow_rma_data)[[1]]
  
  sse_anova = ow_a_results[3, 2]
  ss_subject_anova = 0 
  # Always zero because the subject effect is not considered in an ANOVA without repeated measures
  
  sse_rma = ow_rma_results[4, 2]
  ss_subject_rma = ow_rma_results[3, 2]
  
  
# Defining a table containing the different errortypes-----------------------
  # Dependency: SSE in RM ANOVA is equal to SSE ANOVA minus SS Subjects
  
  
  error_ss_comparison_table = data.frame(check.names = FALSE,
                                         "Sum of squares" = c("Error", "Entity"),
                                         "ANOVA"   = c(sse_anova, ss_subject_anova),
                                         "rmANOVA" = c(sse_rma, ss_subject_rma)
                                        )
  rownames(error_ss_comparison_table) = NULL
  

# Preparing the error sum of squares comparison plots ------------------------
  
  
  # Convert into matrix for stackplot
  ss_comp = as.matrix(error_ss_comparison_table[, -1])
  rownames(ss_comp) = error_ss_comparison_table[, 1]
  colnames(ss_comp) = c("No estmation of  the variation between entities", "Estmation of  the variation between entities")
  

  # Stacked barplot and piechart displaying the reduction of error terms ------------------------------------------
  
  # create variables for comparison plot
  
  # var contains the sse by subject and error
  var <- unlist(error_ss_comparison_table[, -1])
  
  # model is used in the ggplot to assign the values to the bars
  model <- rep(c("No estimation of the\nvariation between entities", "Estimation of the\nvariation between entities"), each = 2)
  
  # source is required for color and legend label assignment in the ggplot
  source <- factor(rep(c("Error", "Entity"), times = 2), levels = c("Entity", "Error"))
  
  # merge variables into one data frame
  comparison_data <- data.frame(var, model, source)
  
  # create stacked barplot
  comp_plot_bar <- ggplot(comparison_data, aes(model, var, fill = source)) + 
      geom_bar(stat = "identity") + 
      labs(x = "Model", y = "Sum of squares (error)", title = "Reduction of sum of squared errors (SSE)") + 
      guides(fill=guide_legend(title=NULL)) + 
      scale_fill_manual(values = c("orange", "navyblue")) + 
      theme_bw() + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) 
  
  # new variable: percentage of sse. used for better readability in piechart
  # for consistency of interpretations it might make sense to use this variable in the barplots as well
  comparison_data$var_percent <- comparison_data$var*100/(max(comparison_data$var))
  
  # create pie chart
  comp_plot_pie <- ggplot(comparison_data, aes(x = "", y = var_percent, fill = source)) + 
      geom_bar(width = 1, stat = "identity") + 
      labs(x = "", y = "", title = "Reduction of sum of squared errors (SSE)") + 
      guides(fill=guide_legend(title=NULL)) + 
      scale_fill_manual(values = c("orange", "navyblue")) + 
      coord_polar(theta = "y") + 
      facet_grid(~model) + 
      theme_bw() + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) 
  
  

  if(plot_type == "pie"){
      final_plot <- comp_plot_pie
  } else {
      final_plot <- comp_plot_bar
  }
  
# Return comparison table ---------------------------------------------------
  
  warning("\nNote that the one-way ANOVA without repeated measures is for\nillustration purposes only since the data structure is correlated\nacross the factor levels because of the dependent measurements.\nThe ANOVA without repeated measures treates the data as if they\nare independend i.e. as if there are different entities in each\ngroup, which is in fact not the case.")
  print(final_plot)
  return(list("one_way_ANOVA_table" = ow_a_results, "error_sum_of_squares_reduction_table" = error_ss_comparison_table))
}


# ---------------------------------------------------------------------------

ow_rma_sse_reduct(ow_rma_data, plot_type = "bar")



