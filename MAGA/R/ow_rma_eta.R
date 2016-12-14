##### Effect size measures for a one-way repeated measures ANOVA
  # Function 'ow_rma' is required (quantlet1_rm_anova.R)!


ow_rma_eta = function(ow_rma_data, append = FALSE){


# Get ANOVA-table ---------------------------------------------------------
  # rmANOVA function 'ow_rma' is required!


  ANOVA_table = ow_rma(ow_rma_data)[[1]]


# Define needed sums of squares -------------------------------------------


  SS_Factor = ANOVA_table[2,2]
  SS_Error = ANOVA_table[4, 2]
  SS_K_Total = ANOVA_table[6,2]


# Compute effect size measures --------------------------------------------


  # Compute standard eta^2
  eta_sq = SS_Factor / SS_K_Total

  # Compute partial eta^2
  eta_partial = SS_Factor / (SS_Factor + SS_Error)


# Create separate table for effect sizes


  effect_size_table = data.frame(check.names = FALSE,
                                 "Source" = "Factor",
                                 "eta squared" = eta_sq,
                                 "partial eta squared" = eta_partial
                                 )
  rownames(effect_size_table) = NULL

# Append effect size measures to ANOVA-table ------------------------------


  ANOVA_table[,"eta squared"] = c(NA, eta_sq, NA, NA, NA, NA)
  ANOVA_table[,"partial eta squared"] = c(NA, eta_partial, NA, NA, NA, NA)


# Return ANOVA-table with effect sizes or create seperate table -----------


  if (append == TRUE){
    return(list("effect_size_table" = effect_size_table, "one_way_repeated_measures_ANOVA_table_with_effect_size_measures" = ANOVA_table))
  }else{
    return(list("effect_size_table" = effect_size_table))
  }
}


