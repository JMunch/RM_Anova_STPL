##### Effect size measures
  # Function 'rma' is required (quantlet1_rm_anova.R)!


rma_effect_size = function(rma_data, append = FALSE){

  
# Get ANOVA-table ---------------------------------------------------------
  # rmANOVA function 'rma' is required!  
  
  
  ANOVA_table = rma(rma_data)
  
  
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
    return(list("effect_size_table" = effect_size_table, "ANOVA_table_with_effect_size_measures" = ANOVA_table))
  }else{
    return("effect_size_table" = effect_size_table)
  }
}


# -------------------------------------------------------------------------
# Testing:

source("r/simulate_rma_data.R")
rma_data = sim_rma_data(10, 5)

source("r/quantlet1_rm_anova.R")

rma_effect_size(rma_data)

