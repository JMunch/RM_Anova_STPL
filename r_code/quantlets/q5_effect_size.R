##### Effect size measures for a one-way repeated measures ANOVA
  # Function 'ow_rma' is required!


ow_rma_eta = function(ow_rma_data, independent_var = 1, append = FALSE){


# check if the data meet the requirements ---------------------------------

    
  # ow_rma_data needs to meet the following requirements:
    
    # independent_var must either be an integer specifying the column position
    # of the independent variable
    if(independent_var %in% 1:ncol(ow_rma_data) == FALSE || length(independent_var) != 1){
        stop("independent_var must be an integer specifying the column position of the independent variable")
    }
  
  # all variables must be numeric
  if(all(sapply(ow_rma_data, is.numeric)) == FALSE | any(sapply(ow_rma_data, is.factor))){
    stop("All variables in ow_rma_data must be numeric")
  }
  
  # n > k (i.e. more entities than factor levels)
  if(nrow(ow_rma_data) <= (ncol(ow_rma_data)-1)){
    stop("Number of entities must exceed number of factor levels")
  }
  
  # k >= 2 (i.e. at least two or more factor levels)
  if((ncol(ow_rma_data)-1) < 2){
    stop("At least two factor factor levels required")
  }
  
    
# Get ANOVA-table ---------------------------------------------------------
  # rmANOVA function 'ow_rma' is required!  
  
  
  ANOVA_table = ow_rma(ow_rma_data, independent_var = independent_var)[[1]]
  

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


# -------------------------------------------------------------------------


# Testing:
ow_rma_eta(ow_rma_data, independent_var = 1, append = FALSE)

