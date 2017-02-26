##### Effect size measures for a one-way repeated measures anova Function 'rma' is required!


rma_eta = function(rma_data, id = 1, append = FALSE) {
    
    # check if the data meet the requirements ---------------------------------
  
    # rma_data needs to meet the following requirements:
    
    # id must either be an integer specifying the column position of the independent variable
    if (id %in% 1:ncol(rma_data) == FALSE || length(id) != 1) {
        stop("id must be an integer specifying the column position of the independent variable")
    }
    
    # all variables must be numeric
    if (all(sapply(rma_data, is.numeric)) == FALSE | any(sapply(rma_data, is.factor))) {
        stop("All variables in rma_data must be numeric")
    }
    
    # n > k (i.e. more entities than factor levels)
    if (nrow(rma_data) <= (ncol(rma_data) - 1)) {
        stop("Number of entities must exceed number of factor levels")
    }
    
    # k >= 2 (i.e. at least two or more factor levels)
    if ((ncol(rma_data) - 1) < 2) {
        stop("At least two factor factor levels required")
    }
    
    
    # Get anova-table --------------------------------------------------------- 
    # rmanova function 'rma' is required!
  
    anova_table = rma(rma_data, id = id)[[1]]
    
    
    # Define needed sums of squares -------------------------------------------
    
    SS_Factor  = anova_table[2, 2]
    SS_Error   = anova_table[4, 2]
    SS_K_Total = anova_table[6, 2]
    
    
    # Compute effect size measures --------------------------------------------
    
    # Compute standard eta^2
    eta_sq = SS_Factor / SS_K_Total
    
    # Compute partial eta^2
    eta_partial = SS_Factor / (SS_Factor + SS_Error)
    
    # Create separate table for effect sizes
    
    effect_size_table = data.frame(check.names           = FALSE, 
                                   Source                = "Factor", 
                                   `eta squared`         = eta_sq, 
                                   `partial eta squared` = eta_partial)
    rownames(effect_size_table) = NULL
    
    
    # Append effect size measures to anova-table ------------------------------
    
    anova_table[, "eta squared"]         = c(NA, eta_sq, NA, NA, NA, NA)
    anova_table[, "partial eta squared"] = c(NA, eta_partial, NA, NA, NA, NA)
    
    
    # Return anova-table with effect sizes or create seperate table -----------
    
    if (append == TRUE) {
        return(anova_table)
    } else {
        return(effect_size_table)
    }
}