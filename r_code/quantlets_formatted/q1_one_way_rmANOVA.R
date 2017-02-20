##### Computation of one-way repeated measures ANOVA (rma)


rma = function(rma_data, id = 1) {
    
    
    # Define needed constants and the dependent variable ----------------------
    
    
    # id must be an integer specifying the column position of the independent variable
    if (id %in% 1:ncol(rma_data) == FALSE || length(id) != 1) {
        stop("id must be an integer specifying the column position of the independent variable")
    }
    
    dependent_variable = as.matrix(rma_data[, -id])
    
    
    # Number of entities
    n = nrow(rma_data)
    
    # Number of factor levels
    k = ncol(dependent_variable)
    
    
    
    # check if the data meet the requirements ---------------------------------
    
    
    # rma_data needs to meet the following requirements:
    
    # all variables must be numeric
    if (all(sapply(rma_data, is.numeric)) == FALSE | any(sapply(rma_data, is.factor))) {
        stop("All variables in rma_data must be numeric")
    }
    
    # n > k (i.e. more entities than factor levels)
    if (n <= k) {
        stop("Number of entities must exceed number of factor levels")
    }
    
    # k >= 2 (i.e. at least two or more factor levels)
    if (k < 2) {
        stop("At least two factor factor levels required")
    }
    
    
    # Libraries needed --------------------------------------------------------
    
    
    # suppress warning message about masked objects by dplyr NOTE: This function still loads the dplyr package!
    suppressWarnings(suppressMessages(require(dplyr)))
    
    
    # Define basic anova components -------------------------------------------
    
    
    grand_mean              = mean(dependent_variable)
    baseline_components     = matrix(grand_mean, nrow = n, ncol = k)
    
    conditional_means       = colMeans(dependent_variable)
    factor_level_components = matrix(conditional_means - grand_mean, nrow = n, ncol = k, byrow = TRUE)
    
    subject_means           = rowMeans(dependent_variable)
    subject_components      = matrix(subject_means - grand_mean, nrow = n, ncol = k)
    
    error_components        = dependent_variable - baseline_components - factor_level_components - subject_components
    
    
    # Construct decomposition matrix ------------------------------------------ Matrix with k * n rows and 5 columns One column for:
    # original values, baseline component, factor level component, subject component, error component
    
    
    decomposition_matrix = data.frame(dependent_variable = as.vector(dependent_variable), 
                                      baseline           = as.vector(baseline_components), 
                                      factor_level       = as.vector(factor_level_components), 
                                      subject_level      = as.vector(subject_components), 
                                      error              = as.vector(error_components))
    
    
    
    # Compute sums of squares -------------------------------------------------
    
    
    ss           = as.data.frame(t(colSums(decomposition_matrix^2)))
    rownames(ss) = "sums_of_squares"
    
    
    # Set degrees of freedom --------------------------------------------------
    
    
    dof = data.frame(dependent_variable = n * k, 
                     baseline           = 1, 
                     factor_level       = k - 1, 
                     subject_level      = n - 1, 
                     error              = (n * k) - 1 - (k - 1) - (n - 1))
    
    
    # Compute mean squares ----------------------------------------------------
    
    
    ms           = ss / dof
    rownames(ms) = "mean_squares"
    
    
    # Compute corrected total sum of squares (variance) -----------------------
    
    
    corrected_sst = ss$dependent_variable - ss$baseline
    variance      = corrected_sst / (dof$dependent_variable - dof$baseline)
    
    
    # Compute F-values --------------------------------------------------------
    
    
    F_value_factor = ms$factor_level / ms$error
    
    F_value_baseline = ms$baseline / ms$subject_level
    
    
    # Set p-values of F distribution ------------------------------------------
    
    
    p_factor   = 1 - pf(F_value_factor, dof$factor_level, dof$error)
    p_baseline = 1 - pf(F_value_baseline, dof$baseline, dof$subject_level)
    
    
    # Create the output table for rmANOVA--------------------------------------
    
    
    # Specify source variable
    source = c("Baseline", "Factor", "Subject", "Error", "Total", "Corrected total")
    
    # Create table
    ANOVA_table = data.frame(check.names          = FALSE, 
                             Source               = source, 
                             `Sum of squares`     = c(ss %>% select(2:5, 1) %>% unlist(), corrected_sst), 
                             `Degrees of freedom` = c(dof %>% select(2:5, 1) %>% unlist(), (n * k) - 1), 
                             `Mean squares`       = c(ms %>% select(2:5) %>% unlist(), NA, variance), 
                             `F-value`            = c(F_value_baseline, F_value_factor, NA, NA, NA, NA), 
                             `p-value`            = c(p_baseline, p_factor, NA, NA, NA, NA))
    rownames(ANOVA_table) = NULL
    
    
    # Return ANOVA-table ------------------------------------------------------
    
    
    return(list(rm_ANOVA_table = ANOVA_table))
}


# -------------------------------------------------------------------------


# Testing:
rma(rma_data, 1)

