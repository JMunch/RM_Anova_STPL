##### Mauchly's Test of Sphericity and the appropriate correction factors (epsilon) Function 'rma' is required!  To conduct the Mauchly's
##### Test of Sphericity (Mauchly, 1940), the procedure described by Huynh and Feldt (1970) is used


rma_spheri = function(rma_data, id = 1, append = FALSE) {
    
    # id must either be an integer specifying the column position of the independent variable
    if (id %in% 1:ncol(rma_data) == FALSE || length(id) != 1) {
        stop("id must be an integer specifying the column position of the independent variable")
    }
    
    dependent_variable = as.matrix(rma_data[, -id])
    
    
    # Defining some variables -------------------------------------------------
    
    # number of factor levels
    k = ncol(dependent_variable)
    
    # number of entities
    n = nrow(rma_data)
    
    # Factor degrees of freedom
    df = k - 1
    
    # Empirical covariance matrix
    covariance_matix = cov(dependent_variable)
    
    
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
    
    # Check whether a test for sphericity is needed
    if (k == 2) {
        stop("Note that there can't be a violation of sphericity since the factor of the one-way anova has only two factor levels")
    }
    
    
    # Helmert matrix required for the computation of mauchly's W --------------
    
    helmert = function(k, df) {
        H       = matrix(0, k, k)
        diag(H) = (0:df) * (-((0:df) * ((0:df) + 1))^(-0.5))
        
        for (i in 2:k) {
            H[i, 1:(i - 1)] = ((i - 1) * (i))^(-0.5)
        }
        
        # H[1,] = 1/sqrt(k)
        return(H)
    }
    
    # The first row of the helmert matrix is not required for further use in this procedure
    C = helmert(k, df)[-1, ]
    
    
    # Computation of mauchly's W ----------------------------------------------
    
    w = det(C %*% covariance_matix %*% t(C))/((sum(diag(C %*% covariance_matix %*% t(C)))/df)^(df))
    
    
    # Chi-Square Test ---------------------------------------------------------
  
    # Computing the degrees of freedom for the chi-square value
    df_w = ((k * df)/2) - 1
    
    # Computing the chi-square value
    f        = 1 - ((2 * (df^2) + df + 2) / (6 * df * (n - 1)))
    chi_sq_w = -(n - 1) * f * log(w)
    
    # Computing the corresponding p-value
    p_w = 1 - pchisq(chi_sq_w, df_w)
    
    
    # Create summary table for mauchly's test ---------------------------------
  
    mauchly_table = data.frame(check.names   = FALSE, 
                               Source        = "Factor", 
                               `Mauchly's W` = w, 
                               `Chi square`  = chi_sq_w, 
                               df            = df_w, 
                               p             = p_w)
    rownames(mauchly_table) = NULL
    
    
    # The three different correction factors (epsilon) ------------------------ 
    # Computing the three different correction factors for the nominator/denominator df of the rmanova factor F-test 
    # (Box, 1954; Geisser & Greenhouse, 1958; Greenhouse & Geisser, 1959; Huynh & Feldt, 1976)
    
    # Lower-Bound correction (Greenhouse & Geisser, 1959)
    epsilon_lb = 1/df
    
    # Box correction (Geisser & Greenhouse, 1958)
    epsilon_gg = (sum(diag(C %*% covariance_matix %*% t(C)))^2) / 
        (df * sum(diag(t((C %*% covariance_matix %*% t(C))) %*% (C %*% covariance_matix %*% t(C)))))
    
    # Huynh-Feldt correction (Huynh & Feldt, 1976)
    epsilon_hf = min((n * df * epsilon_gg - 2) / (df * (n - 1) - (df^2 * epsilon_gg)), 1)
    
    
    # Computing the adjusted p-values for the anova-factor F-test ------------- 
    # The adjustment is realised via correction of the F-distribution degrees of freedom by multplication with the 
    # respective correction factor (epsilon) rmanova function 'rma' is required!
    
    anova_table         = rma(rma_data)[[1]]
    
    corrected_factor_df = anova_table[2, 3] * epsilon_lb
    corrected_error_df  = anova_table[2, 3] * epsilon_lb
    p_factor_lb         = 1 - pf(anova_table[2, 5], corrected_factor_df, corrected_error_df)
    
    corrected_factor_df = anova_table[2, 3] * epsilon_gg
    corrected_error_df  = anova_table[2, 3] * epsilon_gg
    p_factor_gg         = 1 - pf(anova_table[2, 5], corrected_factor_df, corrected_error_df)
    
    corrected_factor_df = anova_table[2, 3] * epsilon_hf
    corrected_error_df  = anova_table[2, 3] * epsilon_hf
    p_factor_hf         = 1 - pf(anova_table[2, 5], corrected_factor_df, corrected_error_df)
    
    
    # Table with adjusted p-values and respective corection factors -----------
    
    epsilon_table = data.frame(check.names = FALSE, 
                               Source      = c("Epsilon", "Adjusted p-Value"), 
                               `Lower-Bound correction (Greenhouse & Geisser, 1959)` = c(epsilon_lb, p_factor_lb), 
                               `Box correction (Geisser & Greenhouse, 1958)`         = c(epsilon_gg, p_factor_gg), 
                               `Huynh-Feldt correction (Huynh & Feldt, 1976)`        = c(epsilon_hf, p_factor_hf))
    rownames(epsilon_table) = NULL
    
  
    # Choose recommended adjustment and add to anova-table ---------------------- 
    # Correction is only applied if assumption of sphericity is rejected Recomendation (e.g.  Girden, 1992) 
    # based on results of Greenhouse and Geisser (1959) as well as Huynh and Feldt (1976)
    
    if (p_w < 0.05) {
        if (p_factor_lb < 0.05) {
            anova_table[, "Recommended Lower-Bound corrected p-Value (Greenhouse & Geisser, 1959))"] = c(NA, p_factor_lb, NA, NA, NA, 
                NA)
        } else {
            if (epsilon_gg < 0.75) {
                anova_table[, "Recommended Box corrected p-Value (Geisser & Greenhouse, 1958)"] = c(NA, p_factor_gg, NA, NA, NA, NA)
            } else {
                anova_table[, "Recommended Huynh-Feldt corrected p-Value (Huynh & Feldt, 1976)"] = c(NA, p_factor_hf, NA, NA, NA, NA)
            }
        }
    }
    
    
    # Return spericity test and p-value adjustment ---------------------------- 
    # If append is 'TRUE' the function will also return the anova-table with the recomendet adjustment of the p-value
    
    if (append == TRUE) {
        return(list(mauchly_table                    = mauchly_table, 
                    correction_factors_epsilon_table = epsilon_table, 
                    corrected_anova_table            = anova_table))
    } else {
        return(list(mauchly_test_table               = mauchly_table, 
                    correction_factors_epsilon_table = epsilon_table))
    }
}
