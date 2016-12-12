##### Mauchly's Test of Sphericity and the appropriate correction factors (epsilon)
  # Function 'ow_rma' is required!
  # To conduct the Mauchly's Test of Sphericity (Mauchly, 1940), the procedure described by Huynh and Feldt (1970) is used


ow_rma_spheri = function(ow_rma_data, append = FALSE){
  
  
# Defining some variables -------------------------------------------------

  
  # number of factor levels
  k = length(ow_rma_data[1,]) - 1
  
  # number of entities
  n = as.numeric(length(ow_rma_data[,1]))
  
  # Factor degrees of freedom
  df = k - 1

  # Empirical covariance matrix
  covariance_matix = cov(as.matrix(ow_rma_data[,-1]))


# Check whether a test for sphericity is needed
  # Note that there can't be a violation of sphericity since the factor of the one-way ANOVA has only two factor levels
  
  
  if (k > 2){
  
    
# Helmert matrix required for the computation of mauchly's W --------------


    helmert = function(k){
      H = matrix(0, k, k)
      diag(H) = (0:df) * (-((0:df) * ((0:df) + 1))^(-0.5))
      for (i in 2:k){
        H[i,1:(i - 1)] = ((i - 1)*(i))^(-0.5)
      }
      # H[1,] = 1/sqrt(k)
      return(H)
    }

    # The first row of the helmert matrix is not required for further use in this procedure
    C = helmert(k)[-1,]


# Computation of mauchly's W ----------------------------------------------


    w = det(C%*%covariance_matix%*%t(C)) / ((sum(diag(C%*%covariance_matix%*%t(C))) / df)^(df))


# Chi-Square Test ---------------------------------------------------------


    # Computing the degrees of freedom for the chi-square value 
    df_w = ((k * df) / 2) - 1
  
    # Computing the chi-square value
    f = 1 - ((2 * (df^2) + df + 2) / (6 * df * (n - 1)))
    chi_sq_w = -(n - 1) * f * log(w)
  
    # Computing the corresponding p-value
    p_w = 1 - pchisq(chi_sq_w, df_w)


# Create summary table for mauchly's test ---------------------------------

  
    mauchly_table = data.frame(check.names = FALSE,
                               "Source" = "Factor",
                               "Mauchly's W" = w,
                               "Chi square" = chi_sq_w,
                               "df" = df_w,
                               "p" = p_w
                               )
    rownames(mauchly_table) = NULL


# The three different correction factors (epsilon) ------------------------
  # Computing the three different correction factors for the nominator/denominator df of the rmANOVA factor F-test (Box, 1954a; Box, 1954b; Geisser & Greenhouse, 1958; Greenhouse & Geisser, 1959; Huynh & Feldt, 1976) )

    
    # Lower-Bound correction (Greenhouse & Geisser, 1959)
    epsilon_lb = 1 / df
    
    # Box correction (Geisser & Greenhouse, 1958)
    epsilon_gg = (sum(diag(C%*%covariance_matix%*%t(C)))^2) / (df * sum(diag(t((C%*%covariance_matix%*%t(C)))%*%(C%*%covariance_matix%*%t(C)))))
    
    # Huynh-Feldt correction (Huynh & Feldt, 1976)
    epsilon_hf = min((n * df * epsilon_gg - 2) / (df * (n - 1) - (df^2 * epsilon_gg)), 1)
  
  
# Computing the adjusted p-values for the ANOVA-factor F-test -------------
  # The adjustment is realised via correction of the F-distribution degrees of freedom by multplication with the respective correction factor (epsilon)
  # rmANOVA function 'ow_rma' is required!  
  
  
    ANOVA_table = ow_rma(ow_rma_data)[[1]]
    
    corrected_factor_df = ANOVA_table[2,3] * epsilon_lb
    corrected_error_df = ANOVA_table[2,3] * epsilon_lb
    p_factor_lb = 1 - pf(ANOVA_table[2,5], corrected_factor_df, corrected_error_df)
    
    corrected_factor_df = ANOVA_table[2,3] * epsilon_gg
    corrected_error_df = ANOVA_table[2,3] * epsilon_gg
    p_factor_gg = 1 - pf(ANOVA_table[2,5], corrected_factor_df, corrected_error_df)
    
    corrected_factor_df = ANOVA_table[2,3] * epsilon_hf
    corrected_error_df = ANOVA_table[2,3] * epsilon_hf
    p_factor_hf = 1 - pf(ANOVA_table[2,5], corrected_factor_df, corrected_error_df)
    
  
# Table with adjusted p-values and respective corection factors -----------
  
  
    epsilon_table = data.frame(check.names = FALSE,
                               "Source" = c("Epsilon", "Adjusted p-Value"),
                               "Lower-Bound correction (Greenhouse & Geisser, 1959)" = c(epsilon_lb, p_factor_lb),
                               "Box correction (Geisser & Greenhouse, 1958)" = c(epsilon_gg, p_factor_gg),
                               "Huynh-Feldt correction (Huynh & Feldt, 1976)" = c(epsilon_hf, p_factor_hf)
                               )
    rownames(epsilon_table) = NULL

  
# Coose recomendet adjustment and add to ANOVA-table ----------------------
  # Correction is only applied if assumption of sphericity is rejected 
  # Recomendation (e.g. Girden, 1992) based on results of Greenhouse and Geisser (1959) as well as Huynh and Feldt (1976)

    
    if (p_w < .05){
      if (p_factor_lb < .05){
        ANOVA_table[,"Recommended Lower-Bound corrected p-Value (Greenhouse & Geisser, 1959))"] = c(NA, p_factor_lb, NA, NA, NA, NA)
      }else{
        if (epsilon_gg < .75){
          ANOVA_table[,"Recommended Box corrected p-Value (Geisser & Greenhouse, 1958)"] = c(NA, p_factor_gg, NA, NA, NA, NA)
        }else{
          ANOVA_table[,"Recommended Huynh-Feldt corrected p-Value (Huynh & Feldt, 1976)"] = c(NA, p_factor_hf, NA, NA, NA, NA)
        }
      }
    }


# Return spericity test and p-value adjustment ----------------------------
  # If append is 'TRUE' the function will also return the ANOVA-table with the recomendet adjustment of the p-value  
  
      
    if (append == TRUE){
      return(list("mauchly_test_table" = mauchly_table, "correction_factors_epsilon_table" = epsilon_table, "corrected_one_way_repeated_measures_ANOVA_table" = ANOVA_table))
    }else{
      return(list("mauchly_test_table" = mauchly_table, "correction_factors_epsilon_table" = epsilon_table))
    }
  
  
# Return notification if no test is required
    
    
  }else{
    no_test_notification = "Note that there can't be a violation of sphericity since the factor of the one-way ANOVA has only two factor levels"
    return(list("Caution" = no_test_warning))
  }
}  


# -------------------------------------------------------------------------


# Testing: 
ow_rma_spheri(ow_rma_data, append = TRUE)

