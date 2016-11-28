##### Mauchly's Test of Sphericity and correction factors (epsilon)

# To conduct the Mauchly's Test of Sphericity (Mauchly, 1940), the procedure described by Huynh and Feldt (1970) is used

# defining some variables

k = length(rma_data[1,]) - 1
n = as.numeric(length(rma_data[,1]))
maxpoly = k - 1


# Empirical covariance matrix

covariance_matix = cov(as.matrix(rma_data[,-1]))


# Helmert matrix required for the computation of mauchly's w

helmert = function(k){
        H = matrix(0, k, k)
        diag(H) = (0:(k - 1)) * (-((0:(k - 1)) * ((0:(k - 1)) + 1))^(-0.5))
        for (i in 2:k){
                H[i,1:(i - 1)] = ((i - 1)*(i))^(-0.5)
        }
#       H[1,] = 1/sqrt(k)
#       The first row of the helmert matrix is not required for further use in this procedure
        H
}

C = helmert(k)[-1,]


# Computation of mauchly's w

w = det(C%*%covariance_matix%*%t(C)) / ((sum(diag(C%*%covariance_matix%*%t(C))) / maxpoly)^(maxpoly))


# Computing the degrees of freedom for the chi-square value 

df_w = ((k * (k - 1)) / 2) - 1


# Computing the chi-square value

f = 1 - ((2 * ((k - 1)^2) + (k - 1) + 2) / (6 * (k - 1) * (n - 1)))
chi_sq_w = -(n - 1) * f * log(w)


# Computing the corresponding p-value

p_w = 1 - pchisq(chi_sq_w, df_w)


# create summary table for mauchly's test

mauchly_table = data.frame("Source" = "Factor",
                              "Mauchly's W" = w,
                              "Chi square" = chi_sq_w,
                              "df" = df_w,
                              "p" = p_w
)
rownames(mauchly_table) = NULL


# Computing the three different correction factors for the nominator/denominator df of the rmANOVA factor F-test (Box, 1954a; Box, 1954b; Geisser & Greenhouse, 1958; Greenhouse & Geisser, 1959; Huynh & Feldt, 1976) )

epsilon_lb = 1 / (k - 1)

epsilon_gg = (sum(diag(C%*%covariance_matix%*%t(C)))^2) / (maxpoly * sum(diag(t((C%*%covariance_matix%*%t(C)))%*%(C%*%covariance_matix%*%t(C)))))

epsilon_hf = min((n * maxpoly * epsilon_gg - 2) / (maxpoly * (n - 1) - (maxpoly^2 * epsilon_gg)), 1)


# Computing the adjusted p-values for the ANOVA-factor F-test (rmANOVA function 'rma' is required)  

ANOVA_table = rma(rma_data)

corrected_factor_df = ANOVA_table[2,3] * epsilon_lb
corrected_error_df = ANOVA_table[2,3] * epsilon_lb
p_factor_lb = 1 - pf(ANOVA_table[2,5], corrected_factor_df, corrected_error_df)

corrected_factor_df = ANOVA_table[2,3] * epsilon_gg
corrected_error_df = ANOVA_table[2,3] * epsilon_gg
p_factor_gg = 1 - pf(ANOVA_table[2,5], corrected_factor_df, corrected_error_df)

corrected_factor_df = ANOVA_table[2,3] * epsilon_hf
corrected_error_df = ANOVA_table[2,3] * epsilon_hf
p_factor_hf = 1 - pf(ANOVA_table[2,5], corrected_factor_df, corrected_error_df)


# Table with adjusted p-values

epsilon_table = data.frame("Source" = c("Epsilon", "Adjusted p-Value"),
                                     "Lower-Bound correction (Greenhouse & Geisser, 1959)" = c(epsilon_lb, p_factor_lb),
                                     "Box correction (Geisser & Greenhouse, 1958)" = c(epsilon_gg, p_factor_gg),
                                     "Huynh-Feldt correction (Huynh & Feldt, 1976)" = c(epsilon_hf, p_factor_hf)
)
rownames(epsilon_table) = NULL


# Coose recomendet adjustment and add to ANOVA-table    

if (p_w < 1){
        if (p_factor_lb < .05){
              ANOVA_table[,"Recommended Lower-Bound corrected p-Value (Greenhouse & Geisser, 1959))"] = c(NA, p_factor_lb, NA, NA, NA, NA)
        }else{
              if (p_factor_lb > .05){
                    if (epsilon_gg < .75){
                          ANOVA_table[,"Recommended Box corrected p-Value (Geisser & Greenhouse, 1958)"] = c(NA, p_factor_gg, NA, NA, NA, NA)
                    }else{
                          ANOVA_table[,"Recommended Huynh-Feldt corrected p-Value (Huynh & Feldt, 1976)"] = c(NA, p_factor_hf, NA, NA, NA, NA)
                    }
              }
        }
}

