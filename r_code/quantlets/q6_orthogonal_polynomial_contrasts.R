##### Orthogonal polynomial contrasts in a one-way repeated measures ANOVA


ow_rma_opc = function(ow_rma_data){

    
# Define some variables ---------------------------------------------------
  
  
  # number of entities 
  n = as.numeric(length(ow_rma_data[,1]))
  
  # id-variable and condition-variable
  rm_names = colnames(ow_rma_data)[-1]
  id_names = colnames(ow_rma_data)[1]
  
  #  number of factor levels
  k = as.numeric(length(rm_names))
  
  
# Convert to long format --------------------------------------------------
  
  
  ow_rma_data_long = reshape(ow_rma_data, 
                             varying = rm_names, 
                             v.names = "value",
                             timevar = "condition", 
                             times = (1:k),
                             idvar = id_names,
                             new.row.names = 1:(k * n),
                             direction = "long"
                             )
  colnames(ow_rma_data_long)[1] = "id"
  ow_rma_data_long$condition = as.numeric(ow_rma_data_long$condition)

  
# Define some more variables ----------------------------------------------
    
  
  # factor level means
  Flm = tapply(ow_rma_data_long$value, ow_rma_data_long$condition, mean)
  
  # general mean
  Gm = mean(ow_rma_data_long$value)
  
  # entity/subject mean
  Em = tapply(ow_rma_data_long$value, ow_rma_data_long$id, mean)
  
  # Measurements
  Me = (1:k)
  
  # Mean of each measurement condition ('MeFlm' dataframe)
  MeFlm = data.frame(Me, Flm)
  MeFlmlong = MeFlm[rep(seq_len(nrow(MeFlm)), each = n),]
  
  # Entities
  E = (1:n)
  
  # Mean of each entity/subject ('EEm' dataframe)
  EEm = data.frame(E, Em)
  EEmlong = EEm[rep(seq_len(nrow(EEm)), k), ]
  
  
# Orthogonal polynomial Contrasts -----------------------------------------
  
  
  # maximal polynomial degree for orthogonal polynomials
  maxpoly = k - 1
  
  # Defining Contrast weights for orthogonal polynomial contrasts
  contrast_weights = t(contr.poly(k))
  
  # Applying formula for linear contrasts
  weighted_dependend_variables = ow_rma_data[rep(seq_len(nrow(ow_rma_data)), each = maxpoly), ][,-1] * (contrast_weights)[rep(seq_len(nrow(contrast_weights)), n), ]
  linear_subject_contrasts = matrix(rowSums(weighted_dependend_variables), byrow = TRUE, ncol = maxpoly)
  
  # Computing contrast estimators for each orthogonal polynomial contrast as well as standard errors for thees estimators
  contrast_estimator = colMeans(linear_subject_contrasts)
  contrast_se = sqrt(apply(linear_subject_contrasts,2,var)) / sqrt(n)
  
  # Computing t-values for each contrast
  contrast_t_values = contrast_estimator / contrast_se
  #contrast_F_values = contrast_t_values^2
  
  # Computing the corresponding p-values
  contrast_p_values = 1 - pt(abs(contrast_t_values), n-1)
  
  # Computing sums of squares for each contrast
  contrast_ss = n * contrast_estimator^2 / rowSums(contrast_weights^2)
  
  # Computing amount of the variance in the dependent variable explained by the factor which in turn can be explained by a cerain orthogonal polynomial trend 
  # ss_trend / ss_factor
  proportional_trend_contribution = contrast_ss / rep(sum(rep((Flm - Gm)^2, each = n)), maxpoly) # seems strange?! Is this right? --> Jap, 'sum(rep((Flm - Gm)^2, each = n)' is computing the Factor ss (its the deviation of the factor level means from grand mean, than squared, and than that times n till every subject is measured under each condition); the last 'rep(..., maxpoly)' is just to matcht the size of the 'contrast_ss' vector
  
  
# Create contrast table ---------------------------------------------------
  
  
  # define source variable
  source = rownames(contrast_weights)
  contrast_table = data.frame(check.names = FALSE,
                              "Source" = source,
                              "Sum of squares" = contrast_ss,
                              "Proportional contribution to the factor effect" = proportional_trend_contribution,
                              "Contrast estimator" = contrast_estimator,
                              "Standard error" = contrast_se,
                              "Degrees of freedom" = rep((n - 1), maxpoly),
                              "t-value" = contrast_t_values,
                              "p-value" = contrast_p_values
                              )
  rownames(contrast_table) = NULL
  
  
# Orthogonal polynomial trends as polynomial regression  ------------------
  # Used to plot the fitted polynomials
  # This is used to display the aditional explanation of the variance in the dependent variable by adding higher order trendcomponent successively 

    
  # Peperation to show the following plots (2) next to eachother
  par(mfrow=c(1,2))
  
  # Fitting the k - 1 orthogonal Polynomials
  for(i in 1:maxpoly){
    pfv = paste("poly.fit.", i, sep = "")
    assign(pfv, lm(ow_rma_data_long$value ~ poly(ow_rma_data_long$condition, degree = i, raw = TRUE)))
    poly.fit.max = lm(ow_rma_data_long$value ~ poly(ow_rma_data_long$condition, degree = i, raw = TRUE))
  }
  
  
# Plotting contrats -------------------------------------------------------
  
  
  # Plotting the predictions by the k - 1  orthogonal polynomial trends together with the raw data                                      
  plot(ow_rma_data_long$condition, ow_rma_data_long$value, main = "raw data", xlab = "condition", ylab = "value")                  
  for(i in 1:maxpoly){
    lines(smooth.spline(ow_rma_data_long$condition, predict(lm(ow_rma_data_long$value ~ poly(ow_rma_data_long$condition, degree = i, raw = FALSE)))), col = i, lwd = 2)
  }
  
  ## !!! plot koennte noch verschoenert werden (Tietel, Beschriftung, Farben, etc.)
  ## !!! das sollten eigentlich smooth lines sein... und nicht die Predictions fuer die einzelnen Levels verbunden mit linien...
  
  
  # Plotting the predictions by the k - 1  orthogonal polynomial trends together with the conditional means 
  plot(MeFlm, main = "conditional means", xlab = "condition", ylab = "value")
  for(i in 1:maxpoly){
    lines(smooth.spline(ow_rma_data_long$condition, predict(lm(ow_rma_data_long$value ~ poly(ow_rma_data_long$condition, degree = i, raw = FALSE)))), col = i, lwd = 2)
  }
  
  ## !!! plot koennte noch verschoenert werden (Titel, Beschriftung, Farben, etc.)
  
  ## !!! das sollten eigentlich smooth lines sein... und nicht die Predictions fuer die einzelnen Levels verbunden mit linien... 
  ## !!! also ein funktionsgraph wäre gut

  
# Return the contrast-table -----------------------------------------------
    
  
  return(list("orthogonal_polynomial_contrast_table" = contrast_table))
}


# -------------------------------------------------------------------------

