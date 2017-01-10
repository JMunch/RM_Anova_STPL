##### Computation of adjusted and unadjusted confidence intervalls for one-way ANOVA with repeated measurement


rma_ci = function(rma_data, C_level = 0.95, id = 1) {
  
  # 'C_level' must be larger than 0 and smaler than 1
  if (C_level >= 1 | C_level <= 0) {
    stop("C_level must be larger than 0 and smaler than 1")
  }
  # id must either be an integer specifying the column position of the independent variable
  if (id %in% 1:ncol(rma_data) == FALSE || length(id) != 1) {
    stop("id must be an integer specifying the column position of the independent variable")
  }
  
  dependent_variable = as.matrix(rma_data[, -id])
  
  
  # Libraries needed --------------------------------------------------------
  
  
  # suppress warning message about ggplot NOTE: This function still loads the package!
  suppressWarnings(suppressMessages(require(ggplot2)))
  
  
  # Define needed constants and variables -----------------------------------
  
  
  # Number of entities
  n = nrow(rma_data)
  
  # Number of factor levels
  k = ncol(dependent_variable)
  
  # Specify the names of the 'id'-variable and of the 'condition'-variables
  rm_names = colnames(dependent_variable)
  id_names = colnames(rma_data)[id]
  
  
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
  
  
  # Convert data to long format ---------------------------------------------
  
  
  rma_data_long = reshape(rma_data, varying = rm_names, v.names = "value", timevar = "condition", times = (1:k), idvar = id_names, new.row.names = 1:(k * n), 
                             direction = "long")
  colnames(rma_data_long)[1] = "id"
  rma_data_long$condition = as.numeric(rma_data_long$condition)
  
  
  # Compute means -----------------------------------------------------------
  
  
  # Factor level means
  Flm = tapply(rma_data_long$value, rma_data_long$condition, mean)
  
  # General mean
  Gm = mean(rma_data_long$value)
  
  # Entity/subject mean
  Em = tapply(rma_data_long$value, rma_data_long$id, mean)
  
  # Mean of each measurement condition
  Me = 1:k
  MeFlm = data.frame(Me, Flm)
  MeFlmlong = MeFlm[rep(seq_len(nrow(MeFlm)), each = n), ]
  
  # Mean of each entity/subject
  E = 1:n
  EEm = data.frame(E, Em)
  EEmlong = EEm[rep(seq_len(nrow(EEm)), times = k), ]
  
  
  # Compute CI for Anova without repeated measures --------------------------
  
  
  # Standard errors of the conditional means
  SE = tapply(rma_data_long$value, rma_data_long$condition, sd)/sqrt(n)
  
  # CI for ANOVA without repeated measures
  CIdist_unadj = abs(qt((1 - C_level)/2, (n - 1))) * SE
  
  
  # Compute CI for Anova without repeated measures -----------------------------
  
  
  # Compute upper and lower bound
  up_unadj = MeFlm$Flm + CIdist_unadj
  low_unadj = MeFlm$Flm - CIdist_unadj
  
  
  # Compute CI for Anova with repeated measures -----------------------------
  
  
  # Correction factor etablished by Morey (2008)
  cf = sqrt(k/(k - 1))
  
  AdjVal = data.frame(Adj = (cf * ((rma_data_long$value - EEmlong$Em + Gm) - MeFlmlong$Flm)) + MeFlmlong$Flm)
  rma_data_long_adj = cbind.data.frame(rma_data_long, AdjVal)
  
  # Standard errors of the conditional means adjusted with the method of O'Brien and Cousineau (2014, see also Loftus & Masson; 1994)
  SE_adj = (tapply(rma_data_long_adj$Adj, rma_data_long_adj$condition, sd)/sqrt(n))
  CIdist_adj = abs(qt((1 - C_level)/2, (n - 1))) * SE_adj
  
  # Compute upper and lower bound
  up_adj = MeFlm$Flm + CIdist_adj
  low_adj = MeFlm$Flm - CIdist_adj
  
  
  # CI comparison (ggplot) ----------------------------------------------------
  
  
  # create two vectors for lower ci values and upper ci values respectively
  lower = c(low_adj, low_unadj)
  upper = c(up_adj, up_unadj)
  
  # create vector that is used for facetting i.e. for assigning the correct values to each plot
  plot_nr = rep(c("Adjusted CI", "Unadjusted CI"), each = k)
  
  # create data frame for ggplot: comparison of ci
  ci_plot_data = data.frame(plot_nr, rbind(MeFlm, MeFlm), lower, upper)
  
  
  # create plot with adjusted ci values
  ci_plot = ggplot(data = ci_plot_data, aes(Me, Flm)) + geom_point(size = 2) + geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.1) + facet_grid(~plot_nr) + 
    labs(x = "Condition", y = "Value", title = "Comparison of adjusted and unadjusted (standard) confidence intervals")
  
  
  # Construct CI-tables ------------------------------------------------------------------------------
  
  
  lu_adj_CI = cbind(low_adj, up_adj)
  lu_unadj_CI = cbind(low_unadj, up_unadj)
  
  colnames(lu_adj_CI) = colnames(lu_unadj_CI) = c("Lower bound", "Upper bound")
  
  
  # Return plot and table displaying adjusted and unadjusted confidence intervals --------------------
  
  
  print(ci_plot)
  return(list(confidence_intervals = data.frame(adjusted_CI = lu_adj_CI, unadjusted_CI = lu_unadj_CI)))
}


# -------------------------------------------------------------------------


# Testing:
rma_ci(rma_data, id = 1)
