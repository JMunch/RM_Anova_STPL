##### Computation of adjusted and unadjusted confidence intervalls for one-way ANOVA with repeated measurement


ow_rma_ci = function(ow_rma_data){

    
# Libraries needed --------------------------------------------------------

  
  ##require(ggplot2) # is not needed yet but will soon :)

  
# Define needed constants and variables -----------------------------------


  # Number of entities 
  n = nrow(ow_rma_data)

  # Specify the names of the 'id'-variable and of the 'condition'-variables
  rm_names = colnames(ow_rma_data)[-1]
  id_names = colnames(ow_rma_data)[1]

  # Number of factor levels
  k = length(rm_names)


# Convert data to long format ---------------------------------------------


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


# Compute means -----------------------------------------------------------


  # Factor level means
  Flm = tapply(ow_rma_data_long$value, ow_rma_data_long$condition, mean)

  # General mean
  Gm = mean(ow_rma_data_long$value)

  # Entity/subject mean
  Em = tapply(ow_rma_data_long$value, ow_rma_data_long$id, mean)

  # Mean of each measurement condition
  Me = 1:k
  MeFlm = data.frame(Me, Flm)
  MeFlmlong = MeFlm[rep(seq_len(nrow(MeFlm)), each = n),]
  
  # Mean of each entity/subject
  E = 1:n
  EEm = data.frame(E, Em)
  EEmlong = EEm[rep(seq_len(nrow(EEm)), k), ]


# Compute CI for Anova without repeated measures --------------------------


  # Confidence level
  Clevel = 0.95 
   
  # Standard errors of the conditional means
  SE = tapply(ow_rma_data_long$value, ow_rma_data_long$condition, sd) / sqrt(n)
  
  # CI for ANOVA without repeated measures 
  CIdist = abs(qt((1 - Clevel)/2, (n - 1))) * SE


# Plot CI for Anova without repeated measures -----------------------------


  # Peperation to show the following plots next to eachother
  par(mfrow=c(1,2))
  
  # Plot condtional means with CI error bars
  barends = 0.05
  
  plot(MeFlm, ylim = c(min((MeFlm$Flm - CIdist)), max((MeFlm$Flm + CIdist))), main = "Unadjusted CI", xlab = "condition", ylab = "value")
  
  # Compute upper and lower bound
  up_unadj = MeFlm$Flm + CIdist 
  low_unadj = MeFlm$Flm - CIdist 
  
  for(i in 1:k) {
    segments(MeFlm$Me[i],low_unadj[i] , MeFlm$Me[i], up_unadj[i])
    segments(MeFlm$Me[i]-barends, up_unadj[i] , MeFlm$Me[i]+barends, up_unadj[i])
    segments(MeFlm$Me[i]-barends, low_unadj[i] , MeFlm$Me[i]+barends, low_unadj[i])
  }

  ## !!! Auch hier muesste der Plot noch verschoenert werden 
  ## !!! Alternative... habs aber nicht hinbekommen fuer verschiedene CI pro mean
  ##segments(MeFlm$Me, MeFlm$Flm - CIdist, MeFlm$Me, MeFlm$Flm + CIdist)
  ##segments(MeFlm$Me - barends, MeFlm$Flm - CIdist, MeFlm$Me + barends, MeFlm$Flm - CIdist)
  ##segments(MeFlm$Me - barends, MeFlm$Flm + CIdist, MeFlm$Me + barends, MeFlm$Flm + CIdist)


# Compute CI for Anova with repeated measures -----------------------------


  # Correction factor etablished by Morey (2008)
  cf = sqrt(k / (k - 1))

  AdjVal = data.frame(Adj = (cf * ((ow_rma_data_long$value - EEmlong$Em + Gm) - MeFlmlong$Flm)) + MeFlmlong$Flm)
  ow_rma_data_long_adj = cbind.data.frame(ow_rma_data_long, AdjVal)

  # Standard errors of the conditional means adjusted with the method of O'Brien and Cousineau (2014, see also Loftus & Masson; 1994)
  SE_adj = (tapply(ow_rma_data_long_adj$Adj, ow_rma_data_long_adj$condition, sd) / sqrt(n))
  
  CIdist_adj = abs(qt((1 - Clevel)/2, (n - 1))) * SE_adj


# Plot CI for Anova with repeated measures --------------------------------


  # Plot of condtional means with adjusted CI error bars
  barends = 0.05
  plot(MeFlm, ylim = c(min((MeFlm$Flm - CIdist_adj)), max((MeFlm$Flm + CIdist_adj))), main = "Adjusted CI", xlab = "condition", ylab = "value")
  
  # Compute upper and lower bound
  up_adj = MeFlm$Flm + CIdist_adj
  low_adj = MeFlm$Flm - CIdist_adj
  
  for(i in 1:k) {
    segments(MeFlm$Me[i],low_adj[i] , MeFlm$Me[i], up_adj[i])
    segments(MeFlm$Me[i]-barends, up_adj[i] , MeFlm$Me[i]+barends, up_adj[i])
    segments(MeFlm$Me[i]-barends, low_adj[i] , MeFlm$Me[i]+barends, low_adj[i])
  }

  ## !!! Auch hier muesste der Plot noch verschoenert werden 
  ## !!! Alternative... habs aber nicht hinbekommen fuer verschiedene CI pro mean
  ##segments(MeFlm$Me, MeFlm$Flm - CIdist_adj, MeFlm$Me, MeFlm$Flm + CIdist_adj)
  ##segments(MeFlm$Me - barends, MeFlm$Flm - CIdist_adj, MeFlm$Me + barends, MeFlm$Flm - CIdist_adj)
  ##segments(MeFlm$Me - barends, MeFlm$Flm + CIdist_adj, MeFlm$Me + barends, MeFlm$Flm + CIdist_adj)


  ## !!! Litle extra plot
  ## !!! Horizontal line are the condition means / 'X' are the subject means 
  ##par(mfrow=c(1,1))
  ##matplot(ow_rma_data[2:(k + 1)], type = c("b"), pch = ".", col = 1:k)
  ##points(Em, cex = 2, pch = 4)
  ##abline(h = Flm, col = 1:k)

  
# Construct CI-tables
  
  
  lu_adj_CI = cbind(low_adj, up_adj)
  colnames(lu_adj_CI) = c("Lower bound", "Upper bound")
  lu_unadj_CI = cbind(low_unadj, up_unadj)
  colnames(lu_unadj_CI) = c("Lower bound", "Upper bound")
  

# Return adjusted and unadjusted confidence intervalls --------------------
  
  
  return(list("adjusted_CI" = lu_adj_CI, "unadjusted_CI" = lu_unadj_CI))
}


# -------------------------------------------------------------------------

