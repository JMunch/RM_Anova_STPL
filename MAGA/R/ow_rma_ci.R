#' Computation of adjusted and unadjusted confidence intervalls for one-way ANOVA with repeated measurement
#'
#' Estimate adjusted and unadjusted confidence intervals for repeated measurement ANOVA. The functions further allow to plot the confidence intervals.
#'
#' @param ow_rma_data datataframe with subject id column and k factor columns.
#'
#'
#' @return list with adj. and unadj. confidence intervals
#' \item{ow_rma_data}{The first object input.}
#' @author Sums of Suares
#' @note This functions allows to calculate the confidence intervals of a repeated measurement ANOVA.
#' @examples
#'
#' CI_list = ow_rma_ci(some_rma_data)
#'
#' # adjusted CIs
#' CI_list$adjusted_CI
#'
#' # unadjusted CIs
#'CI_list$unadjusted_CI
#'
#'
#' @rdname ow_rma_ci
#' @export

ow_rma_ci = function(ow_rma_data){


# Libraries needed --------------------------------------------------------


require(ggplot2)


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


# Compute CI for Anova without repeated measures -----------------------------


  # Compute upper and lower bound
  up_unadj = MeFlm$Flm + CIdist
  low_unadj = MeFlm$Flm - CIdist



# Compute CI for Anova with repeated measures -----------------------------


  # Correction factor etablished by Morey (2008)
  cf = sqrt(k / (k - 1))

  AdjVal = data.frame(Adj = (cf * ((ow_rma_data_long$value - EEmlong$Em + Gm) - MeFlmlong$Flm)) + MeFlmlong$Flm)
  ow_rma_data_long_adj = cbind.data.frame(ow_rma_data_long, AdjVal)

  # Standard errors of the conditional means adjusted with the method of O'Brien and Cousineau (2014, see also Loftus & Masson; 1994)
  SE_adj = (tapply(ow_rma_data_long_adj$Adj, ow_rma_data_long_adj$condition, sd) / sqrt(n))

  CIdist_adj = abs(qt((1 - Clevel)/2, (n - 1))) * SE_adj



  # Compute upper and lower bound
  up_adj = MeFlm$Flm + CIdist_adj
  low_adj = MeFlm$Flm - CIdist_adj




# ggplot CI comparison ----------------------------------------------------


  # create two vectors for lower ci values and upper ci values respectively
  lower <- c(low_adj, low_unadj)
  upper <- c(up_adj, up_unadj)

  # create vector that is used for facetting i.e. for assigning the correct values
  # to each plot
  plot_nr <- rep(c("Adjusted CI", "Unadjusted CI"), each = k)

  # create data frame for ggplot: comparison of ci
  ci_plot_data <- data.frame(plot_nr, rbind(MeFlm, MeFlm), lower, upper)


  # create plot with adjusted ci values
  ci_plot <- ggplot(data = ci_plot_data, aes(Me, Flm)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymax = upper, ymin = lower), width = .1) +
      facet_grid(~plot_nr) +
      labs(x = "Condition", y = "Value", title = "Comparison of adjusted and unadjusted (standard) confidence intervals")



# Construct CI-tables


  lu_adj_CI = cbind(low_adj, up_adj)
  colnames(lu_adj_CI) = c("Lower bound", "Upper bound")
  lu_unadj_CI = cbind(low_unadj, up_unadj)
  colnames(lu_unadj_CI) = c("Lower bound", "Upper bound")


# Return plot and table displaying adjusted and unadjusted confidence intervals --------------------

  print(ci_plot)
  return(list("adjusted_CI" = lu_adj_CI, "unadjusted_CI" = lu_unadj_CI))
}



