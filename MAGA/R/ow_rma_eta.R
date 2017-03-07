#' Effect size measures for a one-way repeated measures ANOVA
#'
#' Compute eta-square and partial eta_square for repeated measures ANOVA. Partial eta square is the effect size for the total variance reduced by the inter-subject variance (as done in a repeated measures ANOVA model)
#'
#' @param rma_data An object of type data.frame. Each row should represent one subject and each column one variable.
#' @param id An integer specifying the column position of the subject ID. Default is 1. Set to "none" if the data does not contain an ID variable.
#' @param append Logical. If FALSE only the effect sizes. If TRUE, an ANOVA table augmented with effect sizes is returned aditionally. Default is FALSE.
#'
#' @return Returns an object of type list.
#' \item{effect_size_table}{An object of type data.frame containing the effect sizes eta square and partial eta square}
#' \item{anova_table}{An object of type data.frame. Contains the anova table augmented with the effect size measures}
#' @author Joachim Munch, Frederik Schreck, Quang Nguyen Duc, Constantin Meyer-Grant, Nikolas Hoeft
#' @note References:
#'
#' Bakeman, R. (2005). Recommended effect size statistics for repeated measures designs, Behavior research methods 37(3): 379–384.
#'
#' Cousineau, D. (2005). Confidence intervals in within-subject designs: A simpler solution to loftus and masson’s method, Tutorials in quantitative methods for psychology 1(1): 42–45.
#'
#' @examples
#'
#' rma_eta(some_rma_data, id = 1, append = FALSE)
#'
#' @rdname rma_eta
#' @export


##### Effect size measures for a one-way repeated measures anova Function 'rma' is required!


rma_eta = function(rma_data, id = 1, append = FALSE) {

  # check if the data meet the requirements ---------------------------------

  # rma_data needs to meet the following requirements:

  # id must be an integer specifying the column position of the ID variable
  if (id %in% 1:ncol(rma_data) == FALSE || length(id) != 1) {
    stop("id must be an integer specifying the column position of the ID variable")
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
