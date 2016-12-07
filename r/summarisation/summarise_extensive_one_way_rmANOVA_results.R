##### Complete one-way repeated measures ANOVA


ow_rma_extensive = function(ow_rma_data){


# Repeated measures ANOVA ----------------------------------------------------


  ow_rma_results = ow_rma(ow_rma_data)


# Evaluate reduction of error sum of squares & corresponding plot ------------


  ow_rma_results = c(ow_rma_results, ow_rma_sse_reduct(ow_rma_data))


# Sphericity test and correction ---------------------------------------------


  ow_rma_results = c(ow_rma_results, ow_rma_spheri(ow_rma_data))


# 95% CI & corresponding error bar plot of the factor level means ------------


  ow_rma_results = c(ow_rma_results, ow_rma_ci(ow_rma_data))


# Effect size measures -------------------------------------------------------


  ow_rma_results = c(ow_rma_results, ow_rma_eta(ow_rma_data))


# Full set of orthogonal polynomial contrasts --------------------------------


  ow_rma_results = c(ow_rma_results, ow_rma_opc(ow_rma_data))


# Return extensive results ---------------------------------------------------
  
  
  return(ow_rma_results)
}

  
# ----------------------------------------------------------------------------


ow_rma_extensive(ow_rma_data)

