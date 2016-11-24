
effect_size = function(rma_res){

# Define needed constants and variables -----------------------------------


SS_Factor = rma_res[2,2]
SS_Error = rma_res[4, 2]
SS_Total = rma_res[5,2]


# Compute effect size measures --------------------------------------------


# Compute standard eta^2
eta_sq = SS_Factor / SS_Total 

# Compute partial eta^2
eta_partial = SS_Factor / (SS_Factor + SS_Error)


# Return list with effect sizes -------------------------------------------


return(list("eta_sq" = eta_sq, "eta_partial" = eta_partial))
}



# I am not sure what our the point of this quantlet should be? !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# --------------------------------------------------------------
# Testing:

source("r/simulate_rma_data.R")
rma_data = sim_rma_data(10, 5)

source("r/quantlet1_rm_anova.R")
rma_res = rma(rma_data = rma_data)

effect_size(rma_res = rma_res)
