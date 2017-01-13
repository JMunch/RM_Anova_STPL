# Load all quantlets
source("r_code/quantlets/q1_one_way_rmANOVA.R")
source("r_code/quantlets/q2_evaluate_reduction_of_sse.R")
source("r_code/quantlets/q3_test_and_adjustment_for_sphericity.R")
source("r_code/quantlets/q4_adjusted_and_unadjusted_CI.R")
source("r_code/quantlets/q5_effect_size.R")
source("r_code/quantlets/q6_orthogonal_polynomial_contrasts.R")
source("r_code/quantlets/q7_simulate_one_way_rmANOVA_data.R")

set.seed(1)
# Simulate data for report and presentation
means = c(400, 170, 55, 45, 40)

ow_rma_data = sim_ow_rma_data(n = 30, k = 5, means = means, poly_order = NULL, noise_sd = c(155, 65, 75, 15, 40), between_subject_sd = 60, NAs = 2) 
ow_rma_data[,2:5] = (ow_rma_data[,2:5]  + 100) / 2
#ow_rma_data = read.csv("r_code/simulated_rigelmann_data.csv")
ow_rma(ow_rma_data)
ow_rma_ci(ow_rma_data, C_level = 0.99)
ow_rma_eta(ow_rma_data)
ow_rma_opc(ow_rma_data)
ow_rma_spheri(ow_rma_data, append = TRUE)
ow_rma_sse_reduct(ow_rma_data, ow_a_table = TRUE)

#write.csv(x = ow_rma_data, file = "r_code/simulated_rigelmann_data.csv")

View(ow_rma_data)
