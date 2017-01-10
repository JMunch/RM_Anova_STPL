# load packages
library(xtable)

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

#rma_data = sim_rma_data(n = 30, k = 5, means = means, poly_order = NULL, noise_sd = c(155, 65, 75, 15, 40), between_subject_sd = 60, NAs = 2) 
#rma_data = (rma_data  + 100) / 2
rma_data = read.csv(file = "r_code/simulated_rigelmann_data.csv")[,-2]
rma_data[,2:6] = (rma_data[,2:6]  + 100) / 2

# rma function
rma_table = rma(rma_data)
xtable(rma_table$rm_ANOVA_table)

# confidence intervals
cis = rma_ci(rma_data, C_level = 0.99, print_plot = FALSE)
xtable(cis$confidence_intervals)
ggsave(file = "ci_plot.jpg", plot = cis$ci_plot, device = "jpg")

# eta function
etas = rma_eta(rma_data)
xtable(etas)

# orthogonal polynomials
opcs = rma_opc(rma_data)
xtable(opcs$contrast_table)

# sphericity
spher = rma_spheri(rma_data, append = TRUE)
xtable(spher$mauchly_table)
xtable(spher$correction_factors_epsilon_table)

# comparison of SSEs
sse = rma_sse_reduct(rma_data)
xtable(sse$sse_reduction_table)
#write.csv(x = ow_rma_data, file = "r_code/simulated_rigelmann_data.csv")

