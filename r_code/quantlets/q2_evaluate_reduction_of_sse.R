##### Evaluate reduction of error sum of squares due to the consideration of variance between entities Function 'ow-rma' is required!


ow_rma_sse_reduct = function(ow_rma_data, independent_var = 1, plot_type = "pie", ow_a_table = FALSE) {
    
    
    # check if the data meet the requirements ---------------------------------
    
    
    # ow_rma_data needs to meet the following requirements:
    
    # independent_var must either be an integer specifying the column position of the independent variable
    if (independent_var %in% 1:ncol(ow_rma_data) == FALSE || length(independent_var) != 1) {
        stop("independent_var must be an integer specifying the column position of the independent variable")
    }
    
    # all variables must be numeric
    if (all(sapply(ow_rma_data, is.numeric)) == FALSE | any(sapply(ow_rma_data, is.factor))) {
        stop("All variables in ow_rma_data must be numeric")
    }
    
    # n > k (i.e. more entities than factor levels)
    if (nrow(ow_rma_data) <= (ncol(ow_rma_data) - 1)) {
        stop("Number of entities must exceed number of factor levels")
    }
    
    # k >= 2 (i.e. at least two or more factor levels)
    if ((ncol(ow_rma_data) - 1) < 2) {
        stop("At least two factor factor levels required")
    }
    
    
    # Libraries needed ----------------------------------------------------------
    
    
    suppressWarnings(suppressMessages(require(dplyr)))
    suppressWarnings(suppressMessages(require(ggplot2)))
    
    
    # Computaion of ANOVA model -------------------------------------------------
    
    # Note that the one-way ANOVA without repeated measures is for illustration purposes only since the data structure is correlated across the factor levels because of
    # the dependent measurements.
    
    # The ANOVA without repeated measures treates the data as if they are independend i.e. as if there are different entities in each group, which is in fact not the
    # case.
    
    
    ow_a = function(ow_rma_data, independent_var) {
        
        
        # Define needed constants and the dependent variable ------------------------
        
        
        dependent_variable = as.matrix(ow_rma_data[, -independent_var])
        
        # Number of entities in one group
        n_group = nrow(ow_rma_data)
        
        # Number of factor levels
        k = ncol(dependent_variable)
        
        # Number of entities
        n = (k * n_group)
        
        
        
        # Define basic ANOVA components ---------------------------------------------
        
        
        grand_mean = mean(dependent_variable)
        baseline_components = matrix(grand_mean, nrow = n_group, ncol = k)
        
        conditional_means = colMeans(dependent_variable)
        factor_level_components = matrix(conditional_means - grand_mean, nrow = n_group, ncol = k, byrow = TRUE)
        
        
        
        # Computation of the error component
        error_components_ANOVA = dependent_variable - baseline_components - factor_level_components
        
        
        # Construct decomposition matrix ---------------------------------------------- Matrix with n rows and 4 columns One column for: original values, factor level
        # component, error component
        
        
        decomposition_matrix_ANOVA = data.frame(dependent_variable = as.vector(dependent_variable), baseline = as.vector(baseline_components), factor_level = as.vector(factor_level_components), 
            error = as.vector(error_components_ANOVA))
        
        
        
        # Compute sums of squares ---------------------------------------------------
        
        
        ss_ANOVA = as.data.frame(t(colSums(decomposition_matrix_ANOVA^2)))
        rownames(ss_ANOVA) = "sums_of_squares"
        
        
        # Set degrees of freedom ---------------------------------------------------- Different dof's for the ss_error than in rmANOVA
        
        
        dof_ANOVA = data.frame(dependent_variable = n, baseline = 1, factor_level = (k - 1), error = (n - k))
        
        
        # Compute mean squares ------------------------------------------------------
        
        
        ms_ANOVA = ss_ANOVA/dof_ANOVA
        rownames(ms_ANOVA) = "mean_squares"
        
        
        # Calculate the F-Value -----------------------------------------------------
        
        
        F_value_factor_ANOVA = ms_ANOVA$factor_level/ms_ANOVA$error
        F_value_baseline_ANOVA = ms_ANOVA$baseline/ms_ANOVA$error
        
        
        # Set p-value of F-distribution ---------------------------------------------
        
        
        p_factor_ANOVA = 1 - pf(F_value_factor_ANOVA, dof_ANOVA$factor_level, dof_ANOVA$error)
        p_baseline_ANOVA = 1 - pf(F_value_baseline_ANOVA, dof_ANOVA$baseline, dof_ANOVA$error)
        
        
        # Compute corrected total sum of squares (variance) -------------------------
        
        
        corrected_sst = ss_ANOVA$dependent_variable - ss_ANOVA$baseline
        variance = corrected_sst/(dof_ANOVA$dependent_variable - dof_ANOVA$baseline)
        
        
        # Create the output table --------------------------------------------------- Corrected total sum of squares could be taken from the repeated measures ANOVA-Model as
        # they coincide
        
        
        # Specify source variable
        source_ANOVA = c("Baseline", "Factor", "Error", "Total", "Corrected total")
        
        # Create table
        ANOVA_table = data.frame(check.names = FALSE, Source = source_ANOVA, `Sum of squares` = c(ss_ANOVA %>% select(2:4, 1) %>% unlist(), corrected_sst), `Degrees of freedom` = c(dof_ANOVA %>% 
            select(2:4, 1) %>% unlist(), (n - 1)), `Mean squares` = c(ms_ANOVA %>% select(2:4) %>% unlist(), NA, variance), `F-value` = c(F_value_baseline_ANOVA, F_value_factor_ANOVA, 
            NA, NA, NA), `p-value` = c(p_baseline_ANOVA, p_factor_ANOVA, NA, NA, NA))
        rownames(ANOVA_table) = NULL
        
        
        # Return ANOVA-table --------------------------------------------------------
        
        
        return(list(ANOVA_table_without_repeaded_measures = ANOVA_table))
    }
    
    
    # Comparison of error terms in both models ----------------------------------
    
    
    # Defining variables -------------------------------------------------------- rmANOVA function 'ow-rma' is required!  ANOVA function 'ow-a' is required!
    
    
    # ANOVA-tables of rmANOVA and ANOVA without repeated measures
    ow_a_results = ow_a(ow_rma_data, independent_var)[[1]]
    ow_rma_results = ow_rma(ow_rma_data, independent_var)[[1]]
    
    sse_anova = ow_a_results[3, 2]
    ss_subject_anova = 0
    # Always zero because the subject effect is not considered in an ANOVA without repeated measures
    
    sse_rma = ow_rma_results[4, 2]
    ss_subject_rma = ow_rma_results[3, 2]
    
    
    # Create variables for comparison plot displaying the reduction of error terms ------------------------------------------
    
    
    # 'var' contains the sse by subject and error
    var = c(sse_anova, ss_subject_anova, sse_rma, ss_subject_rma)
    
    # 'model' is used in the ggplot to assign the values to the bars
    model = rep(c("No estimation of the\nvariation between entities", "Estimation of the\nvariation between entities"), each = 2)
    
    # 'source' is required for color and legend label assignment in the ggplot
    source = factor(rep(c("Error", "Entity"), times = 2), levels = c("Entity", "Error"))
    
    # Merge variables into one data frame
    comparison_data = data.frame(var, model, source)
    
    
    # Create stacked barplot ---------------------------------------------------------
    
    # New variable: percentage of sse. used for better readability in piechart
    
    comparison_data$var_percent = comparison_data$var * 100/(max(comparison_data$var))
    
    comp_plot_bar = ggplot(comparison_data, aes(model, var_percent, fill = source)) + geom_bar(stat = "identity") + labs(x = "Model", y = "Sum of squares (error)", title = "Reduction of sum of squared errors (SSE)") + 
        guides(fill = guide_legend(title = NULL)) + scale_fill_manual(values = c("orange", "navyblue")) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())
    
    
    # Create pie chart -----------------------------------------------------------------
    
    
    
    
    comp_plot_pie = ggplot(comparison_data, aes(x = "", y = var_percent, fill = source)) + geom_bar(width = 1, stat = "identity") + labs(x = "", y = "", title = "Reduction of sum of squared errors (SSE) in percent") + 
        guides(fill = guide_legend(title = NULL)) + scale_fill_manual(values = c("orange", "navyblue")) + coord_polar(theta = "y") + facet_grid(~model) + theme_bw() + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
    
    
    # Selection of plot design --------------------------------------------------
    
    
    if (plot_type == "pie") {
        final_plot = comp_plot_pie
    } else {
        final_plot = comp_plot_bar
    }
    
    
    # Create comparison table --------------------------------------------------- Dependency: SSE in RM ANOVA is equal to SSE ANOVA minus SS entety
    
    
    
    error_ss_comparison_table = data.frame(check.names = FALSE, ` ` = c("Error", "Entity"), `Sum of squares` = c(sse_rma, ss_subject_rma), `Percentage share` = paste(as.character(round(comparison_data$var_percent[3:4])), 
        c("%", "%"), sep = ""))
    rownames(error_ss_comparison_table) = NULL
    
    
    # Return comparison table ---------------------------------------------------
    
    
    print(final_plot)
    if (ow_a_table == TRUE) {
        warning("\nNote that the one-way ANOVA without repeated measures is for\nillustration purposes only since the data structure is correlated\nacross the factor levels because of the dependent measurements.\nThe ANOVA without repeated measures treates the data as if they\nare independend i.e. as if there are different entities in each\ngroup, which is in fact not the case.")
        return(list(one_way_ANOVA_table = ow_a_results, error_sum_of_squares_reduction_table = error_ss_comparison_table))
    } else {
        return(list(error_sum_of_squares_reduction_table = error_ss_comparison_table))
    }
}


# ---------------------------------------------------------------------------


# Testing:
ow_rma_sse_reduct(ow_rma_data, independent_var = 1, plot_type = "bar", ow_a_table = TRUE)

