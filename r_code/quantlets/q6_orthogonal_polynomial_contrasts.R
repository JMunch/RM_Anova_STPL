##### Orthogonal polynomial contrasts in a one-way repeated measures ANOVA


rma_opc = function(rma_data, id = 1, maxpoly = NA, print_plot = TRUE) {
    
    # suppress warning messages from the required packages NOTE: This function still loads the packages!
    suppressWarnings(suppressMessages(require(dplyr)))
    suppressWarnings(suppressMessages(require(ggplot2)))
    suppressWarnings(suppressMessages(require(tidyverse)))
    
    
    # Check if the data meet the following requirement:
    
    # id must either be an integer specifying the column position of the independent variable
    if (id %in% 1:ncol(rma_data) == FALSE || length(id) != 1) {
        stop("id must be an integer specifying the column position of the independent variable")
    }
    
    dependent_variable = as.matrix(rma_data[, -id])
    
    
    
    # Define some variables ---------------------------------------------------
    
    
    # number of entities
    n = nrow(rma_data)
    
    # number of factor levels
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
    
    
    # Convert to long format --------------------------------------------------
    
    
    rma_data_long = reshape(rma_data, varying = rm_names, v.names = "value", timevar = "condition", times = (1:k), idvar = id_names, new.row.names = 1:(k * n), 
        direction = "long")
    colnames(rma_data_long)[1] = "id"
    rma_data_long$condition = as.numeric(rma_data_long$condition)
    
    
    # Define some more variables ----------------------------------------------
    
    
    # factor level means
    Flm = tapply(rma_data_long$value, rma_data_long$condition, mean)
    
    # general mean
    Gm = mean(rma_data_long$value)
    
    # entity/subject mean
    Em = tapply(rma_data_long$value, rma_data_long$id, mean)
    
    # Measurements
    Me = 1:k
    
    # Mean of each measurement condition ('MeFlm' dataframe)
    MeFlm = data.frame(Me, Flm)
    MeFlmlong = MeFlm[rep(seq_len(nrow(MeFlm)), each = n), ]
    
    # Entities
    E = 1:n
    
    # Mean of each entity/subject ('EEm' dataframe)
    EEm = data.frame(E, Em)
    EEmlong = EEm[rep(seq_len(nrow(EEm)), each = k), ]
    
    
    # Orthogonal polynomial Contrasts -----------------------------------------
    
    
    # maximal polynomial degree for orthogonal polynomials
    if((maxpoly > k - 1) | (is.na(maxpoly))){
    maxpoly = k - 1}
    
    # Defining Contrast weights for orthogonal polynomial contrasts
    contrast_weights = t(contr.poly(k))
    
    # Applying formula for linear contrasts
    weighted_dependend_variables = dependent_variable[rep(1:n, each = maxpoly), ] * (contrast_weights)[rep(1:maxpoly, n), ]
    linear_subject_contrasts = matrix(rowSums(weighted_dependend_variables), byrow = TRUE, ncol = maxpoly)
    
    # Computing contrast estimators for each orthogonal polynomial contrast as well as standard errors for thees estimators
    contrast_estimator = colMeans(linear_subject_contrasts)
    contrast_se = sqrt(apply(linear_subject_contrasts, 2, var))/sqrt(n)
    
    # Computing t-values for each contrast
    contrast_t_values = contrast_estimator/contrast_se
    # contrast_F_values = contrast_t_values^2
    
    # Computing the corresponding p-values
    contrast_p_values = 1 - pt(abs(contrast_t_values), n - 1)
    
    # Computing sums of squares for each contrast
    contrast_ss = n * contrast_estimator^2/rowSums(contrast_weights^2)
    
    # Computing amount of the variance in the dependent variable explained by the factor which in turn can be explained by a cerain orthogonal polynomial trend ss_trend
    # / ss_factor
    proportional_trend_contribution = contrast_ss/rep(sum(rep((Flm - Gm)^2, each = n)), maxpoly)
    
    
    
    # Create contrast table ---------------------------------------------------
    
    
    # define source variable
    source = rownames(contrast_weights)
    contrast_table = data.frame(check.names = FALSE, Source = source, `Sum of squares` = contrast_ss, `Proportional contribution to the factor effect` = proportional_trend_contribution, 
        `Contrast estimator` = contrast_estimator, `Standard error` = contrast_se, `Degrees of freedom` = rep((n - 1), maxpoly), `t-value` = contrast_t_values, `p-value` = contrast_p_values)
    rownames(contrast_table) = NULL
    
    
    # Orthogonal polynomial trends as polynomial regression ------------------ Used to plot the fitted polynomials This is used to display the aditional explanation of
    # the variance in the dependent variable by adding higher order trendcomponent successively
    
    
    # initialize empty dataframe for polynomial regression coefficients
    poly_coef = data.frame(matrix(0, ncol = k - 1, nrow = k))
    
    # Fitting the k - 1 orthogonal Polynomials In each cycle of the loop the coefficients are assigned to the i-th column of the object poly_coef
    for (i in 1:maxpoly) {
        pfv = paste("poly_fit_", i, sep = "")
        poly = assign(pfv, lm(rma_data_long$value ~ poly(rma_data_long$condition, degree = i, raw = TRUE)))
        poly_coef[, i][1:(i + 1)] = poly$coef
    }
    
    # Plotting contrats (ggplot) -------------------------------------------------
    
    
    # create datapoints for polynomial plot: this code automatically sets up the data that is required to plot the k-1 polynomial regression lines
    
    poly_curve_data = data.frame(x = seq(1, k, length.out = 100), tcrossprod(outer(seq(1, k, length.out = 100), 0:(k - 1), `^`), do.call(rbind, poly_coef))) %>% gather(var, 
        y, -x)
    
    # plot the k-1 polynomial regression lines
    poly_plot = ggplot(data = rma_data_long, aes(x = condition, y = value)) + geom_point() + labs(col = "Order of \npolynomial", x = "Condition", y = "Value", title = "Orthogonal polynomial contrasts") + 
        geom_path(data = poly_curve_data, aes(x, y, color = var), lwd = 1.2) + scale_color_discrete(labels = as.character(1:(k - 1))) +
        theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.key = element_rect(colour = "black"), plot.title = element_text(face="bold", hjust = .5))
    
    
    # Return the contrast-table and plot ----------------------------------------
    
    if (print_plot == TRUE){
    print(poly_plot)}
    
    return(list(contrast_table = contrast_table, poly_plot = poly_plot))
}


# ---------------------------------------------------------------------------


# Testing:
rma_opc(rma_data, id = 1)

