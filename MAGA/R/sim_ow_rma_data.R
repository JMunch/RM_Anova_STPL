#' Simulate data for one-way repeated measures ANOVA
#'
#' Simulate data that can be used to estimate repeated measures ANOVA models. Function parameters allow to simulate different data properties such as sphericity and polynomial trends.
#'
#' @param n Integer specifying the number of subjects.
#' @param k Integer specifying the number of factor levels i.e. repeated measurements.
#' @param means Numeric vector specifying the means for the k factor levels. If length of means vector exeeds k, the mean vector is truncated to match the number of factor levels. Default is NULL.
#' @param poly_order = Integer specifying the highest order polynomial trend that the simulated data is supposed to have. Ignored if means argument is specified. Default is NULL.
#' @param noise_sd Numeric vector specifying the standard deviation of the noise that is added to each factor level values. If it is desired to simulate sphericity, noise_sd must be of length k, otherwise of length 1. Default is 10.
#' @param between_subject_sd Numeric vector specifying the standard deviation of the values withing each of the k factor levels. Default is 40.
#' @param NAs Integer specifying the number of missing values that is randomly assigned to the data. Only used to ensure that the functions are robust against missing values. Default is 0.
#'
#' @return Returns an object of type data.frame
#' \item{rma_data}{An object of type data.frame containing the simulated data}
#' @author Joachim Munch, Frederik Schreck, Quang Nguyen Duc, Constantin Meyer-Grant, Nikolas Hoeft
#' @note This function allows to generate custom rma data that is suitable for all MAGA functions.
#' @examples
#'
#'sim_rma_data(20, 4, means = NULL, poly_order = NULL, noise_sd = 10, between_subject_sd = 40, NAs = 0)
#'
#' @rdname sim_rma_data
#' @export


# Function to simulate data for repeated measurement ANOVA


sim_rma_data = function(n, k, means = NULL, poly_order = NULL, noise_sd = 10, between_subject_sd = 40, NAs = 0) {

  # Create data structure and simulate data --------------------------------------------------

  # Create empty n x k matrix
  rma_data = matrix(NA, nrow = n, ncol = k + 1)

  # Add column with subject_id
  rma_data[, 1] = 1:n

  if (!is.null(means)) {
    con_means = means

    # Check if length of mean vector corresponds to k
    if (length(means) != k) {
      k = length(means)
      print("Number of factors (k) was changed, because the length of means vector and argument k do not correspond.")
    }
  } else {

    # Simulate conditional means
    if (is.null(poly_order)) {
      con_means = runif(k, min = 100, max = 300)
    } else {

      # Generate polinomial conditional means
      factors   = runif((poly_order + 1), min = 0, max = 1)
      x         = order(runif(k, min = 100, max = 300), decreasing = FALSE)
      con_means = matrix(factors[1], nrow = k)

      for (p in (2:(poly_order + 1))) {
        con_means = con_means + factors[p] * x^p
      }
    }
  }

  # Add con_mean to the rma_data matrix
  rma_data[, 2:(k + 1)] = matrix(rep(con_means, each = n), nrow = n)

  # Simulate subject means Calculate the deviation from the conditional mean for each subject
  mean_deviation        = rnorm(n, mean = 0, sd = between_subject_sd)
  rma_data[, 2:(k + 1)] = rma_data[, 2:(k + 1)] + mean_deviation

  # Check if only one noise_sd was passed (no sphericity) and create vector
  if (length(noise_sd) == 1) {
    noise_sd = rep(noise_sd, times = k)
  }

  # Error if noise_sd vector not a vector of length k
  if (length(noise_sd) != k) {
    print("The vector passed for noise_sd does not have the length k. Please pass a vector of the length k.")
    return(NULL)
  }


  # Add noise to data ----------------------------------------------------------

  noise = matrix(NA, nrow = n, ncol = k)

  for (i in 1:k) {
    noise[, i] = rnorm(n, mean = 0, sd = noise_sd[i])
  }

  rma_data[, 2:(k + 1)] = rma_data[, 2:(k + 1)] + noise

  # Simulating NAs, adds the number of NA to the the data passed as argument
  if (NAs > 0) {
    for (i in 1:NAs) {
      rma_data[runif(1, min = 1, max = n), runif(1, min = 2, max = (k + 1))] = NA
    }
  }


  # Naming columns ------------------------------------------------------------

  factor_names    = character(k + 1)
  factor_names[1] = "Subject_id"

  for (i in 1:k) {
    factor_names[i + 1] = paste("Factor", i)
  }
  colnames(rma_data) = factor_names

  return(data.frame(rma_data))
}
