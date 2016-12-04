

### Quantlet 5: Computation of standard ANOVA and comparison with RM-ANOVA ###

#####################################
# PART 1: Computaion of ANOVA model #
#####################################

#---------------------------------------------------------------------------
anova = function(rma_data){

  # Libraries needed for compuiting rma
  require(dplyr)
  
  
  # Define needed constants and the dependent variable ----------------------
  
  
  n = nrow(rma_data)
  k = ncol(rma_data) - 1
  dependent_variable = as.matrix(rma_data[, -1])
  
  
  # Define basic anova components -------------------------------------------
  
  
  grand_mean = mean(as.matrix(rma_data[,2: (k + 1)])) 
  baseline_components = matrix(rep(grand_mean, times = k*n), nrow = n)
  
  conditional_means = apply(dependent_variable, 2, mean)
  factor_level_components = matrix(rep(conditional_means - grand_mean, each = n), nrow = n)


  # Computation of the error component 
  
  error_components_ANOVA = dependent_variable - baseline_components - factor_level_components
  
  
  # Prepare decomposition matrix --------------------------------------------
  # matrix with k*n rows and 3 columns
  # one column for: original values, factor level component, error component
  decomposition_matrix_ANOVA = data.frame("dependent_variable" = numeric(n*k),
                                          "factor_level" = numeric(n*k),
                                          "error" = numeric(n*k)
                                          )
  
  
  decomposition_matrix_ANOVA$dependent_variable = as.vector(dependent_variable)
  decomposition_matrix_ANOVA$factor_level = as.vector(factor_level_components)
  decomposition_matrix_ANOVA$error = as.vector(error_components_ANOVA)
  
  
  # Compute sums of squares -------------------------------------------------
  
  ss_ANOVA = as.data.frame(t(colSums(decomposition_matrix_ANOVA^2)))
  rownames(ss_ANOVA) = "sums_of_squares"
  
  
  # Set degrees of freedom --------------------------------------------------
  #-->different dof's for the SSE than in RM ANOVA
  
  dof_ANOVA = data.frame("dependent_variable" = (n*k),
                         "factor_level" = k-1,
                         "error" = (n-k)
                          )
  
  
  # Compute mean squares ----------------------------------------------------
  
  ms_ANOVA = ss_ANOVA / dof_ANOVA
  rownames(ms_ANOVA) = "mean_squares"
  
  
  # Calculate the F-Value ---------------------------------------------------
  
  F_value_factor_ANOVA = ms_ANOVA$factor_level / ms_ANOVA$error
  
  
  # Set p-value of F distribution -------------------------------------------
  
  p_factor_ANOVA = 1 - pf(F_value_factor_ANOVA, dof_ANOVA$factor_level, dof_ANOVA$error)
  
  
  # Create the output table -------------------------------------------------
  #-->Corrected total sum of squares can be taken from the RM ANOVA-Model as they coincide 
  
  # specify source variable
  source_ANOVA = c("Factor", "Error", "Corrected total")
  
  # create table
  ANOVA_table_2 = data.frame("Source" = source_ANOVA,
                             "Sum of squares" = c(ss_ANOVA %>% select(2:3) %>% unlist(), NA),#, corrected_sst), # corrected sst muss berechnet werden TODO
                             "Degrees of freedom" = c(dof_ANOVA %>% select(2:3) %>% unlist(), (n*k)-1),
                             "Mean squares" = c(ms_ANOVA %>% select(2:3) %>% unlist(), NA),
                             "F-value" = c(F_value_factor_ANOVA, NA,  NA),
                             "p-value" = c(p_factor_ANOVA, NA, NA)
                             )
  
  rownames(ANOVA_table_2) = NULL
  
  return(ANOVA_table_2)
}

# ----------------------------------------------------------------
# Testing:

#source("r/simulate_rma_data.R")
#rma_data = sim_rma_data(10, 5)
anova_results = anova(rma_data)

anova_results

####################################################
# PART 2: Comparison of error terms in both models #
####################################################

compare_anovas = function(rma_results, anova_results){
  
  require(ggplot2)
  require(reshape)
  
  # Defining variables
  sse_anova = anova_results$Sum.of.squares[2]
  ss_subject_anova = 0 # Always zero
  
  sse_rma = rma_results$Sum.of.squares[4]
  ss_subject_rma = rma_results$Sum.of.squares[3]
  
  
  # Is this needed check works now, so it seems to be correct?????????????????????????????????????????????????!!
  
  # Dependency: SSE in RM ANOVA is equal to SSE ANOVA minus SS Subjects-------
  sse_rma - (sse_anova - ss_subject_rma) # They are nearly equal difference should due to floating point
    
  #--> The SSE in the RM ANOVA model is also computed with larger degrees of freedom than the SSE in the standard ANOVA model. 
  
  #Defining a table containing the different errortypes
  ss_table = matrix(c(sse_anova, sse_rma, 
                      ss_subject_anova, ss_subject_rma),ncol=2,byrow=TRUE)
  colnames(ss_table) = c("ANOVA", "RM_ANOVA")
  rownames(ss_table) = c("SSE", "SS_Subjects")
  
  
  # Visualisation-------------------------------------------------------------
  #1) Stackplot-Version
  barplot1 = barplot(ss_table, 
              col = c("navyblue", "orange"), 
              xlab = "Model", 
              ylab = "Sum of Squares Error", 
              legend.text = TRUE,
              main = "Comparison of error terms between standard ANOVA and Repeated Measures ANOVA")
  
  
  #2)Pie Charts-Version 1
  
  # TODO both pies in one figure
  slices = c(sse_anova, ss_subject_anova) 
  lbls = c("SSE", "SS_Subject_level")
  pct = round(slices/sum(slices)*100)
  lbls = paste(lbls, pct) # add percents to labels 
  lbls = paste(lbls,"%",sep="") # ad % to labels 
  pie1_ANOVA = pie(slices, lbls, col = c("steelblue4", "red"),
                   main="Standard ANOVA", init.angle = 90) 
  pie1_ANOVA
  
  slices_2 = c(sse_rma, ss_subject_rma) 
  lbls_2 = c("SSE", "SS_Subject_level")
  pct_2 = round(slices_2/sum(slices_2)*100)
  lbls_2 = paste(lbls_2, pct_2) # add percents to labels 
  lbls_2 = paste(lbls_2,"%",sep="") # ad % to labels 
  pie1_RM_ANOVA = pie(slices_2, lbls_2, col = c("steelblue4", "red"),
                   main="RM ANOVA", init.angle = 90) 
  pie1_RM_ANOVA
  
  
  
  #Stackplot Version 2 (with ggplot)
  
  #Defining a dataframe for model comparison
  ss_table_gg = melt(ss_table)
  
  colnames(ss_table_gg) = c("Error_type", "ANOVA_type", "value")
  ss_table_gg$value = as.numeric(ss_table_gg$value)
  ss_table_gg$ANOVA_type = as.factor(ss_table_gg$ANOVA_type)
  ss_table_gg$Error_type = as.factor(ss_table_gg$Error_type)
  
  barplot2 = ggplot(ss_table_gg, aes(x = ANOVA_type, y = (value/(sum(value)/2)*100), fill = Error_type)) + 
                  geom_bar(stat = "identity", width = 0.8) +
                  ggtitle("Comparison of error terms between standard ANOVA and Repeated Measures ANOVA") + 
                  xlab("Model") +
                  ylab("Percentages") +
                  geom_text(aes(label = (value/(sum(value)/2))*100), position = "identity") 
  barplot2
  
  
  
  #Pie Chart Version 2 (with ggplot)
  # What is the object bar_chart?????
  
#  pie2 = bar_chart + 
 #             coord_polar(theta = "y", direction = -1)  + 
 #             facet_grid(.~model_comparison$ANOVA_type) +
  #            theme_void() +
   #           ggtitle("Comparison of error terms between standard ANOVA and Repeated Measures ANOVA")
#  pie2

}

# ----------------------------------------------------------------
# Testing:

#source("r/simulate_rma_data.R")
#rma_data = sim_rma_data(10, 5)
rma_results = rma(rma_data)
anova_results = anova(rma_data)

compare_anovas(rma_results, anova_results)
