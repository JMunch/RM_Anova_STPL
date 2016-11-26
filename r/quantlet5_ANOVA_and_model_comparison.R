
### Quantlet 5: Computation of standard ANOVA and comparison with RM-ANOVA ###

#####################################
# PART 1: Computaion of ANOVA model #
#####################################

#---------------------------------------------------------------------------
library(ggplot2)

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
                           "Sum of squares" = c(ss_ANOVA %>% select(2:3) %>% unlist(), corrected_sst),
                           "Degrees of freedom" = c(dof_ANOVA %>% select(2:3) %>% unlist(), (n*k)-1),
                           "Mean squares" = c(ms %>% select(2:3) %>% unlist(), NA),
                           "F-value" = c(F_value_factor_ANOVA, NA,  NA),
                           "p-value" = c(p_factor, NA, NA)
                           )

rownames(ANOVA_table_2) = NULL


####################################################
# PART 2: Comparison of error terms in both models #
####################################################

# Dependency: SSE in RM ANOVA is equal to SSE ANOVA minus SS Subjects-------
ss$error == ss_ANOVA[,3] - ss$subject_level
#--> The SSE in the RM ANOVA model is also computed with larger degrees of freedom than the SSE in the standard ANOVA model. 


# Visualisation-------------------------------------------------------------
#1) Stackplot

table = matrix(c(ss_ANOVA[,3], ss$error, 0, ss$subject_level),ncol=2,byrow=TRUE)

colnames(table) = c("ANOVA", "RM_ANOVA")
rownames(table) = c("SSE", "SS_Subjects")
#table = as.data.frame(table)
table

barplot(table)

#2)Pie Charts

slices = c(ss_ANOVA[,3], 0) 
lbls = c("SSE", "SS_Subject_level")
pct = round(slices/sum(slices)*100)
lbls = paste(lbls, pct) # add percents to labels 
lbls = paste(lbls,"%",sep="") # ad % to labels 
pie(slices, lbls, col=rainbow(length(lbls)),
    main="Standard ANOVA") 

slices_2 = c(ss$error, ss$subject_level) 
lbls_2 = c("SSE", "SS_Subject_level")
pct_2 = round(slices_2/sum(slices_2)*100)
lbls_2 = paste(lbls_2, pct_2) # add percents to labels 
lbls_2 = paste(lbls_2,"%",sep="") # ad % to labels 
pie(slices_2, lbls_2, col=rainbow(length(lbls_2)),
    main="RM ANOVA") 

## ggplot beispiel
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(vs))



