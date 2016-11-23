##### quantlet3_orth_poly_contrasts


#### Preparations

### Clear workspace

#rm(list = ls())   


### Install required packages 



### Define some variables

## 'n' is the number of entities 

n = as.numeric(length(rma_data[,1]))


## Specify the names of the 'id'-variable and of the 'condition'-variables

rm_names = colnames(rma_data)[-1]
id_names = colnames(rma_data)[1]


## 'k' is the number of factor levels

k = as.numeric(length(rm_names))


## Convert to long format

rma_data_long = reshape(rma_data, 
                        varying = rm_names, 
                        v.names = "value",
                        timevar = "condition", 
                        times = (1:k),
                        idvar = id_names,
                        new.row.names = 1:(k * n),
                        direction = "long")
colnames(rma_data_long)[1] = "id"
rma_data_long$condition = as.numeric(rma_data_long$condition)


## 'Flm' are the factor level means

Flm = tapply(rma_data_long$value, rma_data_long$condition, mean)


## 'Gm' is the general mean

Gm = mean(rma_data_long$value)


## 'Em' is the entity/subject mean

Em = tapply(rma_data_long$value, rma_data_long$id, mean)


## 'Me' Measurements

Me = (1:k)


## Mean of each measurement condition ('MeFlm' dataframe)

MeFlm = data.frame(Me, Flm)
MeFlmlong = MeFlm[rep(seq_len(nrow(MeFlm)), each = n),]


## 'E' Entities

E = (1:n)


## Mean of each entity/subject ('EEm' dataframe)

EEm = data.frame(E, Em)
EEmlong = EEm[rep(seq_len(nrow(EEm)), k), ]


### Orthogonal polynomial Contrasts

## 'maxpoly' is the maximal polynomial degree for orthogonal polynomials

maxpoly = k - 1


## Defining Contrast weights for orthogonal polynomial contrasts

contrast_weights = t(contr.poly(k))


## Applining formula for linear contrasts

weighted_dependend_variables = rma_data[rep(seq_len(nrow(rma_data)), each = maxpoly), ][,-1] * (contrast_weights)[rep(seq_len(nrow(contrast_weights)), n), ]
linear_subject_contrasts = matrix(rowSums(weighted_dependend_variables), byrow = TRUE, ncol = maxpoly)


## Computing contrast estimators for each orthogonal polynomial contrast as well as standard errors for thees estimators

contrast_estimator = colMeans(linear_subject_contrasts)
contrast_se = sqrt(apply(linear_subject_contrasts,2,var)) / sqrt(n)


## Computing t-values for each contrast

contrast_t_values = contrast_estimator / contrast_se

#contrast_F_values = contrast_t_values^2


## Computing the corresponding p-values

contrast_p_values = 1 - pt(abs(contrast_t_values), n-1)


## Computing sums of squares for each contrast
contrast_ss = n * contrast_estimator^2 / rowSums(contrast_weights^2)


## Computing amount of the variance in the dependent variable explained by the factor which in turn can be explained by a cerain orthogonal polynomial trend 
## ss_trend / ss_factor

proportional_trend_contribution = contrast_ss/rep(sum(rep((Flm - Gm)^2, each = n)), maxpoly)


### Putting it all together...

## specify source variable

source = rownames(contrast_weights)


## Contrast table

contrast_table = data.frame("Source" = source,
                            "Sum of squares" = contrast_ss,
                            "Proportional contribution to the factor effect" = proportional_trend_contribution,
                            "Contrast estimator" = contrast_estimator,
                            "Standard error" = contrast_se,
                            "Degrees of freedom" = rep((n - 1), maxpoly),
                            "t-value" = contrast_t_values,
                            "p-value" = contrast_p_values
)

rownames(contrast_table) = NULL


### Orthogonal polynomial trends as polynomial regression (used to plot the fitted polynomials)
### This is used to display the aditional explanation of the variance in the dependent variable by adding higher order trendcomponent successively 

### Peperation to show the following plots (4) next to eachother

par(mfrow=c(1,2))


## Fitting the k - 1 orthogonal Polynomials

for(i in 1:maxpoly){
  pfv = paste("poly.fit.", i, sep = "")
  assign(pfv, lm(rma_data_long$value ~ poly(rma_data_long$condition, degree = i, raw = TRUE)))
  poly.fit.max = lm(rma_data_long$value ~ poly(rma_data_long$condition, degree = i, raw = TRUE))
}


## Plotting the predictions by the k - 1  orthogonal polynomial trends together with the raw data                                      

plot(rma_data_long$condition, rma_data_long$value, main = "raw data", xlab = "condition", ylab = "value")                  

for(i in 1:maxpoly){
  lines(smooth.spline(rma_data_long$condition, predict(lm(rma_data_long$value ~ poly(rma_data_long$condition, degree = i, raw = FALSE)))), col = i, lwd = 2)
}

# !!! plot koennte noch verschoenert werden (Tietel, Beschriftung, Farben, etc.)

# !!! das sollten eigentlich smooth lines sein... und nicht die Predictions fuer die einzelnen Levels verbunden mit linien...


## Plotting the predictions by the k - 1  orthogonal polynomial trends together with the conditional means 

plot(MeFlm, main = "conditional means", xlab = "condition", ylab = "value")

for(i in 1:maxpoly){
  lines(smooth.spline(rma_data_long$condition, predict(lm(rma_data_long$value ~ poly(rma_data_long$condition, degree = i, raw = FALSE)))), col = i, lwd = 2)
}

# !!! plot koennte noch verschoenert werden (Tietel, Beschriftung, Farben, etc.)

# !!! das sollten eigentlich smooth lines sein... und nicht die Predictions fuer die einzelnen Levels verbunden mit linien... 
# !!! also ein funktionsgraph wäre gut

