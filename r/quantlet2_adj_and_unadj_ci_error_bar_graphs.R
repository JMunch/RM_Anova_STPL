##### quantlet2_adj_and_unadj_ci_error_bar_graphs


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


### Peperation to show the following plots next to eachother

par(mfrow=c(1,2))


#### Plotting of 95% Confidence Intervalls

## Because we want a 95% CI the confidence level variable 'Clevel' is set to 0.95

Clevel = 0.95


### CI for ANOVA without repeated measures  

## 'SE' are the standard errors of the conditional means

SE = tapply(rma_data_long$value, rma_data_long$condition, sd) / sqrt(n)

CIdist = abs(qt((1 - Clevel)/2, (n - 1))) * SE


## Plot of condtional means with CI error bars


barends = 0.05

plot(MeFlm, ylim = c(min((MeFlm$Flm - CIdist)), max((MeFlm$Flm + CIdist))), main = "Unadjusted CI", xlab = "condition", ylab = "value")


for(i in 1:k) {
  up = MeFlm$Flm[i] + CIdist[i]
  low = MeFlm$Flm[i] - CIdist[i]
  segments(MeFlm$Me[i],low , MeFlm$Me[i], up)
  segments(MeFlm$Me[i]-barends, up , MeFlm$Me[i]+barends, up)
  segments(MeFlm$Me[i]-barends, low , MeFlm$Me[i]+barends, low)
}

# !!! Auch hier muesste der Plot noch verschoenert werden 

# !!! alternative... habs aber nicht hinbekommen fuer verschiedene CI pro mean

#segments(MeFlm$Me, MeFlm$Flm - CIdist, MeFlm$Me, MeFlm$Flm + CIdist)
#segments(MeFlm$Me - barends, MeFlm$Flm - CIdist, MeFlm$Me + barends, MeFlm$Flm - CIdist)
#segments(MeFlm$Me - barends, MeFlm$Flm + CIdist, MeFlm$Me + barends, MeFlm$Flm + CIdist)


### CI for ANOVA with repeated measures

## 'cf' is a correction factor etablished by Morey (2008)

cf = sqrt(k / (k - 1))

AdjVal = data.frame(Adj = (cf * ((rma_data_long$value - EEmlong$Em + Gm) - MeFlmlong$Flm)) + MeFlmlong$Flm)
rma_data_long_adj = cbind.data.frame(rma_data_long, AdjVal)


## 'SE_adj' are the standard errors of the conditional means adjusted with the method of O'Brien and Cousineau (2014, see also Loftus & Masson; 1994)

SE_adj = (tapply(rma_data_long_adj$Adj, rma_data_long_adj$condition, sd) / sqrt(n))

CIdist_adj = abs(qt((1 - Clevel)/2, (n - 1))) * SE_adj


## Plot of condtional means with adjusted CI error bars

barends = 0.05

plot(MeFlm, ylim = c(min((MeFlm$Flm - CIdist_adj)), max((MeFlm$Flm + CIdist_adj))), main = "Adjusted CI", xlab = "condition", ylab = "value")


for(i in 1:k) {
  up = MeFlm$Flm[i] + CIdist_adj[i]
  low = MeFlm$Flm[i] - CIdist_adj[i]
  segments(MeFlm$Me[i],low , MeFlm$Me[i], up)
  segments(MeFlm$Me[i]-barends, up , MeFlm$Me[i]+barends, up)
  segments(MeFlm$Me[i]-barends, low , MeFlm$Me[i]+barends, low)
}

# !!! Auch hier muesste der Plot noch verschoenert werden 

# !!! alternative... habs aber nicht hinbekommen fuer verschiedene CI pro mean

#segments(MeFlm$Me, MeFlm$Flm - CIdist_adj, MeFlm$Me, MeFlm$Flm + CIdist_adj)
#segments(MeFlm$Me - barends, MeFlm$Flm - CIdist_adj, MeFlm$Me + barends, MeFlm$Flm - CIdist_adj)
#segments(MeFlm$Me - barends, MeFlm$Flm + CIdist_adj, MeFlm$Me + barends, MeFlm$Flm + CIdist_adj)


### litle extra plot
## horizontal line are the condition means / 'X' are the subject means 
#par(mfrow=c(1,1))
#matplot(rma_data[2:(k + 1)], type = c("b"), pch = ".", col = 1:k)
#points(Em, cex = 2, pch = 4)
#abline(h = Flm, col = 1:k)

