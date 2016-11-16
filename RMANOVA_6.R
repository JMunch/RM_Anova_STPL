#### Repeated Measures ANOVA with orthogonal polynomial contrasts


### Preparations

## Clear workspace

rm(list = ls())   

## Install required packages 

#install.packages("plyr")
#install.packages("reshape")
library(plyr)
library("reshape")

## Check resp. set working directory

getwd()
setwd("D:\\Uni\\Statistik\\1. Semester\\Statistical_Programming\\Project)


### Read data

## Required format: Tab sep., comma as decimal mark, first row are the col. names, wide format, ordered equidistant Factor levels

RT_wide = read.table("Daten_R_RT.txt", header = TRUE, dec = ",", sep = "\t")
#ER_wide = read.table("Daten_R_ER.txt", header = TRUE, dec = ",", sep = "\t")

str(RT_wide)
#str(ER_wide)


### Define some variables

## 'n' is the number of enteties 

n = length(RT_wide[,1])
n = as.numeric(n)


## Convert to long format

## 'id' are the enteties, which are measured repeadetly
## 'measured' are the different measurement conditions

RT_long = melt(RT_wide,
               id = "ID",
               measured = c("Messung_1", "Messung_2", "Messung_3", "Messung_4", "Messung_5",	
                            "Messung_6", "Messung_7", "Messung_8", "Messung_9"))
#ER_long = melt(ER_wide,
#               id = "ID",
#               measured = c("Messung_1", "Messung_2", "Messung_3", "Messung_4", "Messung_5",	
#                            "Messung_6", "Messung_7", "Messung_8", "Messung_9"))


## 'RT' is here the name of the dependend variable

colnames(RT_long) = c("ID", "Messung", "RT")
#colnames(ER_long) = c("ID", "Messung", "ER")


## 'k' is the number of factor levels

k = nlevels(RT_long$Messung)
k = as.numeric(k)


## 'Flm' are the factor level means

Flm = tapply(RT_long$RT, RT_long$Messung, mean)


## 'Gm' is the General Mean

Gm = mean(RT_long$RT)


## 'Em' is the Entety mean

Em = tapply(RT_long$RT, RT_long$ID, mean)


## 'Me' Measurements

Me = (1:k)


## Make Levels numeric (in case they aint already)

levels(RT_long$Messung) = (1:9)
RT_long$Messung = as.numeric(levels(RT_long$Messung))[RT_long$Messung]

str(RT_long)


## Means of each measurement condition ('MeFlm' dataframe)

MeFlm = data.frame(Me, Flm)
MeFlmlong = MeFlm[rep(seq_len(nrow(MeFlm)), each = n),]

## 'E' Entedties

E = (1:n)


## Means of each measurement condition ('EEm')

EEm = data.frame(E, Em)
EEmlong = EEm[rep(seq_len(nrow(EEm)), k), ]


### Repeated Measures ANOVA




### Sphericity (Mauchly-Test, Correction of degrees of Freedom)




### Plotting of 95% Confidence Intervalls


## Computing 95% CI

## Because we want a 95% CI the confidence level variable 'Clevel' is set to 0.95

Clevel = 0.05

cf = sqrt(k / (k - 1))

AdjVal = data.frame(Adj = (cf * ((RT_long$RT - EEmlong$Em + Gm) - MeFlmlong$Flm)) + MeFlmlong$Flm)

RT_long_adj = cbind.data.frame(RT_long, AdjVal)

#SD_adj = tapply(RT_long_adj$Adj, RT_long_adj$Messung, sd)
SE_adj = (tapply(RT_long_adj$Adj, RT_long_adj$Messung, sd) / sqrt(n))

CIdist = abs(qt(Clevel/2, (n - 1))) * SE_adj


## Plot of condtional means with CI error bars

barends = 0.05

plot(MeFlm, ylim = c(min((MeFlm$Flm - CIdist)), max((MeFlm$Flm + CIdist))))


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


### Orthogonal polynomial Contrasts

## Defining Contrast weights





### Orthogonal polynomial Contrasts as polynomial regression

## 'maxpoly' is the maximal polynomial degree for orthogonal polynomials

maxpoly = k - 1


## Fitting the k - 1 orthogonal Polynomials

for(i in 1:maxpoly){
      pfv = paste("poly.fit.", i, sep = "")
      assign(pfv, lm(RT_long$RT ~ poly(RT_long$Messung, degree = i, raw = FALSE)))
      poly.fit.max = lm(RT_long$RT ~ poly(RT_long$Messung, degree = i, raw = FALSE))
}


## Summary
summary(poly.fit.max)
# !!! irgendwie sollten moeglichst summarys und plots fuer alle orthogonalen polynomialen trends ausgegeben werden...


## Plotting the Predictions                                       
# !!! das sollten eigentlich smooth lines sein... und nicht die Predictions fuer die einzelnen Levels verbunden mit linien...  

plot(RT_long$Messung, RT_long$RT, main = "Plot")                  
# !!! plot koennte noch verschoenert werden (Tietel, Beschriftung, Farben, etc.)
for(i in 1:maxpoly){
      lines(smooth.spline(RT_long$Messung, predict(lm(RT_long$RT ~ poly(RT_long$Messung, degree = i, raw = FALSE)))), col = i, lwd = 2)
}


## Fitting the k - 1 orthogonal Polynomials to the means
# !!! wo liegen die unterschiede zwischen dem fitting zu den kompletten daten und zu den means?

for(i in 1:maxpoly){
  pfv = paste("poly.fit.", i, sep = "")
  assign(pfv, lm(MeFlm$Flm ~ poly(MeFlm$Me, degree = i, raw = FALSE)))
  poly.fit.max = lm(MeFlm$Flm ~ poly(MeFlm$Me, degree = i, raw = FALSE))
}


## Summary

summary(poly.fit.max)


## Plotting the Predictions

plot(MeFlm)
for(i in 1:maxpoly){
  lines(smooth.spline(MeFlm$Me, predict(lm(MeFlm$Flm ~ poly(MeFlm$Me, degree = i, raw = FALSE)))), col = i, lwd = 2)
}




