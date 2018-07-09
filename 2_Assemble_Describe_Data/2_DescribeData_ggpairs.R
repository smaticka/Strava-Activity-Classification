# read in training data and look at correlations between covariates

# location of training and test data sets
setwd('/Users/smaticka/Box Sync/stanford/Classes/MS&E 226 small data/mini project/I_Data_Files')

# Install package on initial use
# install.packages("ggplot2")
# install.packages("GGally")

library(ggplot2)
library(GGally)

# Table of covariates and response variable
df <- readRDS("TrainingData.rds")

ID <- as.data.frame(cbind(df$Athlete, df$FileID))

#reassign df without IDs
df = df[,colnames(df)!="Athlete"]
df = df[,colnames(df)!="FileID"]
df = df[,colnames(df)!="Ibike"]

# convert Iswim integer to numeric to apply mean to data frame as matrix
df$Iswim = as.numeric(df$Iswim)

# calculate mean manually (need to change class)
ncov = dim(df)[2]

# calculate mean and standard deviation of columns
M  <- apply(df[,-ncov], 2, mean)
SD <- apply(df[,-ncov], 2, sd)

print(formatC(M,digits=5,format="fg",flag="#"))
print(formatC(SD,digits=5,format="fg",flag="#"))

# separate activities
n = nrow(df)
mean.swim = nrow(df[df$Activity == "swim",])/n
mean.walk = nrow(df[df$Activity == "walk",])/n
mean.run = nrow(df[df$Activity == "run",])/n
mean.ride = nrow(df[df$Activity == "ride",])/n

#calculate cor. coefficient for each covariate except Dtotal (the response variable for regression model)
df.cov = df[,colnames(df)!="Dtotal"]

Ncov = length(df.cov)

coef = rep(NA,Ncov)
for (i in 1:Ncov-1) {
  coef[i] = cor(df.cov[,i], df$Dtotal)
}

# associate variable name to the coefficient
covar = colnames(df.cov)

plot(factor(covar), coef, las = 2)

# mutual correlations
ggpairs(df.cov, columns = 1:Ncov, las =0)





