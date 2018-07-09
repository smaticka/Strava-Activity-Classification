# this script creates different model templates

setwd('/Users/smaticka/Box Sync/stanford/Classes/MS&E 226 small data/mini project/I_Data_Files')

library(gdata) # to read in data
library(arm)
library(ggplot2)
library(GGally)
library(cvTools)
library(nnet)
library(glmnet)

source("../2_Assemble_Describe_Data/3_RemoveCovar_ggpairs.R")

# set a reference level (chose slowest activity, swim)
df$Activity = relevel(df$Activity, ref = "swim")

############### Linear Regression models

# Predict mean as the most basic predictive model (1A)
fm.base1   = mean(df$Tstart) # mean of start time (linear regression response)
MSE.base1  = sum( (df$Tstart - fm.base1)^2 )/length(df$Tstart) # MSE between basic guess and observed
RMSE.base1 = sqrt( MSE.base1 ) # RMSE of basic model

# Create another basic predictive model using only a few covariates (1C)
# Linear regression
fm.base2    <- lm(df, formula = Tstart ~ 1 + dEmax + Dtotal + Ttotal + Turnsmed)
fm.base2.cv =  cvFit(fm.base2, data = df, y = df$Tstart, K = 10)
MSE.base2   =  fm.base2.cv$cv^2

############### Logistic regression models
# perform cv via a penalized maximum likelihood iterative scheme
# and regularization. 'class' returns classification error.
# ridge = 0 < alpha < 1 = lasso

mat           = data.matrix(data.frame(df$Tstart, df$Ttotal, 
                                       df$Turnsmed, df$Umed),rownames.force = NA)
fm.base2.cat  = cv.glmnet(mat, df$Activity, family = "multinomial",
                          nfolds = 10, alpha =0, type.measure = "class")

# use the lambda that yields the minimum mean cv error
fit.base2.cat = predict.cv.glmnet(object = fm.base2.cat, newx = mat,
                                  s = "lambda.min", type = "class")

# convert to factor to compare to observations
fit.base2.cat = factor(fit.base2.cat)

# mean cv error (misclassification error)
cvErr = fm.base2.cat$cvm[fm.base2.cat$lambda == fm.base2.cat$lambda.min]
levels(fit.base2.cat) = c(levels(fit.base2.cat),"walk")

# compart to observation
df.check = data.frame(Accuracy = (fit.base2.cat == df$Activity),
                      Activity = df$Activity)

Acc = sum(df.check$Accuracy)/nrow(df.check)

# calculate accuracy per activity
df.sub = df.check[(df.check$Activity == 'run'),1]
Acc.run = sum(df.sub)/length(df.sub)

df.sub = df.check[(df.check$Activity == 'ride'),1]
Acc.ride = sum(df.sub)/length(df.sub)

df.sub = df.check[(df.check$Activity == 'walk'),1]
Acc.walk = sum(df.sub)/length(df.sub)

df.sub = df.check[(df.check$Activity == 'swim'),1]
Acc.swim = sum(df.sub)/length(df.sub)


Acc.ave = (Acc.ride+Acc.run+Acc.swim+Acc.walk)/4


############ Transform and choose optimal model (2a)

# Linear regression
## Transform some variables with log
#df.cov.tran = df.cov
#df.cov.tran$dEmax = log(df.cov.tran$dEmax + .005) # non-transformed is better correlated
#df.cov.tran$Grademed = log(df.cov.tran$Grademed + .005)
#df.cov.tran$Turnsmin = log(df.cov.tran$Turnsmin)
#df.cov.tran$Ttotal = log(df.cov.tran$Ttotal)
#df.cov.tran$Dtotal = log(df.cov.tran$Dtotal)
#df.cov.tran$Dispmed = log(df.cov.tran$Dispmed)


coef.tran = rep(NA,Ncov)
for (i in 1:Ncov-1) {
  coef.tran[i] = cor(df.cov.tran[,i], df$Tstart)
}

plot(factor(covar), coef, las = 2)
points(factor(covar), coef.tran, las = 2)


# try a complex model. include all single covariates and all 2-way interactions
fm         <- lm( data = df, formula = Tstart ~ . + .:.)
fm.cv      =  cvFit(fm, data = df, y = df$Tstart, K = 10)
MSE.fm.all =  fm.cv$cv^2

# perform AIC and BIC to see which results in low MSE and complexity
# AIC backward
fm.AIC.bac = stepAIC(fm, direction = "backward", k = 2)
# BIC backward
fm.BIC.bac = stepAIC(fm, direction = "backward", k = log(nrow(df)))

# cross-validate each model
fm.AIC.bac.cv = cvFit(fm.AIC.bac, data = df, y = df$Tstart, K = 10)
fm.BIC.bac.cv = cvFit(fm.BIC.bac, data = df, y = df$Tstart, K = 10)

# calculate MSE
MSE.AIC.bac = fm.AIC.bac.cv$cv^2
MSE.BIC.bac = fm.BIC.bac.cv$cv^2 # chose this model







# Logistic regression - all single terms
mat    = data.matrix(df[,colnames(df)!="Activity"],rownames.force = NA)
fm.cat = cv.glmnet(mat, df$Activity, family = "multinomial",
                   nfolds = 10, alpha =0, type.measure = "class")

# use the lambda that yields the minimum mean cv error
fit.cat = predict.cv.glmnet(object = fm.cat, newx = mat,
                            s = "lambda.min", type = "class")

# convert to factor to compare to observations
fit.cat = factor(fit.cat)

# mean cv error (misclassification error)
cvErr = fm.cat$cvm[fm.cat$lambda == fm.cat$lambda.min]


# compart to observation
df.check = data.frame(Accuracy = (fit.cat == df$Activity),
                      Activity = df$Activity)

Acc = sum(df.check$Accuracy)/nrow(df.check)

# calculate accurace per activity
df.sub = df.check[(df.check$Activity == 'run'),1]
Acc.run = sum(df.sub)/length(df.sub)

df.sub = df.check[(df.check$Activity == 'ride'),1]
Acc.ride = sum(df.sub)/length(df.sub)

df.sub = df.check[(df.check$Activity == 'walk'),1]
Acc.walk = sum(df.sub)/length(df.sub)

df.sub = df.check[(df.check$Activity == 'swim'),1]
Acc.swim = sum(df.sub)/length(df.sub)

Acc.ave = (Acc.ride+Acc.run+Acc.swim+Acc.walk)/4

# look at coefficients
fm.cat.coef <- coef(fm.cat, s = "lambda.min")
coef.swim = fm.cat.coef$swim
coef.run  = fm.cat.coef$run
coef.ride = fm.cat.coef$ride
coef.walk = fm.cat.coef$walk


words = colnames(mat)


plot  (factor(as.character(words)), coef.swim[2:10,], ylab ="Coefficient", las = 2, ylim = c(-12,12))
points(factor(as.character(words)), coef.walk[2:10,], pch = 1, cex = 1.5, col = "blue")
points(factor(as.character(words)), coef.run[2:10,],  pch = 1, cex = 1, col = "green")
points(factor(as.character(words)), coef.ride[2:10,], pch = 1, cex = 2, col = "red")
legend(factor(words[1]), 12, c("Swim","Walk","Run","Ride"),
       col = c("black","blue","green","red"),
       pch = c(NA,1,1,1), lty = c(1,NA,NA,NA))


# repeat with interaction terms

mat   = model.matrix(~ . + Grademed:. + Iswim:. + Umed:., df[,colnames(df)!="Activity"],rownames.force = NA)

#mat   = model.matrix(~ . + .:., df[,colnames(df)!="Activity"], rownames.force = NA)
fm.cat = cv.glmnet(mat, df$Activity, family = "multinomial",
                   nfolds = 10, alpha =0, type.measure = "class")

# use the lambda that yields the minimum mean cv error
fit.cat = predict.cv.glmnet(object = fm.cat, newx = mat,
                            s = "lambda.min", type = "class")

# convert to factor to compare to observations
fit.cat = factor(fit.cat)

# mean cv error (misclassification error)
cvErr = fm.cat$cvm[fm.cat$lambda == fm.cat$lambda.min]


# compart to observation
df.check = data.frame(Accuracy = (fit.cat == df$Activity),
                      Activity = df$Activity)

Acc = sum(df.check$Accuracy)/nrow(df.check)

# calculate accurace per activity
df.sub = df.check[(df.check$Activity == 'run'),1]
Acc.run = sum(df.sub)/length(df.sub)
df.sub = df.check[(df.check$Activity == 'ride'),1]
Acc.ride = sum(df.sub)/length(df.sub)
df.sub = df.check[(df.check$Activity == 'walk'),1]
Acc.walk = sum(df.sub)/length(df.sub)
df.sub = df.check[(df.check$Activity == 'swim'),1]
Acc.swim = sum(df.sub)/length(df.sub)

Acc.ave = (Acc.ride+Acc.run+Acc.swim+Acc.walk)/4


# look at coefficients
fm.cat.coef <- coef(fm.cat, s = "lambda.min")
coef.swim = fm.cat.coef$swim
coef.run  = fm.cat.coef$run
coef.ride = fm.cat.coef$ride
coef.walk = fm.cat.coef$walk


words = colnames(mat)

op <- par(mar = c(9,4,4,2) + 0.1)

plot  (factor(as.character(words)), coef.swim[2:32,], ylab ="Coefficient", las = 2, ylim = c(-.6,.6))
points(factor(as.character(words)), coef.walk[2:32,], pch = 1, cex = 1.5, col = "blue")
points(factor(as.character(words)), coef.run[2:32,],  pch = 1, cex = 1, col = "green")
points(factor(as.character(words)), coef.ride[2:32,], pch = 1, cex = 2, col = "red")
legend(1, 12, c("Swim","Walk","Run","Ride"),
       col = c("black","blue","green","red"),
       pch = c(NA,1,1,1), lty = c(1,NA,NA,NA))
par(op)














# plot things
barchart(df$Activity)
barchart(fit.cat)

plot(df$Tstart, ylab = "Start Time")
points(fm.BIC.bac$fitted.values, col = "blue")
