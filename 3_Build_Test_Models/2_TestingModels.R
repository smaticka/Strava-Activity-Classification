# this is part 3 of the mini project in ms&e 226

library(graphics)

# read in saved test set
df.test <- readRDS("TestData.rds")

ID <- as.data.frame(cbind(df.test$Athlete, df.test$FileID))

## create covariate data frame without response variable

# reassign df.test without IDs
df.test = df.test[,colnames(df.test)!="Athlete"]
df.test = df.test[,colnames(df.test)!="FileID"]

# remove some covariates
df.test = df.test[,colnames(df.test)!="Umean"]
df.test = df.test[,colnames(df.test)!="Umax"]
df.test = df.test[,colnames(df.test)!="Turnsmean"]
df.test = df.test[,colnames(df.test)!="Ibike"]
df.test = df.test[,colnames(df.test)!="Turnsmin"]

# convert Iswim integer to numeric to apply mean to data frame as matrix
df.test$Iswim = as.numeric(df.test$Iswim)


########## test models


##### linear regression
Yhat       <- predict(fm.BIC.bac, df.test)
MSE.LinReg  = mean((df.test$Tstart - Yhat)^2)

# Predict mean as the most basic predictive model 
Yhat.base1       = rep(fm.base1, length(df.test$Tstart))
MSE.LinReg.base1 =  mean((df.test$Tstart - Yhat.base1)^2)

# basic predictive model using only a few covariates
Yhat.base2       <- predict(fm.base2, df.test)
MSE.LinReg.base2  = mean((df.test$Tstart - Yhat.base2)^2)

###### logistic regression
# use dummy model where it guesses the most likely activity (56% of activities were runs)
MSE.LogReg.base1 = length(df.test[df.test$Activity=="run",1])/dim(df.test)[1]

mat           = data.matrix(data.frame(df.test$Tstart, df.test$Ttotal, 
                                       df.test$Turnsmed, df.test$Umed), rownames.force = NA)

# regularization with lamda that minimized classification error 
# (performed cv via a penalized maximum likelihood iterative scheme)
fit.base2.cat = predict.cv.glmnet(object = fm.base2.cat, newx = mat,
                            s = "lambda.min", type = "class")

# convert to factor to compare to observations
fit.base2.cat = factor(fit.base2.cat)

# mean cv error (misclassification error)
cvErr = fm.base2.cat$cvm[fm.base2.cat$lambda == fm.base2.cat$lambda.min]
levels(fit.base2.cat) = c(levels(fit.base2.cat),"swim","walk")

# compare to observation
df.check = data.frame(Accuracy = (fit.base2.cat == df.test$Activity),
                      Activity = df.test$Activity)

Acc = sum(df.check$Accuracy)/nrow(df.check) # accuracy

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

# chosen model
mat   = model.matrix(~ . + Grademed:. + Iswim:. + Umed:.,
                     df.test[,colnames(df.test)!="Activity"],rownames.force = NA)
fit.cat = predict.cv.glmnet(object = fm.cat, newx = mat,
                            s = "lambda.min", type = "class")

# convert to factor to compare to observations
fit.cat = factor(fit.cat)

# mean cv error (misclassification error)
cvErr = fm.cat$cvm[fm.cat$lambda == fm.cat$lambda.min]

# compart to observation
df.check = data.frame(Accuracy = (fit.cat == df.test$Activity),
                      Activity = df.test$Activity)

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

# fit base2 lin reg model on test data
fm.base2.test    <- lm(df.test, formula = Tstart ~ 1 + dEmax + Dtotal + Ttotal + Turnsmed)

#### part 2 c
# bootstrap OLS

# create function for mean and median
coef.boot = function(data,indices) {
  d = data[indices]
  fm <- lm(d, formula = Tstart ~ 1 + dEmax + Dtotal + Ttotal + Turnsmed)
  
  return(fm$coefficients)
}

B = 100
n = nrow(df)
boot.int = rep(NA,B)
boot.b1 = rep(NA,B)
boot.b2 = rep(NA,B)
boot.b3 = rep(NA,B)
boot.b4 = rep(NA,B)

for (i in 1:B) {
  X.sample = df[sample.int(n, size = n, replace = TRUE),]
  fun.out = coef.boot(X.sample)
  
  boot.int[i] = fun.out[1]
  boot.b1[i]  = fun.out[2]
  boot.b2[i]  = fun.out[3]
  boot.b3[i]  = fun.out[4]
  boot.b4[i]  = fun.out[5]
}

CI.mean.lower = rep(NA,5)
CI.mean.upper = rep(NA,5)
# calculate confidence intervals
CI.mean.lower[1] = quantile(boot.int, 0.025)
CI.mean.lower[2] = quantile(boot.b1, 0.025)
CI.mean.lower[3] = quantile(boot.b2, 0.025)
CI.mean.lower[4] = quantile(boot.b3, 0.025)
CI.mean.lower[5] = quantile(boot.b4, 0.025)

CI.mean.upper[1] = quantile(boot.int, 0.975)
CI.mean.upper[2] = quantile(boot.b1, 0.975)
CI.mean.upper[3] = quantile(boot.b2, 0.975)
CI.mean.upper[4] = quantile(boot.b3, 0.975)
CI.mean.upper[5] = quantile(boot.b4, 0.975)

term = c("Intercept","dEmax","Dtotal","Ttotal","Turnsmed")

plot.new()
plot((CI.mean.lower+CI.mean.upper)/2,type="p",ylim=c(-2,14),xlim=c(.8, 5.2),
     ylab = "Confidend Intervals",xlab = "Term Number")
segments(1, CI.mean.lower[1], x1=1, y1=CI.mean.upper[1])
segments(2, CI.mean.lower[2], x1=2, y1=CI.mean.upper[2])
segments(3, CI.mean.lower[3], x1=3, y1=CI.mean.upper[3])
segments(4, CI.mean.lower[4], x1=4, y1=CI.mean.upper[4])
segments(5, CI.mean.lower[5], x1=5, y1=CI.mean.upper[5])

segments(0,0,6,0,lty = 2)

out = summary(fm.base2)
R.Coef = out$coefficients[,1]
R.SE   = out$coefficients[,2]
  
points(c(1.2,2.2,3.2,4.2,5.2),R.Coef,col="red")
segments(1.2, R.Coef[1]-1.96*R.SE[1], x1=1.2, y1= R.Coef[1]+1.96*R.SE[1], col="red")
segments(2.2, R.Coef[2]-1.96*R.SE[2], x1=2.2, y1= R.Coef[2]+1.96*R.SE[2], col="red")
segments(3.2, R.Coef[3]-1.96*R.SE[3], x1=3.2, y1= R.Coef[3]+1.96*R.SE[3], col="red")
segments(4.2, R.Coef[4]-1.96*R.SE[4], x1=4.2, y1= R.Coef[4]+1.96*R.SE[4], col="red")
segments(5.2, R.Coef[5]-1.96*R.SE[5], x1=5.2, y1= R.Coef[5]+1.96*R.SE[5], col="red")


out = summary(fm)
R.Coef.2a = out$coefficients[c(1,6,4,3,8),1]
R.SE.2a     = out$coefficients[c(1,6,4,3,8),2]

  
  
  
  
  