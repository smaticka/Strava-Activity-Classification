# preprocessing for part II
# this script removes some covariates, calculates correlation coefficients, and creates a 
# ggpairs plot with scatter plots, correlation coefficients, and pdfs.

df <- readRDS("TrainingData.rds")

ID <- as.data.frame(cbind(df$Athlete, df$FileID))

## create covariate data frame without response variable

# reassign df without IDs
df = df[,colnames(df)!="Athlete"]
df = df[,colnames(df)!="FileID"]

# remove some covariates
df = df[,colnames(df)!="Umean"]
df = df[,colnames(df)!="Umax"]
df = df[,colnames(df)!="Turnsmean"]
df = df[,colnames(df)!="Ibike"]
df = df[,colnames(df)!="Turnsmin"]

# convert Iswim integer to numeric to apply mean to data frame as matrix
df$Iswim = as.numeric(df$Iswim)

df.cov = df[,colnames(df)!="Tstart"]

## rerun correlation coefficients for kept covariates
Ncov = length(df.cov)

coef = rep(NA,Ncov)
for (i in 1:Ncov-1) {
  coef[i] = cor(df.cov[,i], df$Tstart)
}

# associate variable name to the coefficient
covar = colnames(df.cov)
plot(factor(covar), coef, las = 2)

# mutual correlations
ggpairs(df, las =1)


