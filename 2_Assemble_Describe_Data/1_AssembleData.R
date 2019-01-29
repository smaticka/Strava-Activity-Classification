# Filename: ProcessData.R
#
# Author: Sam Maticka
#
# Description: This script reads in .csv files containing covariates (processed in MATLAB)
rm(list=ls()) # clear work environment

# location of this file
setwd('/Users/smaticka/Box Sync/stanford/Classes/MS&E 226 small data/Strava-Activity-Classification/I_Data_Files/csv files/')

df.Sam.swim <- read.csv('Samswim.csv', header = TRUE)
df.Adam.swim <- read.csv('Adamswim.csv', header = TRUE)
df.Adam.ride <- read.csv('Adamride.csv', header = TRUE)
df.Adam.run <- read.csv('Adamrun.csv', header = TRUE)
df.Adam.walk <- read.csv('Adamwalk.csv', header = TRUE)

# combine all activities into one data frame
df = rbind(df.Sam.swim, df.Adam.run, df.Adam.ride, df.Adam.swim, df.Adam.walk)

# # replace the start time covariate with time
# df$Tstart = df$Time
# df = df[,colnames(df)!="Time"]

# shuffle the data frame (random sample without replacement)
# (column means were the same before and after shuffling. good.)
n = dim(df)[1]
df = df[sample.int(n, size = n, replace = FALSE),]

# separate the data into training and test
# use a seed in case I need to rerun
set.seed(123)

# divide total number of total observations into training and testing
train.ind = sample(1:n, 0.8*n)
df.train = df[train.ind,]
df.test  = df[-train.ind,]

saveRDS(df.train, file="../TrainingData.rds")
saveRDS(df.test, file="../TestData.rds")




