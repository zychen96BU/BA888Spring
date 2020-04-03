### SET WORKING DIRECTORY ---------------------------------------------------
currDirectory=dirname(rstudioapi::getSourceEditorContext()$path)
setwd(currDirectory)

### LIBRARIES  -------------------------------------
library(openxlsx)
library(zoo)
library(forecast)
library(scorer)
library(plotly)
library(aTSA)
library(reshape2)

### Import the Data --- ( in case you don't want to use any library to open the file
# you can transform the file to .csv, and use the read.csv() function)
dfbose = read.xlsx("bose_data.xlsx")

# eyeball the data ---
# Scatterplot:
plot(dfbose$week, dfbose$demand, main = "Scatterplot",
     xlab = "week", ylab = "demand",
     pch = 19, frame = FALSE)
# Plot line: ---
plot(dfbose$week, dfbose$demand, type="l", xlab="week",
     ylab="demand" )

# 100 data points in total; 80-20 split
train_bose <- dfbose[1:80,]

## Moving Average method
# train_bose.demand[:10] :
train_bose$demand[1:10]
# train_bose.loc[:10,"demand"] :
train_bose$demand[1:11]

# use the rolling_mean function to compute rolling means
# note that rolling_mean on row 3 is equal to (row 1 + row 2 + row 3)/3
# this is the MA(3) forecast for row 4
# (use of function rollmean of package ZOO)
rollmean(train_bose$demand[1:11], k = 3, fill = NA, align = "right")

# MA(3) forecast for week 4 through 80
rollmean(train_bose$demand[1:79], k = 3, fill = NA, align = "right")[3:79]

#Mean Squared Error of MA(3)
# check shape equivalence first
# actual for week 4 through week 80
train_bose$demand[4:nrow(train_bose)]
# MA(3) forecast for week 4 through week 80
rollmean(train_bose$demand[1:79], k = 3, fill = NA, align = "right")[3:79]
# compare the actual with forecast, summarize into MSE
mean_squared_error(train_bose$demand[4:nrow(train_bose)], rollmean(train_bose$demand[1:79], k = 3, fill = NA, align = "right")[3:79])

# try N = 1 to 30; find the smallest MSE using grid search
# for loop review: 
# N = 8 is the best
for (N in (1:30)) {
  print(c(N,mean_squared_error(train_bose$demand[31:nrow(train_bose)], rollmean(train_bose$demand[1:79], k = N, fill = NA, align = "right")[30:79])))
}

# store MA(8) forecast in the testing/holdout sample: last 20 rows
dfbose[80:100,'ma']=rollmean(train_bose$demand, k = 8, fill = NA, align = "right")[80]

## Simple Exponential Smoothing method
# need to update to the most recent statsmodels
# in prompt, do this: conda install statsmodels
# "test drive" with an example: alpha = 0.5
# ignore the first 10 data rows for burn-in
ExponentialSmoothing <- HoltWinters(train_bose$demand, alpha=0.5, beta=FALSE, gamma=FALSE)
ExponentialSmoothing$fitted[10:79]  
mean_squared_error(train_bose$demand[11:nrow(train_bose)], ExponentialSmoothing$fitted[10:79])


### We get DIFFERENT results BECAUSE: Alpha COULD NOT be ZERO--------------------------------------------------

# alpha needs to be between 0 and 1, use grid search
# alpha = 0 is the best
for (alpha in (seq(0.1,1.0,0.1))) {
  ExponentialSmoothing <- HoltWinters(train_bose$demand, alpha=alpha, beta=FALSE, gamma=FALSE)
  print(c(alpha,mean_squared_error(train_bose$demand[11:nrow(train_bose)], ExponentialSmoothing$fitted[10:79])))
}

# store SES(0) in the test sample [in our case, we use alpha=0.01 !!!]
# difference between .predict() vs .forecast()
ExponentialSmoothing <- stats::HoltWinters(train_bose$demand, alpha=0.01, beta=FALSE, gamma=FALSE)
dfbose$ses <- NA
dfbose$ses[81:100] <- as.data.frame(forecast:::forecast.HoltWinters(ExponentialSmoothing, h=1))[,1]


# compare MSE in the holdout sample for MA(8) and SES(0)
mean_squared_error(dfbose$demand[81:nrow(dfbose)],dfbose$ma[81:nrow(dfbose)]) ##CORRECT
mean_squared_error(dfbose$demand[81:nrow(dfbose)],dfbose$ses[81:nrow(dfbose)])  ## Different because we could not use smoothing_Level~Alpha=0.0

# plot forecast vs actual
#creat a new data frame where we add train and test
dfbose$Train <- NA
dfbose$Test <- NA
dfbose$Train[1:81] <- dfbose$demand[1:81]
dfbose$Test[81:100] <- dfbose$demand[81:100]
fig <- plot_ly(dfbose, x = ~week, y = ~Train, name = 'train', type = 'scatter', mode = 'lines',
               line = list(color = 'rgb(205, 12, 24)', width = 4)) 
fig <- fig %>% add_trace(y = ~Test, name = 'test', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
fig <- fig %>% add_trace(y = ~ma, name = 'ma', line = list(color = 'rgb(2, 12, 124)', width = 4, dash = 'dash')) 
fig <- fig %>% add_trace(y = ~ses, name = 'ses', line = list(color = 'rgb(22, 196, 167)', width = 4, dash = 'dash')) 
fig <- fig %>% layout(title = "Comparaison",
                      xaxis = list(title = "weeks"),
                      yaxis = list (title = "values"))
fig
#Acceptable 

#----------------
#---------------


## Double ES method for dealing with trend
# load IE data
dfie = read.xlsx("ie_data.xlsx")

# eyeball test
# Plot line: ---
plot(dfie$week, dfie$subscription, type="l", xlab="week",
     ylab="subscription" )

# DF unit root test
# p-value > 0.05 -> not rejecting H0 -> nonstationary -> so we need DES
DFunit <- adf.test(dfie$subscription)

#print(paste0("ADF Statistic: ", DFunit[0]))
#print(paste0("p-value: ", DFunit[2]))

# training sample first 30 weeks
# allow 5 weeks of burn in period
# DES for weeks 6-30, given alpha and beta is calculated by
# alpha = smoothing_level, beta = smoothing_slope
ExponentialSmoothing <- HoltWinters(dfie$subscription[1:30], alpha=0.3, beta=0.5, gamma=FALSE)
ExponentialSmoothing$fitted[4:28,1]

# write a function that takes alpha and beta as input and produces the traning sample MSE
DESMSE <- function(Alpha,Beta){
  ExponentialSmoothing <- HoltWinters(dfie$subscription[1:30], alpha=Alpha, beta=Beta, gamma=FALSE)
  return(mean_squared_error(dfie$subscription[5:30] , ExponentialSmoothing$fitted[3:28,1]))
}

# test the function  ------------#Not the same (almost,  need to check the test)
DESMSE(0.5,0.5)
DESMSE(0.1,0.5)
DESMSE(0.5,0.1)

#This time, we introduce a more formal approach to do grid search
# the function below allows you to construct a dataframe of alpha-beta combinations
Grid_Search <- function(ALpha,BEta){
  n <- length(ALpha)
  m <- length(BEta)
  nbr_row=1
  df_Grid <- matrix(0, nrow = (n*m), ncol = 2)
  for (i in (1:n)) {
    for (j in (1:m)) {
      df_Grid[nbr_row,1] <- ALpha[i]
      df_Grid[nbr_row,2] <- BEta[j]
      nbr_row <- nbr_row+1
    }
  }
  df_Grid <- data.frame(df_Grid)
  colnames(df_Grid) <- c("ALpha", "BEta")
  return(df_Grid)
}

#Or we can use in a simple form :
expand_grid=function(dictionary){
  return(as.data.frame(expand.grid(dictionary)))
}

# define the alpha and beta values
dictionary = list(alpha= seq(from = 0.01, to = 1, by = 0.01) , beta= seq(from = 0.01, to = 1, by = 0.01))
alpha=seq(0.01,1,0.01)
beta=seq(0.01,1,0.01)


# construct alpha,beta values to try 
des_results <- Grid_Search(alpha, beta)
#or:
des_results = expand_grid(dictionary)


# add a column called mse, as a place holder for putting in the MSE results later
des_results$mse <- 0

# now, we reuse the DESMSE function above to cycle through all alpha beta values.
# this is a giant loop, so let's time it!
# time a small version if dealing with large loops
x=subset(des_results, alpha == 0.5 & beta == '0.1')

start_Time = Sys.time()

for (alpha in seq(from = 0.01, to = 1, by = 0.01)){
  for (beta in seq(from = 0.01, to = 1, by = 0.01)){
    des_results[des_results$alpha == alpha & des_results$beta == beta,]$mse=DESMSE(alpha,beta)
  }
}
end_Tme = Sys.time()

print(paste0("time for the loop  ",end_Tme-start_Time," s"))

# find the best alpha beta
# alpha = 1, beta = 0.04
head(des_results[order(des_results$mse, decreasing = FALSE),])

# store DES(1,0.19) [alpha = 1, beta = 0.04] in the test sample
dfie$des <- NA
ExponentialSmoothing <- stats::HoltWinters(dfie$subscription[1:30], alpha=0.9, beta=0.04, gamma=FALSE)
dfie$des[31:60] <- as.data.frame(forecast:::forecast.HoltWinters(ExponentialSmoothing, h=30))[,1]

# MSE in the holdout sample
mean_squared_error(dfie$subscription[31:60] , dfie$des[31:60])

#plot forecast vs actual
dfie$Train <- NA
dfie$Test <- NA
dfie$Train[1:31] <- dfie$subscription[1:31]
dfie$Test[31:60] <- dfie$subscription[31:60]
fig <- plot_ly(dfie, x = ~week, y = ~Train, name = 'train', type = 'scatter', mode = 'lines',
               line = list(color = 'rgb(205, 12, 24)', width = 4)) 
fig <- fig %>% add_trace(y = ~Test, name = 'test', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
fig <- fig %>% add_trace(y = ~des, name = 'Des*', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
fig <- fig %>% layout(title = "Comparaison",
                      xaxis = list(title = "weeks"),
                      yaxis = list (title = "Subscription Quantity"))
fig
## NOt  The  Same ---- need verification

## Exercise
# Single ES
dfjeans = read.xlsx("jeans_data.xlsx")
dfjeans$promo_114=as.integer(dfjeans$price==114.75)
dfjeans$promo_101=as.integer(dfjeans$price == 101.25)
X=as.data.frame( cbind(promo_114=dfjeans$promo_114,promo_101=dfjeans$promo_101, se_indicator=dfjeans$se_indicator ))
olsjeans <- lm(dfjeans$sales ~ ., data = X)
olsjeans$residuals

######-"-----
#####-----------

# Double ES
# .pkl is a pandas-style dataframe format
# the solution i proporse is to read the file in python and save it in .csv format
# check this website for methode to read .pkl from R
# :  https://www.ankuroh.com/programming/data-analysis/reading-pickle-file-in-r/
#install.packages('reticulate')
#import pandas as pd
#def read_pickle_file(file):
# pickle_data = pd.read_pickle(file)
#return pickle_data
#require("reticulate")
#source_python("pickle_reader.py")
#pickle_data <- read_pickle_file("C:/bitcoin_data.pkl")
#plot(x=dfbitcoin$date,y=dfbitcoin$price)
#   OR, a simple way : We did read the .pkl file with python and saved it in a .csv format, which we are using now
dfbitcoin = as.data.frame(read.csv('dfbitcoin.csv'))
dfbitcoin$date <- as.Date(dfbitcoin$date)
plot(x=dfbitcoin$date, y=dfbitcoin$price, type="l", xlab="date",
     ylab="price" )


# Triple ES
# load data, two issues:
# 1. month is loaded as index
# 2. convert format from wide to long
dfairline = read.xlsx("airline_data.xlsx")

# reset index
# dfairline = dfairline.reset_index()
# wide to long
mdf <- melt(data = dfairline, id.vars = "X1")
dfairline <- mdf

# rename
colnames(dfairline)= c("month","year","load")

# de-seasonalize series
# sample average
dfairline$avg = mean(dfairline$load, na.rm = TRUE)

# monthly average
s <- with(dfairline, aggregate( load ~ month, FUN = mean, na.rm=T))
s <- s %>% arrange(match(s$month, month.name))
dfairline$month_avg = s$load

# seasonal factor
dfairline$season_factor = (dfairline$month_avg / dfairline$avg)
# de-sesonalized series
dfairline$de_load = (dfairline$load / dfairline$season_factor)
