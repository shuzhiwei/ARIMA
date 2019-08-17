############################# {COPYRIGHT-TOP} ###
# RDE Group Confidential
# OCO Source Materials
# 
# (C) Copyright RDE Group Corp. 2016-2018 All Rights Reserved.
#
# The source code for this program is not published or otherwise
# divested of its trade secrets, irrespective of what has been
# deposited with the China Copyright Office.
############################# {COPYRIGHT-END} ###

#############################
# R Source Code
# Forecast electricity usage
#
# Made by Yichen Zheng
# 2018/11/07
#############################

#https://blog.csdn.net/gdyflxw/article/details/55509656?_ad0.1106877725084624
#https://segmentfault.com/a/1190000006985454
#https://www.cnblogs.com/sylvanas2012/p/4328861.html
#https://www.cnblogs.com/bradleon/p/6832867.html

#Use setwd("E:/..") to set current working path first
#Copy "electricity5.csv" in the same path

#Load library
library(forecast)

#Load data
dataY <- read.csv("electricity5.csv")
startDate = c(2016,6) #2016/06
monthsForTesting = 6  #Use last 2 months for testing

#Convert to time-series data
data <- ts(dataY, frequency = 12, start = startDate)

#Plot orignal data
plot(data, ylab = "Monthly electricity usage")

#Create training data (without last 2 months)
training <- ts (as.vector(data[1:(dim(as.vector(dataY))[1] - monthsForTesting)]),
                frequency = 12, start = startDate)

#Display training data
tsdisplay(training)

#Step 1.
#Remove go-up by diff 1
diff1 <- diff(training, 1)
tsdisplay(diff1)

#Remove seasional by another diff
diff1a <- diff(diff1, 12)
tsdisplay(diff1a)

#########################################################
#option #1
#auto.arima
#fit <- auto.arima(training, trace=T) #it desn't work well

#########################################################
#option #2
#Get best p,q and P, Q (find MIN AIC) by ourself
d = 1;
D = 1;
best_p = 0;
best_q = 0;
best_P = 0;
best_Q = 0;
best_AIC = NA;

for (p in c(0:2)) {
  for (q in c(0:2)) {
    for (P in c(0:2)) {
      for (Q in c(0:2)) {
        suppressWarnings(f <- try(arima(training, order=c(p,1,q), seasonal=list(order=c(P,1,Q), period=12)), silent = TRUE)) #print(f)
        if (!is.element("try-error", class(f)))
        {
          aic = f$aic #print(aic)
          if (is.na(best_AIC) || aic < best_AIC)
          {
            best_AIC = aic
            best_p = p
            best_q = q
            best_P = P
            best_Q = Q
          }
        }
      }
    }
  }
}
print(paste('Best arima model is: (', best_p, ',1,', best_q,')(', best_P, ',1,', best_Q,')[12]'))

#Create arima model
fit <- arima(training, order=c(best_p,1,best_q), seasonal=list(order=c(best_P,1,best_Q), period=12))
#########################################################

#forecase 2 months
forecastData = forecast(fit, h = monthsForTesting, level = c(99.5))

#Plot
plot(forecastData, 
     ylab = "Monthly electricity usage / 某用电模块的月用电量", 
     main = "Monthly Forecasts - RDE Group / 月用电量预测 - 龙德缘电力",
     sub = "Red - Actual, Blue - Forecast / 红色 - 实际用电量, 蓝色 - 预测值"
)
#lines(forecastData$fitted,col="green")
lines(data,col="red")
