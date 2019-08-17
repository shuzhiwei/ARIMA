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
#https://www.jianshu.com/p/5346e38081e2

#Use setwd("E:/..") to set current working path first
#Copy "electricity_zpark-imway_daily.csv" in the same path

#Load library
library(forecast)
library(lubridate) #You may need to install 'lubridate' first

#Load data
dataRaw <- read.csv("electricity_zpark-imway_daily.csv")
weeksForTesting = 4  #Use last 4 weeks for testing
weeksToIgnore = 18 #Ignore the some begining weeks, where data is not good
beginOfWeek = 1 #The begin day of week [1,7]

#Convert daily to weekly data
beginIndex = 1 #Search begin index by beginOfWeek
totalLength = min(lengths(dataRaw))
dates = as.vector(dataRaw$date)
values = as.vector((dataRaw$value))
for(index in c(1:totalLength)) {
  date = dates[index]
  if (wday(date) == beginOfWeek)
  {
    beginIndex = index
    break
  }
}
startDate = dates[beginIndex]
print(paste('The start date is:', startDate))
#
dataWeekly = rep(0, floor((totalLength - (beginIndex - 1))/7))
daySum = 0
valueSum = 0
weekIndex = 0
for(index in c(beginIndex:totalLength)) {
  daySum = daySum + 1
  valueSum = valueSum + values[index]
  if (daySum >= 7)
  {
    weekIndex = weekIndex + 1
    dataWeekly[weekIndex] = valueSum
    daySum = 0
    valueSum = 0
  }
}
print(paste('The total number of weeks converted is:', length(dataWeekly)))

#Convert to time-series data
data <- ts(dataWeekly, frequency = 1)

#Plot orignal data
plot(data, ylab = "Weeky electricity usage (Orignal)", 
     xlab = paste("Number of weeks starting from", startDate))

#Ignore begining weeks
if (weeksToIgnore > 0)
{
  startDate = as.Date(startDate) + weeks(weeksToIgnore)
  dataWeekly = dataWeekly[(1+weeksToIgnore):length(dataWeekly)]
  data <- ts(dataWeekly, frequency = 1)
  plot(data, ylab = "Weeky electricity usage", 
       xlab = paste("Number of weeks starting from", startDate))
}

#Create training data (without last 4 weeks)
training <- ts (as.vector(data[1:(length(dataWeekly) - weeksForTesting)]),
                frequency = 1)

#Display training data
tsdisplay(training)

#Step 1.
#Remove go-up by diff 1
diff1 <- diff(training, 1)
tsdisplay(diff1)

#Remove seasional by another diff
# diff1a <- diff(diff1, 52)
# tsdisplay(diff1a)

#########################################################
#option #1
#auto.arima
# fit <- auto.arima(training, trace=T) #it desn't work well

#########################################################
#option #2
#Get best p,q and P, Q (find MIN AIC) by ourself
best_p = 4 # -1; # Give -1 to run the search, and it takes time.
d = 1;
best_q = 0;
best_P = 0;
D = 1;
best_Q = 0;
best_frequency = 52;
best_AIC = NA;

if (best_p < 0) #if best_p is not given, search them
{
  for (frequency in c(4,36,48,52)) {
    for (p in c(4,9,12)) {
      for (q in c(0:1)) {
        for (P in c(0:1)) {
          for (Q in c(0:1)) {
            suppressWarnings(f <- try(arima(training, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=frequency)), silent = TRUE)) #print(f)
            if (!is.element("try-error", class(f)))
            {
              aic = f$aic #print(aic)
              if (is.na(best_AIC) || aic < best_AIC)
              {
                best_AIC = aic
                best_frequency = frequency
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
  }
}

print(paste('Best arima model is: (', best_p, ',', d, ',', best_q,')(', best_P, ',', D,',', best_Q,')[', best_frequency,']'))

#Create arima model
suppressWarnings(fit <- arima(training, order=c(best_p,d,best_q), seasonal=list(order=c(best_P,D,best_Q), period=best_frequency)))
#########################################################

#forecase 4 weeks
forecastData = forecast(fit, h = weeksForTesting, level = c(99.5))

#Plot
plot(forecastData, 
     xlab = paste("Weeks starting from", startDate, "/ 从", startDate, "开始的周数"),
     ylab = "Weekly electricity usage / 周用电总量", 
     main = "Weekly Forecasts - RDE Group / 周用电量预测 - 龙德缘电力",
     sub = "Red - Actual, Blue - Forecast / 红色 - 实际用电量, 蓝色 - 预测值"
)
#lines(forecastData$fitted,col="green")
lines(data,col="red")
