#set working directory
setwd("D:/R files/time series/R time_series/daily total female california")

#import library
library(astsa)

#read data
df <- read.csv("D:/R files/time series/R time_series/daily total female california/daily-total-female-births-in-cal.csv")

#get the no of birth column
no_of_birth<-df$Daily.total.female.births.in.California..1959

#use the date format to format date column
df$Date<-as.Date(df$Date,"%m,%d,%Y")

#plot time series
plot.ts(no_of_birth, main="no of birth in california in 1959" , ylab="no. of birth")

#test for autocoorelation
Box.test(no_of_birth , lag = log(length(no_of_birth)))

#plot differenced time series 
plot.ts(diff(no_of_birth),main = "differeenced series" , ylab="")

#test cooreletion in differeenced data
Box.test(diff(no_of_birth), lag = log(length(diff(no_of_birth))))

# acf and pacf of differenecd data
acf(diff(no_of_birth))
pacf(diff(no_of_birth))

#fit various models
model1<-arima(no_of_birth , order=c(0,1,1))
sse1<-sum(model1$residuals^2)
model1_test<-Box.test(model1$residuals , lag=log(length(model1$residuals)))

model2<-arima(no_of_birth , order=c(0,1,2))
sse2<-sum(model2$residuals^2)
model2_test <-Box.test(model2$residuals , lag=log(length(model2$residuals)))

model3<-arima(no_of_birth , order=c(7,1,1))
sse3<-sum(model3$residuals^2)
model3_test <-Box.test(model3$residuals , lag=log(length(model3$residuals)))

model4<-arima(no_of_birth , order=c(7,1,2))
sse4<-sum(model4$residuals^2)
model4_test<-Box.test(model4$residuals , lag=log(length(model4$residuals)))

#create data frame
df_arima <-data.frame(row.names = c("AIC" , "SSE" , "P-value"),
                      c(model1$aic , sse1 , model1_test$p.value),
                      c(model2$aic , sse2 , model2_test$p.value),
                      c(model3$aic , sse3 , model3_test$p.value),
                      c(model4$aic , sse4 , model4_test$p.value))
colnames(df_arima)<-c("arima(0,1,1)","arima(0,1,2)","arima(7,1,1)","arima(7,1,2)")

format(df_arima , scientific = FALSE)

#fit sarima model 
sarima(no_of_birth , 0,1,2,0,0,0)

