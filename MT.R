library("fpp2")
install.packages("smooth")
library("smooth")
install.packages("openair")
library("openair")
install.packages("dplyr")
library("dplyr")
library("readxl")
library(MASS)


data <- read_excel("C:/Users/RK/Downloads/term/term 3/TSFT/project/MT presentation/TSFT_GroupE_data - mod file.xlsx",sheet = "Nagpur")
summary(data)
data_ts = ts(data$precipMM,start = c(2009,1),frequency = 12)
data_model = window(data_ts,end=c(2017,12))   #model period
data_holdout = window(data_ts,start=c(2018,1))  #hold out period


#plot
autoplot(data_ts)
monthplot(data_ts)
seasonplot(data_ts)





#Additive error models

fit1 <- ets(data_model,model = "ANN")
summary(fit)
autoplot(fit)



fit2 <- ets(data_model,model = "ANA")   #auto selected
summary(fit)
autoplot(fit)


fit3 <- ets(data_model,model = "AAN")  
summary(fit)
autoplot(fit)

fit4 <- ets(data_model,model = "AAA")  
summary(fit)
autoplot(fit)


#fit_auto= ets(data_ts)
#summary(fit_auto)
#autoplot(fit_auto)
#multiplicative error
#no model 

autoplot(forecast(fit1,h =25))
accuracy(forecast(fit1,h =25),data_holdout)


autoplot(forecast(fit2,h =25))
accuracy(forecast(fit2,h =25),data_holdout)


autoplot(forecast(fit3,h =25))
accuracy(forecast(fit3,h =25),data_holdout)


autoplot(forecast(fit4,h =25))
accuracy(forecast(fit4,h =25),data_holdout)





#regression with dummy seasonality
#Quadratic trend
reg_model = tslm(data_model ~ trend + I(trend^2)+ season)
reg_model

fcast <- forecast(reg_model,h=25)

autoplot(fcast) +
  ggtitle("Forecasts using regression") +
  xlab("Year") + ylab("rainfall")

accuracy(fcast,data_holdout)

#linear trend

reg_model1 = tslm(data_model ~ trend + season)
reg_model1

fcast1 <- forecast(reg_model1,h=25)

autoplot(fcast1) +
  ggtitle("Forecasts using regression") +
  xlab("Year") + ylab("rainfall")

accuracy(fcast1,data_holdout)

#regression without trend

reg_model2 = tslm(data_model ~ season)
reg_model2
#autoplot(fitted(reg_model2))

fcast2 <- forecast(reg_model2,h=25)

autoplot(fcast2) +
  ggtitle("Forecasts using regression") +
  xlab("Year") + ylab("rainfall")

accuracy(fcast2,data_holdout)



#without tslm
n=length(data_model)
t <- matrix(1:n,n,1)

Jan <- matrix(c(1,matrix(0,11,1)),n,1)
Feb <- matrix(c(0,1,matrix(0,10,1)),n,1)
Mar <- matrix(c(0,0,1,matrix(0,9,1)),n,1)
Apr <- matrix(c(matrix(0,3,1),1,matrix(0,8,1)),n,1)
May <- matrix(c(matrix(0,4,1),1,matrix(0,7,1)),n,1)
Jun <- matrix(c(matrix(0,5,1),1,matrix(0,6,1)),n,1)
Jul <- matrix(c(matrix(0,6,1),1,matrix(0,5,1)),n,1)
Aug <- matrix(c(matrix(0,7,1),1,matrix(0,4,1)),n,1)
Sep <- matrix(c(matrix(0,8,1),1,matrix(0,3,1)),n,1)
Oct <- matrix(c(matrix(0,9,1),1,matrix(0,2,1)),n,1)
Nov <- matrix(c(matrix(0,10,1),1,0),n,1)
Dec <- matrix(c(matrix(0,11,1),1),n,1)

t2 <- t^2  #trend variable is insignificant

y_lm2 <- lm ( data_model ~ t+ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov )

step_ylm2  <- stepAIC(y_lm2, direction="both")
lm3_input <- data.frame( t,Jan ,  Feb , Mar , Apr , May , Jun , Jul , Aug , Nov)
y_lm3 <- lm(data_model ~ ., lm3_input)
summary(y_lm3) 
extractAIC(y_lm3)


#predict
#t0<-(length(data_model)+1):(length(data_model)+length(data_holdout))
#y_lm5 = lm ( data_model ~ t0 + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov )

y_hat = predict(y_lm3,lm3_input)
accuracy(y_hat,data_model)


# 
# library(car)
# linearHypothesis(y_lm3, "Jul = Aug")
# linearHypothesis(y_lm3, c("Jul = Aug", "Jun = Nov"))
# linearHypothesis(y_lm3, c("Jul = Aug", "Jun = Nov", "Mar=May"))
# 
# Jul_aug <- Jul + Aug
# Jun_nov <- Jun + Nov
# Mar_may <- Mar + May
# lm4_input <- data.frame( Jan ,  Feb , Mar , Apr , May , Jun_nov , Jul_aug)
# y_lm4 <- lm(data_model ~ ., lm4_input)
# summary(y_lm4) 
# extractAIC(y_lm4)
# yfit2 <- fitted(y_lm4)
# 
# lm5_input <- data.frame(t, t2 , Jan ,  Feb , Mar_may , Apr  , Jun_nov , Jul_aug)
# y_lm5 <- lm(y ~ ., lm5_input)
# summary(y_lm5) 
# extractAIC(y_lm5)






























