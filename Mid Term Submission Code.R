rm(list=ls())
getwd()
setwd("C:/Users/danis/Desktop/TermIII/TSFT/Project")

library(fpp2)

# Answer 1

# Reading data
library("readxl")
nagpur_data = data.frame(read_excel("C:/Users/danis/Desktop/TermIII/TSFT/Project\\TSFT_GroupE_data - mod file.xlsx",sheet = "Nagpur"))

nagpur_data = nagpur_data[-c(3:19,21:25)]
  
# Creating timeseries object
nagpur_ts <- ts(nagpur_data$precipMM, start=c(2009,1), frequency = 12)
nagpur_ts
# Creating ts for model & val period
nag_model <- window(nagpur_ts, end= c(2017,12));
nag_val<- window(nagpur_ts, start=c(2018,1));
n_model<-length(nag_model); n_val<-length(nag_val);
nag_m <- nagpur_data[1:n_model,];
nag_v <- nagpur_data[(n_model+1):(n_model+n_val),];



# Summary statistics
Acf(nagpur_ts)
Pacf(nagpur_ts)

# Plotting data
plot(nagpur_ts) 


### A. Classical Decomposition Model

# Using decompose function in R

#Error Function
Acc <- function(Yhat,Y) 
{
  error <- Y - Yhat
  err <- error[!is.na(error)]	#remove NA
  Y <- Y[!is.na(error)]
  MSE <- round(mean(err^2), 2)
  MAD <- round(mean(abs(err)), 2)
  MAPE <- round(mean(abs(err/Y)) * 100, 3)
  ErrVec <- cbind(MSE, MAD, paste(toString(MAPE),"%"))
  colnames(ErrVec) <- c("MSE","MAD","MAPE")
  return(ErrVec)
}

# Using Additive seasonality

add.de.nag <- decompose(nag_model, type="additive")  
plot(add.de.nag)
fit_add <- add.de.nag$trend + add.de.nag$seasonal
error_add <- nag_model - fit_add
pval1 <- round(Box.test(error_add, lag = 13, type =  "Ljung-Box")$p.value,5)
Acc(fit_add,nagpur_ts)
out1 <- cbind("Additive", Acc(fit_add,nagpur_ts),pval1)
out1

# Using Multiplicative seasonality

mult.de.nag <- decompose(nag_model, type="mult")
plot(mult.de.nag)
fit_mult <- mult.de.nag$trend * mult.de.nag$seasonal
error_mult <- nag_model - fit_mult
pval2 <- round(Box.test(error_mult, lag = 13, type =  "Ljung-Box")$p.value,5)
out2 <- cbind("Multiplicative", Acc(fit_mult,nagpur_ts), pval2)
out2



### Quadratic trend

# Creating serial number
nag_m$sno <- 1:n_model
nag_v$sno <- (n_model+1):(n_model+n_val)

# Trend Estimation by LS regression on grouped data
freq<-12
grpYnagdata <- aggregate(x=nag_m$precipMM, by=list(nag_m$year),  FUN="mean")
grpYnagdata$t <- freq *(1:nrow(grpYnagdata)) - (freq/2 - 0.5)		# for even period = frequency. modify for odd
head(grpYnagdata)
names(grpYnagdata) <- c("year", "yprecipMM","t")

#Fitting quadratic model
model_1 <- lm(yprecipMM ~ t + I(t^2),  data=grpYnagdata)
nag_m$trend_quad <- model_1$coefficients[1] + model_1$coefficients[2]*nag_m$sno+ model_1$coefficients[3]*((nag_m$sno)^2)

#Predicting trend for holdout period
newdata = data.frame(t=(n_model+1):(n_model+n_val)) 
nag_v$trend_quad <- predict(model_1, newdata) 

nag_v$trend_quad
nag_m$trend_quad


### Linear Trend

# Creating serial number
nag_m$sno <- 1:n_model
nag_v$sno <- (n_model+1):(n_model+n_val)

# Trend Estimation by LS regression on grouped data
freq<-12
grpYnagdata <- aggregate(x=nag_m$precipMM, by=list(nag_m$year),  FUN="mean")
grpYnagdata$t <- freq *(1:nrow(grpYnagdata)) - (freq/2 - 0.5)		# for even period = frequency. modify for odd
head(grpYnagdata)
names(grpYnagdata) <- c("year", "yprecipMM","t")

#Fitting linear model
model_2 <- lm(yprecipMM ~ t,  data=grpYnagdata)
nag_m$trend_linear <- model_2$coefficients[1] + model_2$coefficients[2]*nag_m$sno

#Predicting trend for holdout period
newdata = data.frame(t=(n_model+1):(n_model+n_val)) 
nag_v$trend_linear <- predict(model_2, newdata) 

nag_v$trend_linear
nag_m$trend_linear


### Seasonalities using Quadratic trend

###  Seasonal estimation  -- ratio to trend method
nag_m$r2QT <- nag_m$precipMM/nag_m$trend_quad
mthnagdata_r2QT <- aggregate(x=nag_m$r2QT, by=list(nag_m$month),  FUN="mean")
names(mthnagdata_r2QT) <- c("month","r2QT.Seasonal")
sum <- sum(mthnagdata_r2QT$r2QT.Seasonal)
mthnagdata_r2QT$r2QT.Seasonal <- mthnagdata_r2QT$r2QT.Seasonal *freq / sum
nag_m <- merge(nag_m,mthnagdata_r2QT,by ="month")
nag_m <- nag_m[order(nag_m$sno),]
nag_m$QT_r2T_fit <-nag_m$trend_quad * nag_m$r2QT.Seasonal #Fitted value

#Calculating error & predicted value
nag_m$QT_r2T_error <- nag_m$precipMM -  nag_m$QT_r2T_fit 
nag_v <- merge(nag_v,mthnagdata_r2QT,by="month")
nag_v <- nag_v[order(nag_v$sno),]
nag_v$QT_r2T_pred <-nag_v$trend_quad * nag_v$r2QT.Seasonal

# Errors for model period
Acc(nag_m$QT_r2T_fit,nag_m$precipMM)

# Errors for validation period
Acc(nag_v$QT_r2T_pred,nag_v$precipMM)

# White noise test
plot(nag_m$QT_r2T_error) 
Acf(nag_m$QT_r2T_error, na.action = na.pass)
Box.test(nag_m$QT_r2T_error, lag = 12, type =  "Ljung-Box")



## Seasonal estimation by average percentage method 
nag_m1 <- merge(nag_m,grpYnagdata,by="year")
nag_m1$apY <- nag_m1$precipMM/nag_m1$yprecipMM

apY <- aggregate(x=nag_m1$apY, by=list(nag_m1$month),  FUN="mean")

head(apY)
names(apY) <- c("month","ap.Seasonal")
nag_m1 <- merge(nag_m1,apY,by="month")
nag_m1 <- nag_m1[order(nag_m1$sno),]

nag_m1$QT_ap_fit <-nag_m1$trend_quad * nag_m1$ap.Seasonal
nag_m1$QT_ap_error <- nag_m1$precipMM -  nag_m1$QT_ap_fit

nag_v <- merge(nag_v,apY,by="month")
nag_v <- nag_v[order(nag_v$sno),]
nag_v$QT_ap_pred <-nag_v$trend_quad * nag_v$ap.Seasonal

# Errors for model period
Acc(nag_m1$QT_ap_pred,nag_m1$precipMM)

# Errors for validation period
Acc(nag_v$QT_ap_pred,nag_v$precipMM)

# White noise test
plot(nag_m1$QT_ap_error) 
Acf(nag_m1$QT_ap_error, na.action = na.pass)
Box.test(nag_m1$QT_ap_error, lag = 12, type =  "Ljung-Box")


## Seasonal estimation by ratio to moving average


nag_m$MA <- ma(nag_m$precipMM, order=12, centre=TRUE)
nag_m$r2MA <- nag_m$precipMM/nag_m$MA

mthnagdata_r2MA <-  aggregate(x=as.vector(nag_m$r2MA), by=list(nag_m$month),  FUN="mean", na.rm=TRUE)

names(mthnagdata_r2MA) <- c("month","r2MA.Seasonal")
sum <- sum(mthnagdata_r2MA$r2MA.Seasonal)
mthnagdata_r2MA$r2MA.Seasonal <- mthnagdata_r2MA$r2MA.Seasonal *freq / sum
nag_m <- merge(nag_m,mthnagdata_r2MA,by="month")
nag_m <- nag_m[order(nag_m$sno),]
nag_m$QT_r2MA_fit <-nag_m$trend_quad * nag_m$r2MA.Seasonal #Fitted value


#Calculating error & predicted value
nag_m$QT_r2MA_error <- nag_m$precipMM -  nag_m$QT_r2MA_fit 
nag_v <- merge(nag_v,mthnagdata_r2MA,by="month")
nag_v <- nag_v[order(nag_v$sno),]
nag_v$QT_r2MA_pred <-nag_v$trend_quad * nag_v$r2MA.Seasonal


# Errors for model period
Acc(nag_m$QT_r2MA_pred,nag_m$precipMM)

# Errors for validation period
Acc(nag_v$QT_r2MA_pred,nag_v$precipMM)

# White noise test
plot(nag_m$QT_r2MA_error) 
Acf(nag_m$QT_r2MA_error, na.action = na.pass)
Box.test(nag_m$QT_r2MA_error, lag = 12, type =  "Ljung-Box")




###  Seasonal estimation  -- difference from trend  method
nag_m$d2QT <- nag_m$precipMM - nag_m$trend_quad
mthnagdata_d2QT <- aggregate(x=nag_m$d2QT, by=list(nag_m$month),  FUN="mean")
names(mthnagdata_d2QT) <- c("month","d2QT.Seasonal")
sum <- sum(mthnagdata_d2QT$d2QT.Seasonal)
mthnagdata_d2QT$d2QT.Seasonal <- mthnagdata_d2QT$d2QT.Seasonal - (sum / freq)
nag_m <- merge(nag_m,mthnagdata_d2QT,by ="month")
nag_m <- nag_m[order(nag_m$sno),]
nag_m$QT_d2T_fit <-nag_m$trend_quad + nag_m$d2QT.Seasonal #Fitted value

#Calculating error & predicted value
nag_m$QT_d2T_error <- nag_m$precipMM -  nag_m$QT_d2T_fit 
nag_v <- merge(nag_v,mthnagdata_d2QT,by="month")
nag_v <- nag_v[order(nag_v$sno),]
nag_v$QT_d2T_pred <-nag_v$trend_quad + nag_v$d2QT.Seasonal

# Errors for model period
Acc(nag_m$QT_d2T_fit,nag_m$precipMM)

# Errors for validation period
Acc(nag_v$QT_d2T_pred,nag_v$precipMM)

# White noise test
plot(nag_m$QT_d2T_error) 
Acf(nag_m$QT_d2T_error, na.action = na.pass)
Box.test(nag_m$QT_d2T_error, lag = 12, type =  "Ljung-Box")





######################################

### Seasonalities using Linear trend

###  Seasonal estimation  -- ratio to trend method
nag_m$r2LT <- nag_m$precipMM/nag_m$trend_linear
mthnagdata_r2LT <- aggregate(x=nag_m$r2LT, by=list(nag_m$month),  FUN="mean")
names(mthnagdata_r2LT) <- c("month","r2LT.Seasonal")
sum <- sum(mthnagdata_r2LT$r2LT.Seasonal)
mthnagdata_r2LT$r2LT.Seasonal <- mthnagdata_r2LT$r2LT.Seasonal *freq / sum
nag_m <- merge(nag_m,mthnagdata_r2LT,by ="month")
nag_m <- nag_m[order(nag_m$sno),]
nag_m$LT_r2T_fit <-nag_m$trend_linear * nag_m$r2LT.Seasonal #Fitted value

#Calculating error & predicted value
nag_m$LT_r2T_error <- nag_m$precipMM -  nag_m$LT_r2T_fit 
nag_v <- merge(nag_v,mthnagdata_r2LT,by="month")
nag_v <- nag_v[order(nag_v$sno),]
nag_v$LT_r2T_pred <-nag_v$trend_linear * nag_v$r2LT.Seasonal

# Errors for model period
Acc(nag_m$LT_r2T_fit,nag_m$precipMM)

# Errors for validation period
Acc(nag_v$LT_r2T_pred,nag_v$precipMM)

# White noise test
plot(nag_m$LT_r2T_error) 
Acf(nag_m$LT_r2T_error, na.action = na.pass)
Box.test(nag_m$LT_r2T_error, lag = 12, type =  "Ljung-Box")



## Seasonal estimation by average percentage method 
nag_m1 <- merge(nag_m,grpYnagdata,by="year")
nag_m1$apY <- nag_m1$precipMM/nag_m1$yprecipMM

apY <- aggregate(x=nag_m1$apY, by=list(nag_m1$month),  FUN="mean")

head(apY)
names(apY) <- c("month","ap.Seasonal")
nag_m1 <- merge(nag_m1,apY,by="month")
nag_m1 <- nag_m1[order(nag_m1$sno),]

nag_m1$LT_ap_fit <-nag_m1$trend_linear * nag_m1$ap.Seasonal
nag_m1$LT_ap_error <- nag_m1$precipMM -  nag_m1$LT_ap_fit

nag_v <- merge(nag_v,apY,by="month")
nag_v <- nag_v[order(nag_v$sno),]
nag_v$LT_ap_pred <-nag_v$trend_linear * nag_v$ap.Seasonal

# Errors for model period
Acc(nag_m1$LT_ap_pred,nag_m1$precipMM)

# Errors for validation period
Acc(nag_v$LT_ap_pred,nag_v$precipMM)

# White noise test
plot(nag_m1$LT_ap_error) 
Acf(nag_m1$LT_ap_error, na.action = na.pass)
Box.test(nag_m1$LT_ap_error, lag = 12, type =  "Ljung-Box")


## Seasonal estimation by ratio to moving average


nag_m$MA <- ma(nag_m$precipMM, order=12, centre=TRUE)
nag_m$r2MA <- nag_m$precipMM/nag_m$MA

mthnagdata_r2MA <-  aggregate(x=as.vector(nag_m$r2MA), by=list(nag_m$month),  FUN="mean", na.rm=TRUE)

names(mthnagdata_r2MA) <- c("month","r2MA.Seasonal")
sum <- sum(mthnagdata_r2MA$r2MA.Seasonal)
mthnagdata_r2MA$r2MA.Seasonal <- mthnagdata_r2MA$r2MA.Seasonal *freq / sum
nag_m <- merge(nag_m,mthnagdata_r2MA,by="month")
nag_m <- nag_m[order(nag_m$sno),]
nag_m$LT_r2MA_fit <-nag_m$trend_linear * nag_m$r2MA.Seasonal #Fitted value


#Calculating error & predicted value
nag_m$LT_r2MA_error <- nag_m$precipMM -  nag_m$LT_r2MA_fit 
nag_v <- merge(nag_v,mthnagdata_r2MA,by="month")
nag_v <- nag_v[order(nag_v$sno),]
nag_v$LT_r2MA_pred <-nag_v$trend_linear * nag_v$r2MA.Seasonal


# Errors for model period
Acc(nag_m$LT_r2MA_pred,nag_m$precipMM)

# Errors for validation period
Acc(nag_v$LT_r2MA_pred,nag_v$precipMM)

# White noise test
plot(nag_m$LT_r2MA_error) 
Acf(nag_m$LT_r2MA_error, na.action = na.pass)
Box.test(nag_m$LT_r2MA_error, lag = 12, type =  "Ljung-Box")




###  Seasonal estimation  -- difference from trend  method
nag_m$d2LT <- nag_m$precipMM - nag_m$trend_linear
mthnagdata_d2LT <- aggregate(x=nag_m$d2LT, by=list(nag_m$month),  FUN="mean")
names(mthnagdata_d2LT) <- c("month","d2LT.Seasonal")
sum <- sum(mthnagdata_d2LT$d2LT.Seasonal)
mthnagdata_d2LT$d2LT.Seasonal <- mthnagdata_d2LT$d2LT.Seasonal - (sum / freq)
nag_m <- merge(nag_m,mthnagdata_d2LT,by ="month")
nag_m <- nag_m[order(nag_m$sno),]
nag_m$LT_d2T_fit <-nag_m$trend_linear + nag_m$d2LT.Seasonal #Fitted value

#Calculating error & predicted value
nag_m$LT_d2T_error <- nag_m$precipMM -  nag_m$LT_d2T_fit 
nag_v <- merge(nag_v,mthnagdata_d2LT,by="month")
nag_v <- nag_v[order(nag_v$sno),]
nag_v$LT_d2T_pred <-nag_v$trend_linear + nag_v$d2LT.Seasonal

# Errors for model period
Acc(nag_m$LT_d2T_fit,nag_m$precipMM)

# Errors for validation period
Acc(nag_v$LT_d2T_pred,nag_v$precipMM)

# White noise test
plot(nag_m$LT_d2T_error) 
Acf(nag_m$LT_d2T_error, na.action = na.pass)
Box.test(nag_m$LT_d2T_error, lag = 12, type =  "Ljung-Box")


#############################################################################

### Using Box Cox transformation

lam_nag <- BoxCox.lambda(nag_model)

# selecting lambda as 0.5 here (between 0 and auto-1.115)
y <- BoxCox(nag_model,lam_nag) 
nag_bc_m<-data.frame(precipMM=as.matrix(y))
nag_bc_m <- cbind(nag_bc_m, nag_m[c("year","month")])

y <- BoxCox(nag_val,0.5) 
nag_bc_v<-data.frame(precipMM=as.matrix(y))
nag_bc_v <- cbind(nag_bc_v, nag_v[c("year","month")])


## Using the linear trend with difference from trend seasonality


### Linear Trend on Box-Cox transformed Data

# Creating serial number
nag_bc_m$sno <- 1:n_model
nag_bc_v$sno <- (n_model+1):(n_model+n_val)

# Trend Estimation by LS regression on grouped data
freq<-12
grpYnagdata <- aggregate(x=nag_bc_m$precipMM, by=list(nag_bc_m$year),  FUN="mean")
grpYnagdata$t <- freq *(1:nrow(grpYnagdata)) - (freq/2 - 0.5)		# for even period = frequency. modify for odd
head(grpYnagdata)
names(grpYnagdata) <- c("year", "yprecipMM","t")

#Fitting linear model
model_2 <- lm(yprecipMM ~ t,  data=grpYnagdata)
nag_bc_m$trend_linear <- model_2$coefficients[1] + model_2$coefficients[2]*nag_bc_m$sno

#Predicting trend for holdout period
newdata = data.frame(t=(n_model+1):(n_model+n_val)) 
nag_bc_v$trend_linear <- predict(model_2, newdata) 

nag_bc_v$trend_linear
nag_bc_m$trend_linear







###  Seasonal estimation  -- difference from trend  method
nag_bc_m$d2LT <- nag_bc_m$precipMM - nag_bc_m$trend_linear
mthnagdata_d2LT <- aggregate(x=nag_bc_m$d2LT, by=list(nag_bc_m$month),  FUN="mean")
names(mthnagdata_d2LT) <- c("month","d2LT.Seasonal")
sum <- sum(mthnagdata_d2LT$d2LT.Seasonal)
mthnagdata_d2LT$d2LT.Seasonal <- mthnagdata_d2LT$d2LT.Seasonal - (sum / freq)
nag_bc_m <- merge(nag_bc_m,mthnagdata_d2LT,by ="month")
nag_bc_m <- nag_bc_m[order(nag_bc_m$sno),]
nag_bc_m$LT_d2T_fit <-nag_bc_m$trend_linear + nag_bc_m$d2LT.Seasonal #Fitted value

#Calculating error & predicted value
nag_bc_m$LT_d2T_error <- nag_bc_m$precipMM -  nag_bc_m$LT_d2T_fit 
nag_bc_v <- merge(nag_bc_v,mthnagdata_d2LT,by="month")
nag_bc_v <- nag_bc_v[order(nag_bc_v$sno),]
nag_bc_v$LT_d2T_pred <-nag_bc_v$trend_linear + nag_bc_v$d2LT.Seasonal

# Errors for model period
Acc(nag_bc_m$LT_d2T_fit,nag_bc_m$precipMM)

# Errors for validation period
Acc(nag_bc_v$LT_d2T_pred,nag_bc_v$precipMM)

# White noise test
plot(nag_bc_m$LT_d2T_error) 
Acf(nag_bc_m$LT_d2T_error, na.action = na.pass)
Box.test(nag_bc_m$LT_d2T_error, lag = 12, type =  "Ljung-Box")


