library("TTR")
library("forecast")
setwd('C:/Users/Sajmal/Documents/Studies/Term3/TSFT')
percip <- read.csv("perc_nagpur.csv", header=TRUE)
percts <- ts(percip[,3],start =c(2009,1), frequency=12)
plot(decompose(percts))

Acc <- function(Yhat,Y) {
  error <- Y - Yhat
  err <- error[!is.na(error)]	#remove NA
  Y <- Y[!is.na(error)]
  MSE <- sqrt(round(mean(err^2), 2))
  MAE <- round(mean(abs(err)), 2)
  MAPE <- round(mean(abs(err/Y)) * 100, 3)
  ErrVec <- cbind(MSE, MAE, paste(toString(MAPE),"%"))
  colnames(ErrVec) <- c("RMSE","MAE","MAPE")
  return(ErrVec)
}

# Simple Exponential Smoothing 
ts1 <- window(percts, end=c(2017,12))
ts2 <- window(percts, start=c(2018, 01))
ts1.decomposed <- decompose(ts1, type="additive")
ts2.new <- ts2 - ts2.decomposed$trend - ts2.decomposed$seasonal
ts2.decomposed <- decompose(ts2, type="additive")
ts2.new <- ts2 - ts2.decomposed$trend - ts2.decomposed$seasonal
plot(ts1.new)
fc <- ses(ts1.new,h=25,fan=FALSE)
class(fitted(fc))
Acc(fitted(fc), ts1 )
res <- ts1 - fitted(fc)
plot(res) 
acf(res)
Box.test(res, lag = 13, type = "Ljung-Box")
predict(fc,25)
result.ses <- Acc(predict(fc,25)$mean, ts2.new)


#Holt's method on deseasonalized data

ts1 <- window(percts, end=c(2017,12))
ts2 <- window(percts, start=c(2018, 01))

ts1 <- decompose(ts1)
ts1.deseason <- seasadj(ts1)
fc <- holt(ts1.deseason,h=25)

matrix_fc <- as.matrix(fitted(fc))
Acc(matrix_fc, as.matrix(ts1.deseason))
res <- ts1.deseason - matrix_fc
plot(res) 
acf(res) 
Box.test(res, lag = 13, type = "Ljung-Box")
x <- (predict(fc,25))

ts2 <- decompose(ts2)
ts2.deseason <- seasadj(ts2)
predict(fc,25)
result.holt<- Acc((predict(fc,25))$mean, ts2.deseason)


#Holt Winters on Original data
ts1 <- window(percts, end=c(2017,12))
ts2 <- window(percts, start=c(2018, 01))

fc <- hw(ts1,seasonal = "additive" )

Acc(as.matrix(fitted(fc)), ts1 )
res <- ts1 - as.matrix(fitted(fc))
plot(res) 
acf(res) 
Box.test(res, lag = 13, type = "Ljung-Box")
x <- (predict(fc,25))
predict(fc,25)
result.holtwint <- Acc((predict(fc,25))$mean, ts2)


pred_results<- rbind(result.ses,result.holt,result.holtwint)
pred_results
