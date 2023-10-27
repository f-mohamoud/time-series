
library(astsa)
# SECTION 1

# question 1
plot(flu,ylab="", main="Flu" ,ylim=c(-2,1))

# question 2
plot(sqrt(flu),ylab="", main="Square Root",ylim=c(-2,1))
summary(sqrt(flu))
plot(log(flu), ylab="", main="Log",ylim=c(-2,1))
summary(log(flu))
sflu=sqrt(flu)

# question 3
diff_flu=diff(sflu)

# question 4
plot(diff_flu,ylab="",main="First-Order Differencing")
abline(h=0,lty=2)

# question 5
diff6flu=diff(diff_flu, lag = 12)

# question 6
plot(diff6flu,ylab="", main="Lag-12 Seasonal Differencing")
abline(h=0)




# SECTION 2

# question 1a
hwes=HoltWinters(sflu)
plot(hwes,ylab = "",main = "Holt-Winters Exponential Smoothing",ylim = c(0,1),xlim=c(1968,1979))

# question 1b
hwest=HoltWinters(sflu, gamma = FALSE) #detrend
hwes2=sflu-hwest$fitted[,'xhat']#deseason
plot(hwes2,ylab="",main="Holt-Winters Exponential Smoothing", ylim=c(-0.4,0.3))
abline(h=0,lty=2)

# question 2a
bav=decompose(sflu, type = "additive")
plot(bav)
plot(sflu, ylab="", main="Brockwell and Davis Algorithm",ylim = c(0,1),xlim=c(1968,1979))
lines(bav$trend, col="red",)

# question 2b
bav2= sflu - brockanddav$trend-brockanddav$seasonal
plot(detrenddeseasonalisebndav,ylab="",main="Brockwell and Davis Algorithm", ylim=c(-0.4,0.3))
abline(h=0,lty=2)

# question 3a
library(forecast)
sir=tslm(sflu~trend(sflu)+seasonaldummy(sflu))
plot(sflu,ylab="", main="Seasonal Indicator Regression",ylim = c(0,1),xlim=c(1968,1979))
lines(fitted(sir),col="red")

# question 3b
sir2=residuals(sir)
plot(sir2,ylab="",main="Seasonal Indicator Regression", ylim=c(-0.4,0.3))
abline(h=0,lty=2)

