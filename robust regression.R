library(foreign)
library(MASS)

setwd("~/asdos aed/minggu ke 9")
mydata<-read.table(file="no outlier robust regression.csv", sep=";", header = T)
plot(mydata$Production~mydata$Area)
olslm<-lm(mydata$Production~mydata$Area)
summary(olslm)
abline(olslm, col='red')

mydata2<-read.table(file="outlier robust regression.csv", sep=";", header = T)
plot(mydata2$Production~mydata2$Area)
olslm2<-lm(mydata2$Production~mydata2$Area)
summary(olslm2)
abline(olslm2, col='red')

library(olsrr)
ols_plot_resid_lev(olslm2)

d1 <- cooks.distance(olslm2)
r <- stdres(olslm2)
a <- cbind(mydata2$Production, d1, r)
a[d1 > 4/15, ]#15 is number of observations

rr.huber <- rlm(mydata2$Production~mydata2$Area, method = "MM", psi = psi.huber )
summary(rr.huber)
plot(mydata2$Production~mydata2$Area)
abline(rr.huber)
abline(olslm2, col='red')

residual_hu=rr.huber$residuals
sse_hu=sum(residual_hu^2)
sst=sum((mydata2$Production-mean(mydata2$Production))^2)
R2_hu=1-(sse_hu/sst)


rr.bisquare <- rlm(mydata2$Production~mydata2$Area, method = "M", psi = psi.bisquare)
summary(rr.bisquare)
abline(rr.bisquare, col='blue')

residual_bis=rr.bisquare$residuals
sse_bis=sum(residual_bis^2)
sst=sum((mydata2$Production-mean(mydata2$Production))^2)
R2_bis=1-(sse_bis/sst)

library(foreign)
cdata <- read.dta("https://stats.idre.ucla.edu/stat/data/crime.dta")
summary(cdata)
ols <- lm(crime ~ poverty + single, data = cdata)
summary(ols)
shapiro.test(ols$residuals)

plot(ols, las = 1)
library(olsrr)
ols_plot_resid_lev(ols)

d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(cdata, d1, r)
a[d1 > 4/51, ]

rabs <- abs(r)
a <- cbind(cdata, d1, r, rabs)
asorted <- a[order(-rabs), ]
asorted[1:10, ]

rr.huber <- rlm(crime ~ poverty + single, data = cdata, method = "MM", psi = psi.huber )
summary(rr.huber)
plot(rr.huber)


rr.bisquare <- rlm(crime ~ poverty + single, data=cdata, method = "MM", psi = psi.bisquare)
summary(rr.bisquare)
plot(rr.bisquare)



