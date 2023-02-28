library(foreign)
library(MASS)

setwd("~/asdos aed/minggu ke 9")
df<-read.table(file="mineral data.csv", sep=";", header = T)
df

plot(df$Zn~df$Cu)
olslm<-lm(df$Zn~df$Cu)
summary(olslm)
abline(olslm, col='red')

library(olsrr)
ols_plot_resid_lev(olslm)

d1 <- cooks.distance(olslm)
r <- stdres(olslm)
a <- cbind(df$Zn, d1, r)
a[d1 > 4/53, ]
4/53
#huber with M estimator
rr.huber <- rlm(df$Zn~df$Cu, method = "M", psi = psi.huber )
summary(rr.huber)
plot(df$Zn~df$Cu)
abline(rr.huber)

abline(olslm, col='red')

residual_hu=rr.huber$residuals
sse_hu=sum(residual_hu^2)
sst=sum((df$Zn-mean(df$Zn))^2)
R2_hu=1-(sse_hu/sst)

#huber with MM estimator
rr.huber <- rlm(df$Zn~df$Cu, method = "MM", psi = psi.huber )
summary(rr.huber)
abline(rr.huber,col='green')


residual_hu=rr.huber$residuals
sse_hu=sum(residual_hu^2)
sst=sum((df$Zn-mean(df$Zn))^2)
R2_hu1=1-(sse_hu/sst)

#bi square with m  estimator
rr.bisquare <- rlm(df$Zn~df$Cu, method = "M", psi = psi.bisquare)
summary(rr.bisquare)
abline(rr.bisquare, col='blue')

residual_bis=rr.bisquare$residuals
sse_bis=sum(residual_bis^2)
sst=sum((df$Zn-mean(df$Zn))^2)
R2_bis=1-(sse_bis/sst)

#bi square with MM  estimator
rr.bisquare <- rlm(df$Zn~df$Cu, method = "MM", psi = psi.bisquare)
summary(rr.bisquare)
abline(rr.bisquare, col='blue')

residual_bis=rr.bisquare$residuals
sse_bis=sum(residual_bis^2)
sst=sum((df$Zn-mean(df$Zn))^2)
R2_bis1=1-(sse_bis/sst)
