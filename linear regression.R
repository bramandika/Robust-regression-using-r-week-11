library(MASS)
library(stats)
data("whiteside")
linearModelA <- lm(Gas ~ Temp, data = whiteside)
names(linearModelA)
abline(linearModelA, lty = 2, lwd = 2)
linearModelA$coefficients
linearModelA$coefficients
summary(linearModelA)
#regression plot
coeff=linearModelA$coefficients
eq = paste0("y = ", round(coeff[1], 1), "*x ", 
            round(coeff[2], 1))
plot(whiteside$Temp,whiteside$Gas,main=eq)
abline(linearModelA, col = "darkgreen")

#residual plot
pred <- predict(linearModelA)
res <- resid(linearModelA)
sres <-rstandard(linearModelA)

plot(linearModelA$fitted.values,linearModelA$residuals)
abline(a=0,b=0)
# standardized residual plot
plot(linearModelA$fitted.values,sres)
abline(a=0,b=0)
#actual y vs predicted
plot(linearModelA$fitted.values,whiteside$Gas)
reg_std <- lm(whiteside$Gas~linearModelA$fitted.values)
abline(reg_std)

#check the distributions
shapiro.test(linearModelA$residuals)
qqnorm(linearModelA$residuals)
qqline(linearModelA$residuals,col='red')

#checking the heterocedasity
plot(linearModelA$fitted.values,linearModelA$residuals)
(mean(abs(c(linearModelA$residuals)))/whiteside$Gas)*100

glej=lm(abs(linearModelA$residuals)~linearModelA$fitted.values)
summary(glej)

library(Metrics)
library(car)

durbinWatsonTest(linearModelA)
acf(linearModelA$residuals)

#cek nilai rmse
library(Metrics)
rmse(whiteside$Gas,linearModelA$fitted.values)
rlm
