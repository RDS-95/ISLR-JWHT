##EXERCISE 3-13

set.seed(1)
x<-rnorm(100)

eps<-rnorm(100,sd=(0.25^2))

y<- (-1)+(0.5*x)+eps

#y is a vector length 100, B0=-1, B1=0.5

plot(x,y)

#There is a strong, positive, linear relationship between x and y.

lm.fit=lm(y~x)
summary(lm.fit)

#estimate for intercept(-1.002356) and slope(0.499934) are v close to the true value

abline(lm.fit,lwd=4)
abline(a=-1,b=0.5,col="red")

legend(x="topleft",
       legend = c("model fit","true fit"),
       lty=c(1,1),
       col=c(1,"red"))

lm.fitp=lm(y~x+I(x^2))
summary(lm.fitp)

##The quadratic term doesn't improve the fit. In fact, we can not reject the null hypothesis for this term due to the high p value.

##Confidence intervals in the noisy data set will be greater. with less noise, the size of the interval will decrease.
