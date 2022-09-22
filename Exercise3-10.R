##ISLR EXERCISE 3.10

LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("Libraries Loaded")
}

LoadLibraries()

fix(Carseats)
attach(Carseats)

lm.fit=lm(Sales~Price+Urban+US,data=Carseats)
summary(lm.fit)

##Can reject the null hypothesis for the Price + US variables.

lm.fit2=lm(Sales~Price+US,data=Carseats)
summary(lm.fit2)

anova(lm.fit,lm.fit2)
##models seem to perform similarly as predictors. RSS of ~2420 for both.
##This indicated that the average prediction by our linear fit will be off by
##2420 ? ??

par(mfrow=c(2,2))
plot(lm.fit2)

##There is one particularly high leverage point

identify(Sales,Price,US)

#For outliers, I don't see any particularly egregious cases. There may be a handful of values which could be taken from the plot, depending on the threshold we set.


