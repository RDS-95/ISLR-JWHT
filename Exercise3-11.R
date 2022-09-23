##EXERCISE 3.11 ISLR

set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)

lm.fit<-lm(y~x+0)
summary(lm.fit)

#B1: 1.9939
#standard error: 0.1065
#t-stat: 18.73
#p-value: <2e-16

#There is a positive linear relationship indicated by the intercept.
#We can expect an average error of 0.1065 for each prediction of x
#degrees of freedom is 99 (100 instances - 1 estimator)
#from a t table, a t-stat above ~1.66 gives the 5% confidence interval
#a t-stat of 18.73 is far in excess of this value => reject null hypothesis
#the p value shows us that we can definitely reject the null hypothesis

lm.fit1<-lm(y~x)
summary(lm.fit1)



