##EXERCISE 3.12 ISLR

#If the abs values of x and y are the same and there is no B0 coef

#Where coefficients are different

x=rnorm(100)
y=5+3*x+rnorm(100)

fit1<-lm(x~y)
fit2<-lm(y~x)

coef(fit1)
coef(fit2)

#where coefficients are the same

x=rnorm(100)
y=x

fit1<-lm(x~y)
fit2<-lm(y~x)

coef(fit1)
coef(fit2)
