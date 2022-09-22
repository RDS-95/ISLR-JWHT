##ISLR LAB 4.6

library(ISLR)
names(Smarket)
dim(Smarket)

summary(Smarket)

pairs(Smarket)

cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

##LOGISTIC REGRESSION

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)

summary(glm.fit)

coef(glm.fit)

summary(glm.fit)$coef[,4]

glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]

contrasts(Direction)

glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"

mean(glm.pred==Direction)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)

Direction.2005=Direction[!train]

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial,subset=train)

glm.probs=predict(glm.fit,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"

table(glm.pred,Direction.2005)

mean(glm.pred==Direction.2005)

glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)

mean(glm.pred==Direction.2005)

##LINEAR DISCRIMINANT ANALYSIS

library(MASS)

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit

lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)

lda.class=lda.pred$class
table(lda.class,Direction.2005)

mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

sum(lda.pred$posterior[,1]>.9)

##QUADRATIC DISCRIMINANT ANALYSIS

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)

mean(qda.class==Direction.2005)

##K-NEAREST NEIGHBORS

library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)

knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)

mean(knn.pred==Direction.2005)

##CARAVAN DATA APPLICATION

dim(Caravan)
attach(Caravan)
summary(Purchase)

standardized.X=scale(Caravan[,-86])

var(Caravan[,2])
var(standardized.X[,2])

test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)

mean(test.Y!=knn.pred)
mean(test.Y!="No")

table(knn.pred,test.Y)

knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)

glm.fit=glm(Purchase~.,data=Caravan,family=binomial,
            subset=-test)

glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)

glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
