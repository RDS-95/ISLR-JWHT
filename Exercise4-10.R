##EXERCISE 4-10 ISLR

library(ISLR)
library(MASS)

summary(Weekly)

pairs(Weekly)

attach(Weekly)

plot(Lag2,Lag4) #tried a few different plots here

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Weekly,
            family=binomial)

summary(glm.fit)

#Lag2 has the highest probability of being significant

glm.probs=predict(glm.fit,type="response")

glm.pred=rep("Down",484+605)
glm.pred[glm.probs>.5]="Up"

table(glm.pred,Direction)

#The logreg is predicting "up" far more often than "down"
#Results show a lot of "Down" directions misclassified as "Up"

#training using logreg

train=(Year<2008)
Weekly.0809=Weekly[!train,]
Direction.0809=Direction[!train]

glm.fit2=glm(Direction~Lag2,
            data=Weekly,
            family=binomial,
            subset=train)

glm.probs2=predict(glm.fit2,Weekly.0809,type="response")

glm.pred2=rep("Down",72+84)
glm.pred2[glm.probs2>.5]="Up"

table(glm.pred2,Direction.0809)

(7+79)/(7+79+5+65)

#55% correct for logistic regression

#attempting with LDA

lda.fit=lda(Direction~Lag2,data=Weekly,subset=train)

lda.pred=predict(lda.fit,Weekly.0809)
lda.class=lda.pred$class

table(lda.class,Direction.0809)

(6+79)/(7+79+5+66)

#54% Correct with LDA

#Attempting with QDA

qda.fit=qda(Direction~Lag2,data=Weekly,subset=train)

qda.class=predict(qda.fit,Weekly.0809)$class

table(qda.class,Direction.0809)

84/(84+72)

#53.8% with QDA, but this isn't a very useful predictor. all values predicted Yes

#KNN with K=1

library(class)

train.X=cbind(Lag2)[train,]
test.X=cbind(Lag2)[!train,]
train.Direction=Direction[train]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.0809)



