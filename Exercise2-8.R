##CH2 Exercises

college<-ISLR::College

fix(college)

college =college[,-1]

summary(college)

pairs(college[,1:10],college)

attach(college)

Private = as.factor(Private)

plot(college$Outstate,college$Private)

boxplot(Outstate , Private , col ="red", varwidth =T, xlab="Outstate",
     ylab="Private")

Elite=rep("No",nrow(college ))
Elite[college$Top10perc >50]=" Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)

summary(Elite)

boxplot(Outstate,Elite)

par(mfrow=c(2,2))
hist(Apps)
hist(Accept)
hist(Enroll)
hist(Outstate)