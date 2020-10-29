getwd()
setwd("C:/Users/Susheela/Documents/data analysis/assignment 3")
getwd()
df <- read.csv("tombstone.csv", header=TRUE)
names(df)[3]="Response"
names(df)[2]="Covariate"
names(df)
attach(df)
Response
Covariate

plot(Covariate,Response,pch=20)
model2 <- lm(Response~Covariate,data=df)
summary(model2)
plot(Response~Covariate)
abline(model2,lwd=3)
summary(model2)$r.square  # r square value
summary(model2) #summary
summary(model2)$coef[,1] # coefficient estimate
summary(model2)$coef[,2] # standard error


summary(model2)$coef[,3] # t value
summary(model2)$coef[,4] # p value

summary(model2)
summary(lm(Response~Covariate, offset=0.02*Covariate))

confint(model2,level=0.95)
confint(model2,level=0.90)
confint(model2,level=0.99)

predict.lm(model2, interval="confidence") 

newx<-seq(0,500)
conf<-predict(model2,newdata=data.frame(Covariate=newx),interval = c("confidence"),level = 0.90,type="response")
plot(Covariate,Response,pch=20)
model2 <- lm(Response ~ Covariate, data=df)
abline(model2,col="blue")
lines(newx,conf[,3],col="red",lty=2)
lines(newx,conf[,2],col="red",lty=2)


