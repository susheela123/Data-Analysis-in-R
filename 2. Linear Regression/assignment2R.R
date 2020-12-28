getwd()
setwd('C:/Users/Susheela/Documents/data analysis/assignment 2')
getwd()
df <- read.csv("bus.csv",header=TRUE)
df
names(df)
names(df)[1]="Response"
names(df)[2]="Covariate"
names(df)
attach(df)
Response
Covariate

#question2
plot(Covariate,Response,pch=20)

#question 3
model <- lm(Response~Covariate,data=df)
plot(Response~Covariate)
abline(model,lwd=3)

model$coefficients

model$fitted.values
sum(model$fitted.values)
Response
sum(Response)


model$residuals
sum(model$residuals)

summary(model)

#question 4
updatedCovariate=Covariate+1
model1 <- lm(Response~updatedCovariate,data=df)


plot(Response~updatedCovariate)

abline(model1,col="black")

summary(model)
summary(model1)

model$residuals
model1$residuals

#question 8

mean(Response)
mean(Covariate)
points(mean(Covariate),mean(Response),pch=20,col="blue",cex=3)
abline(model,col="blue",lwd=3)

summary(Response)
model$coefficients
model$coefficients[1]

summary(Response)

df[which(df$Response==max(Response)),]

residual=abs(model$residuals)
df <- cbind(df,residual)
df[which(df$residual==max(df$residual)),]

