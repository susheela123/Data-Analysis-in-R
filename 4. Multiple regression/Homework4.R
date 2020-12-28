getwd()
setwd('C:/Users/Susheela/Documents/data analysis/assignment 4')
dataset<-read.csv('PGA.csv',header=TRUE)
names(dataset)
pairs(dataset,pch=20)  #scatter plot
par(mfrow=c(2,5))
hist(as.numeric(dataset$Age),xlab="Age",main="Histogram of Age parameter")                         
hist(as.numeric(dataset$AverageDrive),xlab="AverageDrive",main="Histogram of AverageDrive parameter")                       
hist(as.numeric(dataset$DrivingAccuracy),xlab="DrivingAccuracy",main="Histogram of DrivingAccuracy parameter")           
hist(as.numeric(dataset$GreensonRegulation),xlab="GreensonRegulation",main="Histogram of GreensonRegulation parameter")       
hist(as.numeric(dataset$AverageNumofPutts),xlab="AverageNumofPutts",main="Histogram of AverageNumofPutts parameter")                  
hist(as.numeric(dataset$SavePercent),xlab="SavePercent",main="Histogram of SavePercent parameter")                   
hist(as.numeric(dataset$MoneyRank),xlab="MoneyRank",main="Histogram of MoneyRank parameter")                        
hist(as.numeric(dataset$Numevents),xlab="Numevents",main="Histogram of NUmevents parameter")                           
hist(as.numeric(dataset$TotalWinnings),xlab="TotalWinnings",main="Histogram of TotalWinning Parameter")  
hist(as.numeric(dataset$Response),xlab="Response",main="Histogram of Response Parameter")  
names(dataset)[1]="Name"
names(dataset)[2]="Age"
names(dataset)[3]="AverageDrive"
names(dataset)[4]="DrivingAccuracy"
names(dataset)[5]="GreensonRegulation"
names(dataset)[6]="AverageNumofPutts"
names(dataset)[7]="SavePercent"
names(dataset)[8]="MoneyRank"
names(dataset)[9]="Numevents"
names(dataset)[10]="TotalWinnings"
names(dataset)[11]="Response"
attach(dataset)
model1<-lm(Response~ Age+AverageDrive+DrivingAccuracy+GreensonRegulation+AverageNumofPutts+SavePercent+Numevents,data=dataset)
summary(model1)$coef[,3]   # t values
summary(model1)$coef[,4]   # p values
f = summary(model1)$fstatistic 
fstat =f[1]
print(fstat)
model2<-lm(Response~ Age+AverageDrive,data=dataset)
anova(model1,model2)
model3<-lm(Response ~ Age + AverageDrive + SavePercent,data=dataset)
anova(model1,model3)
confint(model1,level=0.95)
predict(model1,level=.95,list(Age = 35,
                                      AverageDrive = 287,
                                      DrivingAccuracy = 64,
                                      GreensonRegulation = 64.9,
                                      AverageNumofPutts = 1.778,
                                      SavePercent = 48,
                                      Numevents = 26),interval="confidence")
predict(model1,level=.95,list(Age = 42,
                              AverageDrive = 295,
                              DrivingAccuracy = 69,
                              GreensonRegulation = 67.7,
                              AverageNumofPutts = 1.80,
                              SavePercent = 54,
                              Numevents = 30),interval="confidence")
model1
dataset_unit_normal=as.data.frame(apply(dataset[2:11],2,function(x){(x-mean(x))/sd(x)}))

# redo regression
model1_unit_normal <- lm(Response~ Age+AverageDrive+DrivingAccuracy+GreensonRegulation+AverageNumofPutts+SavePercent+Numevents,data=dataset)
# check the coefficient
model1_unit_normal
