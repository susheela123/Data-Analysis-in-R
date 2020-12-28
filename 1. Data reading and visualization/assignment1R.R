setwd('C:/Users/Susheela/Desktop/GITHUB/Data-Analysis/assignment 1') # set the path to file location 
getwd()

df<-read.csv("car_r.csv",header=TRUE)                     # reads the file into dataframe 'df' 

?read.table  # ? is used for functions usage instructions

names(df)    # variables in the dataframe

count=0     # Number of observations with brand="ford"
for(i in 1:500){
  if(df[i,1]=='Ford'){
    count=count+1
  }
}
print('number of observations')
print(nrow(df))                #Total number of observations
print('Ford count:')
print(count)                  #number of observations with brand="Ford"
#histogram


colMeans(df[2:8],na.rm=TRUE)
print("Standard deviations:")
i=0
for(i in 2:8){
  print(colnames(df[i]))
  
  print(c(sd(as.numeric(df[[i]]),na.rm = TRUE)))
}


#histogram
hist(as.numeric(df$brand),xlab="Brand",main="Histogram of Brand parameter")                         
hist(as.numeric(df$mileage),xlab="mileage",xlim=c(0,70000),main="Histogram of mileage parameter")                       
hist(as.numeric(df$num_accidents),xlab="num_accidents",main="Histogram of Num_accidents parameter")           
hist(as.numeric(df$num_passengers),xlab="num_passengers",xlim=c(0,20),main="Histogram of Num_passengers parameter")       
hist(as.numeric(df$speed_car),xlab="speed_car",main="Histogram of Speed_car parameter")                  
hist(as.numeric(df$speed_air),xlab="speed_air",xlim=c(-14,15),main="Histogram of speed_air parameter")                   
hist(as.numeric(df$height),xlab="height",main="Histogram of height parameter")                        
hist(as.numeric(df$width),xlab="width",main="Histogram of width parameter")                           
hist(as.numeric(df$ABS),xlab="ABS",main="Histogram of ABS Parameter")  

#Proportion of missing values in Mileage variable
nacount=0
i=0
for (i in 1:nrow(df)){
  if(is.na(as.numeric(df[i,2]))){
    nacount<-nacount+1
  }
}
nacount
print("Proportion of missing values")
print(nacount/nrow(df))

#relative speed
i=0
relativespeed<-list()
for(i in 1:nrow(df)){
  relativespeed[i]<-df[i,5]+df[i,6]
}
print("Average relative speed:")
mean(as.numeric(relativespeed))

#absolute value
print("Mean absolute value of speed_air")
mean(abs(df$speed_air))

#question 8

countmileage=0

for(i in 1:nrow(df)){
  if(df[i,2]<40000 | is.na(df[i,2])){
    countmileage=countmileage+1
  }
    
}
print("Number of records with Mileage less than 40000 is")
countmileage
i=0
countheight=0
for (i in 1:nrow(df)){
  if(df[i,7]<5){
    countheight=countheight+1
  }
}
print("Number of records with height less than 5 is")
countheight
updatedDF = df[!df$mileage< 40000 & !df$height<5 & !is.na(df$mileage)==TRUE,]
updatedDF

updatedDF <- subset(df, df$mileage>40000  & df$height>5 & is.na(df$mileage)==FALSE) #new dataset with the conditions in the question
updatedDF

#question 9
df_Ford<-subset(updatedDF,updatedDF$brand=="Ford")
df_GM<-subset(updatedDF,updatedDF$brand=="GM")
df_Toyota<-subset(updatedDF,updatedDF$brand=="Toyota")

#question 10
#speed
mean(df_Ford$speed_car)
mean(df_GM$speed_car)
mean(df_Toyota$speed_car)
sd(as.numeric(df_Ford$speed_car),na.rm = TRUE)
sd(as.numeric(df_GM$speed_car),na.rm = TRUE)
sd(as.numeric(df_Toyota$speed_car),na.rm = TRUE)

dim(df_Ford)
dim(df_GM)
dim(df_Toyota)

#height
mean(df_Ford$height)
mean(df_GM$height)
mean(df_Toyota$height)

sd(as.numeric(df_Ford$height),na.rm = TRUE)
sd(as.numeric(df_GM$height),na.rm = TRUE)
sd(as.numeric(df_Toyota$height),na.rm = TRUE)

#width
mean(df_Ford$width)
mean(df_GM$width)
mean(df_Toyota$width)

sd(as.numeric(df_Ford$width),na.rm = TRUE)
sd(as.numeric(df_GM$width),na.rm = TRUE)
sd(as.numeric(df_Toyota$width),na.rm = TRUE)

#number of accidents
mean(df_Ford$num_accidents)
mean(df_GM$num_accidents)
mean(df_Toyota$num_accidents)

#number of passengers
mean(df_Ford$num_passengers)
mean(df_GM$num_passengers)
mean(df_Toyota$num_passengers)
