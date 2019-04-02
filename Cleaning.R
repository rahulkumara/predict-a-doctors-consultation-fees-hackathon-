Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201')
library(rJava)

setwd("D://Data Analytics/Project 1")

library(readxl)

data = read_excel("Final_Train.xlsx")


str(data)

table(is.na(data))
check_na <- data[rowSums(is.na(data)) > 0,]
check_na

targetdata <- data[,c(1,2,4,5,7)]
targetdata

table(is.na(targetdata))
check_na <- targetdata[rowSums(is.na(targetdata)) > 0,]
check_na

targetdata<- targetdata[complete.cases(targetdata$Place),]

table(is.na(targetdata))
check_na <- targetdata[rowSums(is.na(targetdata)) > 0,]
check_na

#Playing with degrees



#find number of degree of a doctor

len <- nrow(targetdata)
len

i=1
fdata=0
for(i in 1:len)
{
  demo=targetdata[[1]][i]
  fdata[i] <- paste(demo, ",", sep="")
  fdata
  i=i+1
}

library(stringr)
j=1
ndeg=0
for(j in 1:len)
{
  demo1=fdata[[j]][1]
  ndeg[j] <- str_count(demo1,",")
  ndeg
  j=j+1
}

#find number of years of experience

#final_train$Experience<-sapply(strsplit(final_train$Experience," "),function(x) x[1])
#final_train$Place<-sapply(strsplit(final_train$Place,","),function(x) x[2])

exp = targetdata[[2]]

j=1
nyears=0
for(j in 1:len)
{
  demo2=exp[j]
  regexp <- "[[:digit:]]+"
  years <- str_extract(demo2, regexp)
  nyears[j] <- years
  j=j+1
}

#Split the place

placecol = targetdata[[3]]
j=1
city=0
for(j in 1:len)
{
  demo3=placecol[j]
  place = strsplit(demo3,",")
  city[j] = place[[1]][2]
  city
  j=j+1
}

#Assign value to place
#final_train$Place<-factor(final_train$Place,labels=1:8)

table(is.na(city))
check_na <- city[rowSums(is.na(targetdata)) > 0,]
check_na
city <- as.character(city)
city[is.na(city)] <- "unknown"


i=1
cvalue=0
for(i in 1:len)
{

  if(city[i] == " Bangalore")
  {
    cvalue[i] = 1
  }
  else if(city[i] == " Chennai")
  {
    cvalue[i] = 2
  }
  else if(city[i] == " Coimbatore")
  {
    cvalue[i] = 3
  }
  else if(city[i] == " Delhi")
  {
    cvalue[i] = 4
  }
  else if(city[i] == " Ernakulam")
  {
    cvalue[i] = 5
  }
  else if(city[i] == " Hyderabad")
  {
    cvalue[i] = 6
  }
  else if(city[i] == " Mumbai")
  {
    cvalue[i] = 7
  }
  else if(city[i] == " Thiruvananthapuram")
  {
    cvalue[i] = 8
  }
  else
  {
    cvalue[i] = 0
  }
  i=i+1
}

#Assigning values to profile
#final_train$Place<-factor(final_train$Place,labels=1:8)

profile = targetdata[[4]]

i=1
pvalue=0
for(i in 1:len)
{
  if(profile[i] == "Ayurveda")
  {
    pvalue[i] = 1
  }
  else if(profile[i] == "Dentist")
  {
    pvalue[i] = 2
  }
  else if(profile[i] == "Dermatologists")
  {
    pvalue[i] = 3
  }
  else if(profile[i] == "ENT Specialist")
  {
    pvalue[i] = 4
  }
  else if(profile[i] == "General Medicine")
  {
    pvalue[i] = 5
  }
  else if(profile[i] == "Homeopath")
  {
    pvalue[i] = 6
  }

  i=i+1
}

#adding new data to data set

nyears<- as.numeric(nyears)

targetdata$Qual <- ndeg
targetdata$nyear <- nyears
targetdata$nplace <- cvalue
targetdata$nprof <- pvalue
targetdata$city <- city

library(dummies)
targetdata<-cbind(targetdata,dummy(targetdata$Profile,sep="_"))
targetdata<-cbind(targetdata,dummy(targetdata$city,sep="_"))

# To find value of Highest Degree

data<-read.csv("valuecol.csv")
hqual = data
factor(hqual,levels = c(0,1,2,3,4,5))
targetdata$hqual=hqual$x

# END OF VALUE OF HIGHEST DEGREE

ntargetdata <- targetdata[,c(6,27,7,11:16,17:26,5)]

names(ntargetdata)[7]<-paste("targetdata_ENT_Specialist")
names(ntargetdata)[8]<-paste("targetdata_General_Medicine")
names(ntargetdata)[10]<-paste("targetdata_Bangalore")
names(ntargetdata)[11]<-paste("targetdata_Chennai")
names(ntargetdata)[12]<-paste("targetdata_Coimbatore")
names(ntargetdata)[13]<-paste("targetdata_Delhi")
names(ntargetdata)[14]<-paste("targetdata_Ernakulam")
names(ntargetdata)[15]<-paste("targetdata_Hyderabad")
names(ntargetdata)[16]<-paste("targetdata_Mumbai")
names(ntargetdata)[17]<-paste("targetdata_Sector_5")
names(ntargetdata)[18]<-paste("targetdata_Thiruvananthapuram")

# univariate analysis 1

library(moments)
library(diptest)

hist(ntargetdata$Qual,col="blue")
plot(density(ntargetdata$Qual),col="blue")
skewness(ntargetdata$Qual)

hist(ntargetdata$nyear,col="blue")
plot(density(ntargetdata$nyear),col="blue")
skewness(ntargetdata$nyear)

hist(ntargetdata$nplace,col="blue")
plot(density(ntargetdata$nplace),col="blue")
skewness(ntargetdata$nplace)

hist(ntargetdata$nprof,col="blue")
plot(density(ntargetdata$nprof),col="blue")
skewness(ntargetdata$nprof)

hist(ntargetdata$hqual,col="blue")
plot(density(ntargetdata$hqual),col="blue")
skewness(ntargetdata$hqual)

#bivariae analysis

plot(ntargetdata$Qual,ntargetdata$Fees,col=c("green","red"),
     main="Qual Vs. Fees",font=10)

plot(ntargetdata$nyear,ntargetdata$Fees,col=c("green","red"),
     main="Years of Experience Vs. Fees",font=10)

plot(ntargetdata$nplace,ntargetdata$Fees,col=c("green","red"),
     main="Locality Vs. Fees",font=10)

plot(ntargetdata$nprof,ntargetdata$Fees,col=c("green","red"),
     main="Profile Vs. Fees",font=10)

plot(ntargetdata$hqual,ntargetdata$Fees,col=c("green","red"),
     main="Hqual Vs. Fees",font=10)


#bivirate analysis - cor

cor(ntargetdata)
correlation = data.frame(cor(ntargetdata))

# CREATE TRAIN & TEST DATA
a <- sample(nrow(ntargetdata),nrow(ntargetdata)*0.7)
train <- ntargetdata[a,]
test <- ntargetdata[-a,]

# CREATE THE MODEL ON TRAIN DATA LINEAR REGRESSION
model1 = lm(train$Fees ~ .,data = train)
summary(model1)

# CREATE THE MODEL ON TRAIN DATA RANDOM FOREST
library(randomForest)
set.seed(222)
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)

model2 = randomForest(train$Fees ~ .,data = train,method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(model2)
summary(model2)

library(caret)
p1=predict(model2, train)



plot(p1, col=c("green","red"))

train$pred=p1

actual <- train$Fees
predicted <- train$pred

R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))

# CHECK VARIABLE INFLUENCE FACTOR SCORE
library(fmsb)
VIF(model1)

#predict

pred <- predict(model1, test)
pred_df <- data.frame(pred, test$Fees)
plot(pred, col=c("green","red"))

# VIEW THE RESIDUAL ERROR
library(qqplotr)
library(ggplot2)
library(MASS)
library(fitdistrplus)
qqnorm(resid(model1))
qqline(resid(model1))

# VALIDATE THE MODEL - RSQUARE COMPARISON
# In train data, we get Rsquare from model. Value is 0.07789.
# In test data, we need to manually calculate Rsquare.
SSE <- sum((test$Fees - pred) ^ 2)
SST <- sum((test$Fees - mean(test$Fees)) ^ 2)
rsquare <- 1 - SSE/SST
print(rsquare)

# VALIDATE THE MODEL - RESIDUAL STANDARD ERROR COMPARISON
# In train data, we get residual standard error from model (174.8)
# In test data, we need to manually calculate residual standard error.
rmse = sqrt(mean(model1$residuals^2))
print(rmse)

write.csv(pred, "pred.csv", row.names = FALSE)


