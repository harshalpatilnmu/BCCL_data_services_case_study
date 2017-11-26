library(ROCR)
library(caTools)
library(mice)
data_train<-read.csv('D:/BCCL_DATA_SERVICE/challenge/train_data.csv')
data_test<-read.csv('D:/BCCL_DATA_SERVICE/challenge/test_data.csv')
data_predict<-read.csv('D:/BCCL_DATA_SERVICE/challenge/train_prediction.csv')
str(data_train)
str(data_predict)
data_train$Result=data_predict$status
str(data_train)
genderint=ifelse(data_train$gender=="F",0,ifelse(data_train$gender=='M',1,2))
data_train$genderint=genderint
str(data_train)
as.integer(data_train$gender)
data_train$genderint=genderint
str(data_train)
data_train$gender=as.integer(data_train$gender)
data_train$qualification=as.integer(data_train$qualification)
data_train$is_self_employed=as.integer(data_train$is_self_employed)
data_train$property_area=as.integer(data_train$property_area)
data_train$Result=as.integer(data_train$Result)
set.seed(88)
spt1=sample.split(data_train$Result,0.70)
spt1
trainquality=subset(data_train,spt1==TRUE)
test=subset(data_train,spt1==FALSE)
str(trainquality)
pc=rpart(formula = Result~.,data=trainquality,method = "class", minbucket=5)
pc
prp(pc)
pretest = predict(pc, newdata = test, type = "class")
pretest
table(test$Result, pretest)
4+16
60/(60+7+3)
