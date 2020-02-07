##############  Neural Network  ###################
devtools::install_github("rstudio/reticulate")
library(dplyr)
install_keras()
library(keras)
install_tensorflow()
library(tensorflow)


setwd("~/2019 spring/DSO 562/Project3")
data=read.csv('23varaibles.csv')

data$date=as.Date(data$date)
train_test<-data[data$date<='2016-10-31',]
oot<-data[data$date>'2016-10-31',]

train<-sample(1:nrow(train_test),nrow(train_test)*0.7)
data.train<-train_test[train,]
data.test<-train_test[-train,]
train.x<-data.train[,4:26]
train.y<-data.train[,3]
test.x<-data.test[,4:26]
test.y<-data.test[,3]




model2<-keras_model_sequential() %>%
  layer_dense(units=18, input_shape= c(23), activation="relu")%>%
  layer_dense(units=12, activation="relu")%>%
  layer_dense(units=8, activation="relu")%>%
  layer_dense(units=1, activation="sigmoid")

model2 %>% summary()

model2 %>% compile(
  optimizer=optimizer_adam(lr=0.0005),
  loss="binary_crossentropy",
  metrics=c("accuracy")
)

X<-as.matrix(train.x)
Y<-train.y
length(X)
length(Y)
dim(X)

model2 %>% 
  fit(X,Y)  #batch_size = 64 ,validation_split = 0.2,epochs = 18



?py_call_impl()

##################Yadi Code#########################

library(nnet)
ideal <- class.ind(data.train$fraud_label)
nn= nnet(data.train[,-c(1,2,3)],ideal, size=5,  softmax = TRUE)
#train
nn_pre_train<-predict(nn,data.train[,-c(1,2,3)], type="raw")[,2]
train_res_nn<-data.frame(record=data.train$record,pro=nn_pre_train,Fraud=as.numeric(data.train$fraud_label))
num=floor(0.03*nrow(train_res_nn))#2142
train_res_nn<-train_res_nn[order(-train_res_nn$pro),][0:num,]
sum(train_res_nn$Fraud)/sum(data.train$fraud_label)#0.5246076
#test
nn_pre_test<-predict(nn,data.test[,-c(1,2,3)], type="raw")[,2]
test_res_nn<-data.frame(record=data.test$record,pro=nn_pre_test,Fraud=as.numeric(data.test$fraud_label))
num=floor(0.03*nrow(test_res_nn))#2142
test_res_nn<-test_res_nn[order(-test_res_nn$pro),][0:num,]
sum(test_res_nn$Fraud)/sum(data.test$fraud_label)#0.5141483
#oot
nn_pre_oot<-predict(nn,oot[,-c(1,2,3)], type="raw")[,2]
oot_res_nn<-data.frame(record=oot$record,pro=nn_pre_oot,Fraud=as.numeric(oot$fraud_label))
num=floor(0.03*nrow(oot_res_nn))#2142
oot_res_nn<-oot_res_nn[order(-oot_res_nn$pro),][0:num,]
sum(oot_res_nn$Fraud)/sum(oot$fraud_label)#0.4878458




