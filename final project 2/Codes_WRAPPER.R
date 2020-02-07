library(dplyr)
data=read.csv('data256.csv')
data=subset(data,select=-X)
#1.select 115 var after KS/FDR Jan-Oct.
#2.forward stepwise regression--filter first 100
library(leaps)
fsr_model<- regsubsets(Fraud~ ., data=data,nvmax =100,
                     method = "forward",matrix.logical=TRUE)#not sure about the 20? or 50?
?regsubsets
summary(fsr_model)
#plot(models)
#3.update data>>update xy
#4.LASSO
library(glmnet)
#define x/y
cvfit = cv.glmnet(x, y)#x:input matrix;y:response vector
lasso_model<-glmnet(x, y, family = "binomial", alpha = 1, lambda =cvfit$lambda.min)
summary(c)
#predictors(lasso_model)

#5.output final variable selecton--20
#prepare final 20 var data as data
#6.get train/test dataset
library(caret)
trainIndex = createDataPartition(data$Fraud,
                                 p=0.75, list=FALSE,times=1)

train =data[trainIndex,]
test =data[-trainIndex,]

#logistic regression
lg.reg<-glm(Fraud~.,data=train,family='binomial')
lg.pre<-predict(lg.reg,test,type='response')
#rank test based on lg.res, calculate 30% fraud
#randomForest
library(randomForest)
rf<-randomForest(formula=Fraud~.,train,ntree=500)
rf_pre<- predict(object =rf,   # model object 
                            newdata =test,  # test dataset
                            type = "response") 
#rank test based on lg.res, calculate 30% fraud
#boosting tree
library(gbm)
gra_boost<- gbm(formula = Fraud~ ., 
                    distribution ='bernoulli', 
                    data =train,
                    n.trees = 10000)
ntree_opt<- gbm.perf(object =gra_boost, 
                     method = 'OOB', 
                     oobag.curve = TRUE)
gra_boost<- gbm(formula = Fraud~ ., 
                distribution ='bernoulli', 
                data =train,
                n.trees =gra_boost)
gb_pre<- predict(object =gra_boost, 
                  newdata =test,
                  n.trees = ntree_opt_oob)
#neural network
library(nnet)
nn= nnet(train,size=10, softmax=TRUE)
nn_pre<-predict(nn,test, type="response")
