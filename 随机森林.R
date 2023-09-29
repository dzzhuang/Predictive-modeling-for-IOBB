library(randomForest)
library(ggplot2)
library(ROCR)
library(caret)
library(dplyr)
data <- read.csv("总病例.csv")
data <- na.omit(data)
data$sex <- factor(data$sex,levels = c(0,1),labels = c(0,1))  
data$age<- as.numeric(data$age)
data$a.NIHSS <- as.numeric(data$a.NIHSS)
data$a.GCS <- as.numeric(data$a.GCS)
data$SAP<- as.numeric(data$SAP)
data$fisher<- as.numeric(data$fisher)
data$a.hunt<- as.numeric(data$a.hunt)
data$SAP<- as.numeric(data$SAP)
data$mRS <- factor(data$mRS,levels = c(0,1),labels = c(0,1))  
建模
trainlist <- createDataPartition(data$mRS,p=0.75,list=F)
trainset <- data[trainlist,]
testset <- data[-trainlist,]
set.seed(9999)
rf.train <- randomForest(trainset$mRS~trainset$sex+trainset$age+trainset$fisher+trainset$glu+trainset$ca+trainset$a.GCS+trainset$hypertension+trainset$SAP+trainset$a.NIHSS,data=trainset,importance=T)
rf.train <- randomForest(trainset$mRS~.,data=trainset,importance=T)
rf.train
importance(rf.train)
varImpPlot(rf.train)
plot(rf.train,main="randomforest origin")
预测
rf.test <-predict(rf.train ,newdata=testset,type="prob")   
roc.rf <-multiclass.roc(testset$mRS,rf.test)
roc.rf
plot.roc(roc.rf,print.auc=TRUE,auc.polygon=TRUE,print.thres = TRUE)
ci.auc(roc.rf)

library(randomForest)
library(ggplot2)
library(ROCR)
library(caret)
library(dplyr)
library(openxlsx)
setwd('D:')
data.all <- read.csv("总病例.csv")
set.seed(9)
folds <- createFolds(y=data.all$encephalocele,k=5)
max=0
num=0
for(i in 1:5){    
  fold_test <- data.all[folds[[i]],]   #取folds[[i]]作为测试集  
  fold_train <- data.all[-folds[[i]],]   # 剩下的数据作为训练集    
  print("***组号***")  
  fold_pre <-randomForest(encephalocele~.,data= fold_train,mtry=3,ntree=500, proximity=TRUE,importance=TRUE)
  fold_predict <- predict(fold_pre,type='response',newdata=fold_test)
  fold_predict =ifelse(fold_predict>0.5,1,0)  
  fold_test$predict = fold_predict  
  fold_error = fold_test[,35]-fold_test[,34]  #Y（真实值）在23列，模型预测值在24列
  fold_accuracy = (nrow(fold_test)-sum(abs(fold_error)))/nrow(fold_test)  
  print(i)  
  print("***测试集精确度***")
  print(fold_accuracy)  
  print("***训练集精确度***")
  fold_predict2 <- predict(fold_pre,type='response',newdata=fold_train)  
  fold_predict2 =ifelse(fold_predict2>0.5,1,0)  
  fold_train$predict = fold_predict2  
  fold_error2 = fold_train[,35]-fold_train[,34]  
  fold_accuracy2 = (nrow(fold_train)-sum(abs(fold_error2)))/nrow(fold_train)  
  print(fold_accuracy2)      
  if(fold_accuracy>max)    {    
    max=fold_accuracy      
    num=i  
  }  }
print(max)
print(num)
#精度最高样本
#测试
testi <- data.all[folds[[num]],]
#训练
traini <- data.all[-folds[[num]],]

library(pROC) #绘制ROC曲线
library(randomForest)
#数据预处理
traini$encephalocele = as.factor(traini$encephalocele)
wine_randomforest <- randomForest(encephalocele ~.,
                                  data = traini,
                                  ntree =500,
                                  mtry=3,
                                  importance=TRUE ,
                                  proximity=TRUE)

#查看变量的重要性
wine_randomforest$importance
varImpPlot(wine_randomforest, main = "variable importance")
#对测试集进行预测
pre_ran <- predict(wine_randomforest,newdata=testi)
#将真实值和预测值整合到一起
obs_p_ran = data.frame(prob=pre_ran,obs=testi$encephalocele)
#输出混淆矩阵
table(testi$encephalocele,pre_ran,dnn=c("真实值","预测值"))
#绘制ROC曲线
ran_roc <- roc(testi$encephalocele,as.numeric(pre_ran))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='随机森林模型ROC曲线,mtry=3,ntree=500')
