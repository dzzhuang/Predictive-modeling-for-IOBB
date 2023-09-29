library(randomForest)
library(ggplot2)
library("caret")
#当前工作路径
getwd()
#设置自己的工作路径，数据均可输出至该文件夹
setwd('D:')
data.all <- read.csv("附一病例.csv")
set.seed(9)
folds <- createFolds(y=data.all$encephalocele,k=7)
max=0
num=0
for(i in 1:5){    
  fold_test <- data.all[folds[[i]],]   #取folds[[i]]作为测试集  
  fold_train <- data.all[-folds[[i]],]   # 剩下的数据作为训练集    
  print("***组号***")  
  fold_pre <-randomForest(encephalocele~.,data= fold_train,mtry=6,ntree=500, proximity=TRUE,importance=TRUE)
  fold_predict <- predict(fold_pre,type='response',newdata=fold_test)
  fold_predict =ifelse(fold_predict>0.5,1,0)  
  fold_test$predict = fold_predict  
  fold_error = fold_test[,34]-fold_test[,35]  #Y（真实值）在23列，模型预测值在24列
  fold_accuracy = (nrow(fold_test)-sum(abs(fold_error)))/nrow(fold_test)  
  print(i)  
  print("***测试集精确度***")
  print(fold_accuracy)  
  print("***训练集精确度***")
  fold_predict2 <- predict(fold_pre,type='response',newdata=fold_train)  
  fold_predict2 =ifelse(fold_predict2>0.5,1,0)  
  fold_train$predict = fold_predict2  
  fold_error2 = fold_train[,34]-fold_train[,35]  
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
#利用精度最高样本重新构建模型
prei <- randomForest(encephalocele~.,data= traini,ntree=500,mtry=6,proximity=TRUE,importance=TRUE)

#看因子重要性
importance<-importance(x= prei)
importance
set.seed(100)
varImpPlot(prei)

#训练测试集
predicti <- predict (prei,type='response',newdata=testi)
testi$predict = predicti
#训练训练集
predicti2 <- predict (prei,type='response',newdata=traini)
traini$predict = predicti2
#训练全体
predicti3 <- predict (prei,type='response',newdata=data.all)
data.all$predict = predicti3
#输出训练、测试、全体预测值（也可不用输出）
write.csv(traini,"ausfold_train.csv")
write.csv(testi,"ausfold_test.csv")
write.csv(data.all,"ausfold_all.csv")

#绘制ROC曲线
library(ROCR)
library(pROC) 
#训练：    
pred <- prediction(predicti2,traini$encephalocele)   #预测值(0.5二分类之前的预测值)和真实值  
performance(pred,'auc')@y.values        #AUC值
perf <- performance(pred,'tpr','fpr')
plot(perf,col="red" )
#测试：
pred2 <- prediction(predicti,testi$encephalocele)  
performance(pred2,'auc')@y.values      
perf2 <- performance(pred2,'tpr','fpr')
plot(perf2,add=TRUE, col ="blue")#add即为加在上一个ROC图上面（训练）
#legend("bottomright", legend=c("Development AUC=0.998","Validation AUC=0.842"),col=c("red","blue"),lty=1)

#全体：
pred3 <- prediction(predicti3,data.all$mRS)  
performance(pred3,'auc')@y.values      
perf3 <- performance(pred3,'tpr','fpr')
plot(perf3,add=TRUE, col ="green")

#混淆矩阵
#训练：
predict =ifelse(predicti2>0.5,1,0)
traini$predict = predict
true_value=traini[,6]
predict_value=traini[,7]#计算模型精确度
error = predict_value-true_value
accuracy = (nrow(traini)-sum(abs(error)))/nrow(traini)
precision=sum(true_value & predict_value)/sum(predict_value)
recall=sum(predict_value & true_value)/sum(true_value)
F_measure=2*precision*recall/(precision+recall)  
print(accuracy)
print(precision)
print(recall)
print(F_measure)#混淆矩阵，显示结果依次为TP、FN、FP、TN
table(true_value,predict_value)    


#测试
predict =ifelse(predicti>0.5,1,0)
testi$predict = predict
true_value=testi[,6]
predict_value=testi[,7]#计算模型精确度
error = predict_value-true_value
accuracy = (nrow(testi)-sum(abs(error)))/nrow(testi)
precision=sum(true_value & predict_value)/sum(predict_value)
recall=sum(predict_value & true_value)/sum(true_value)
F_measure=2*precision*recall/(precision+recall)  
print(accuracy)
print(precision)
print(recall)
print(F_measure)#混淆矩阵，显示结果依次为TP、FN、FP、TN
table(true_value,predict_value)  

#全体：
predict =ifelse(predicti3>0.5,1,0)
data.all$predict = predict
true_value=data.all[,6]
predict_value=data.all[,7]#计算模型精确度
error = predict_value-true_value
accuracy = (nrow(data.all)-sum(abs(error)))/nrow(data.all)
precision=sum(true_value & predict_value)/sum(predict_value)
recall=sum(predict_value & true_value)/sum(true_value)
F_measure=2*precision*recall/(precision+recall)  
print(accuracy)
print(precision)
print(recall)
print(F_measure)#混淆矩阵，显示结果依次为TP、FN、FP、TN
table(true_value,predict_value)
