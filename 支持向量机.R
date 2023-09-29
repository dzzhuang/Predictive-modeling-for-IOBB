#支持向量机需要加载R包“e1071”。
install.packages("e1071")
library(e1071)       # 支持向量机建模
library(pROC)       # 用于计算ROC，前者用于plot画法，后者用于ggplot画法
library(magrittr)      # 我主要使用它的管道函数，使代码看起来更优雅，思路更清晰
#今天我们用的数据集是经典的红酒质量数据集。数据集稍后我会传到网盘上，再给大家发链接。需要注意的是，read.csv的时候，分隔符是“;”。拿不准分隔符是啥的时候，一定要打开数据集看看。
#dataset
arget.url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/undocumented/connectionist-bench/sonar/sonar.all-data'
set.seed(17)
require(caret)
data <- read.csv("总病例.csv")
folds <- createFolds(y=data[,34],k=5)
library(pROC)
max=0
num=0
auc_value<-as.numeric()
for(i in 1:5){
  fold_test <- data[folds[[i]],] #取folds[[i]]作为测试集
  fold_train <- data[-folds[[i]],] # 剩下的数据作为训练集
  fold_pre <- svm(as.numeric(encephalocele)~.,data=fold_train,kernel = "linear")
  fold_predict <- predict(fold_pre,type='response',newdata=fold_test)
  auc_value<- append(auc_value,as.numeric(auc(as.numeric(fold_test[,34]),fold_predict)))
}
num<-which.max(auc_value)
print(auc_value)
fold_test <- data[folds[[num]],]
fold_train <- data[-folds[[num]],]
Modelsvm <- svm(as.factor(encephalocele)~.,data = fold_train,scale = T,kernel="linear")  # 模型建立"polynomial""radial""linear"
Presvm <- predict(Modelsvm,fold_test[,-ncol(fold_test)]) # 以上训练的模型对测试集进行验证
PicPRoc <- roc(Presvm,fold_test[,ncol(fold_test)])   #运用pRoc包的roc函数计算roc
PicPRoc
plot(PicPRoc, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE, 
     auc.polygon.col="skyblue", print.thres=TRUE)
#data.frame(fold_test,fold_predict)#将state.division列添加到原始数据集并且合并成新的数据框
#modelroc <- roc(fold_test$mRS, fold_predict)
#plot(modelroc, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)
#train test split
s <- sample(nrow(redwine),nrow(redwine)*0.7,replace = F)
trainset <- redwine[s,]
testset <- redwine[-s,]
dim(testset)
#数据预处理，首先随机取70%的数据集作为训练集。剩下的30%作为测试集。可以用dim功能检查一下测试集的行列数。
#training
fit1 <- svm(encephalocele ~.,data = trainset,kernel = "linear",probability =TRUE)
summary(fit1)
fit2 <- svm(encephalocele ~.,data = trainset,kernel = "sigmoid")
summary(fit2)
#在这里我建立了两个模型，采用了不同的kernel函数。画个图看看模型效果吧。
#plot svm
plot(fit2,trainset,sulphates~density)
#然后在同一个测试集上进行预测，对比两个模型的效果。
#predict
p1 <- predict(fit1,testset)
p2 <- predict(fit2,testset)
#模型效果的衡量指标有很多，下次有空咱们详细说说，今天就先看看准确率吧。
t1 <- table(p1,testset$encephalocele)
acc1 <- sum(diag(t1))/nrow(testset)
acc1
t2 <- table(p2,testset$encephalocele)
acc2 <- sum(diag(t2))/nrow(testset)
acc2



library(e1071)       # 支持向量机建模
library(pROC)       # 用于计算ROC，前者用于plot画法，后者用于ggplot画法
library(magrittr)      # 我主要使用它的管道函数，使代码看起来更优雅，思路更清晰
setwd("I:\\9-SWH论文合作")          # 设置工作路径
files <- list.files()                    # 列出工作路径所有文件
RadiomicsData <- read.csv(files[8])     # 读取需要数据集
# str(RadiomicsData)                 # 查看数据集结构、指标类别
RadiomicsData <- RadiomicsData[,-1]   # 我的第一列是行数，我不需要，因此删掉它
Rows <- nrow(RadiomicsData)                    # 样本数【多少行即多少人】
TrainRows <- sample(1:Rows,0.7*Rows %>% round(0),replace = F) # 选定训练集样本
Trainsvm <- RadiomicsData[TrainRows,]             # 训练集确定
Testsvm <- RadiomicsData[-TrainRows,]             # 测试集确定
Modelsvm <- svm(as.factor(mRS)~.,data = Trainsvm,scale = T,kernel="sigmoid")  # 模型建立"polynomial""radial""linear"
Presvm <- predict(Modelsvm,Testsvm[,-ncol(Testsvm)]) # 以上训练的模型对测试集进行验证
PicPRoc <- roc(Presvm,Testsvm[,ncol(Testsvm)])   #运用pRoc包的roc函数计算roc
PicPRoc
plot(PicPRoc, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE, 
     auc.polygon.col="skyblue", print.thres=TRUE)


plot(PicPRoc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE)
