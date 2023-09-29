library(rpart)
library(tibble)
library(bitops)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ggplot2)
arget.url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/undocumented/connectionist-bench/sonar/sonar.all-data'
set.seed(17)
require(caret)
data <- read.csv("总病例.csv")
folds <- createFolds(y=data[,34],k=7)
library(pROC)
max=0
num=0
auc_value<-as.numeric()
for(i in 1:5){
  fold_test <- data[folds[[i]],] #取folds[[i]]作为测试集
  fold_train <- data[-folds[[i]],] # 剩下的数据作为训练集
  fold_pre <- rpart(as.numeric(encephalocele)~.,data=fold_train)
  fold_predict <- predict(fold_pre,newdata=fold_test)
  auc_value<- append(auc_value,as.numeric(auc(as.numeric(fold_test[,34]),fold_predict)))
}
num<-which.max(auc_value)
print(auc_value)
fold_test <- data[folds[[num]],]
fold_train <- data[-folds[[num]],]
fold_train$encephalocele <- factor(fold_train$encephalocele)
fold_pre <- rpart(as.factor(encephalocele)~.,data=fold_train)
fancyRpartPlot(fold_pre)
x<-subset(fold_test,select=-encephalocele)
pred<-predict(fold_pre,x,type="class")
k<-fold_test[,"encephalocele"]
table(pred,k)
predtree_v<-predict(fold_pre,newdata=fold_test,type="class")
t<-data.frame(predtree_v,fold_test$encephalocele)
t$predtree_v <- as.numeric(t$predtree_v)
p_t<-t$predtree_v
r_t<-t$fold_test.encephalocele

library(pROC) 
modelroc <- roc(p_t,r_t) 
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE, 
     auc.polygon.col="skyblue", print.thres=TRUE)




#今天来说说R语言的决策树模型构建。
#首先，需要安装两个R包。'rpart' 就是用来做决策树建模的，'rpart.plot'是用来绘制决策树的树形图的包。当然，如果你之前已经安装过，那就只用library加载一下就好。
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
#然后，我们调用一下R里自带的数据库，iris。
#iris
trainset <- read.csv("福建病例.csv")
testset <- read.csv("附一病例.csv")

#attach(mydata)
#summary(mydata)

#data split
#s = sample(c(1:nrow(iris)),120,replace = F)
#trainset = iris[s,]
#testset = iris[-s,]
#在构建预测模型之前，不管是用决策树，还是用随机森林，还是其他什么模型，首先要进行数据集的划分！我这里用的是sample功能，生成一串指定长度的随机数，再到iris数据集里去找到这些随机的样本作为训练集。剩下的就作为预测集。

#在严谨的论文、实验设计里，一般是划分3个数据集，分别是训练集、验证集、测试集。其中，验证集是用来作为模型训练的依据，也参与到训练的过程里，让模型能根据验证集的结果及时做出训练的调整。测试集是独立于模型训练过程之外，用于衡量模型效果。

#在这里咱们只是演示，就不整3个数据集这么麻烦，但你论文里千万别忘了嗷！回头别怪我，这锅我不背。
trainset$encephalocele <- factor(trainset$encephalocele)
fit1 = rpart(as.factor(encephalocele)~.,data = trainset)
summary(fit1)
#plot
rpart.plot(fit1,type = 2)
#predict
pred<-predict(fit1,testset,type = "class")

#图中可以看出，这三类在训练集被成功的区分开了，那么在测试集的表现如何呢？在这一步需要注意，type='class'就是输出预测的标签。

t = table(pred,testset$encephalocele)
acc = sum(diag(t))/nrow(testset) *100
print(acc)


t<-data.frame(pred,testset$encephalocele)
t$pred <- as.numeric(t$pred)
p_t<-t$pred
r_t<-t$testset.encephalocele

library(pROC) 
modelroc <- roc(p_t,r_t) 
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE, 
     auc.polygon.col="skyblue", print.thres=TRUE)

#将class改为prob就可以得到概率，再write.csv(pred,"ROC.csv")输出概率在spss中画ROC