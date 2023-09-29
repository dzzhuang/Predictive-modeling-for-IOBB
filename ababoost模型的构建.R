
install.packages("adabag") #adaboost模型
install.packages("RColorBrewer") #用于绘图
library(RColorBrewer)
library(adabag)
library(caret)

a<- read.csv("总病例.csv")
#训练集、测试集划分
set.seed(111)
trainlist <- createDataPartition(a$encephalocele,p = 0.7, list = FALSE)
trainset <- a[trainlist,]
testset <- a[-trainlist,]

trainset$encephalocele = as.factor(trainset$encephalocele)
testset$encephalocele = as.factor(testset$encephalocele)

#Adaboost算法构建模型
fit <- boosting(encephalocele~., data = trainset,boos=TRUE, mfinal=100 )



#预测测试集
pre <- predict(fit,newdata = testset)

#模型评估
adaboost.cf <- caret::confusionMatrix(as.factor(pre$class),as.factor(testset$encephalocele))
adaboost.cf #acc,kappa,混淆矩阵在测试集上预测以后，我们采用caret包来进行模型效果评估。

#多类别计算AUC，multiclass.roc
library(pROC)
pre_prob <- data.frame(pre$prob)
names(pre_prob) <- levels(testset$encephalocele)
roc_ada <- multiclass.roc(testset$encephalocele,pre_prob)
print(roc_ada)

write.csv(pre_prob,"pre_prob.csv")
#将特征重要性存为一个数据框
imp <- data.frame(fit$importance)
imp$variable <- row.names(imp)
#从高到低进行排序
imp <- imp[order(imp$fit.importance,decreasing = T),]
#特征重要性绘图
barplot(rev(imp$fit.importance),horiz=T,xlim=c(-18,56),axes=F,col=rep(brewer.pal(7,'YlOrRd'),each=1))
text(seq(from=0.7,length.out=135,by=1.2),x=-10,label=rev(imp$variable))
axis(3,c(0,10,20,30,40,50),c('0','10','20','30','40','50'))

