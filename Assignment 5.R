# Assignment 5: Classification Trees
# Xiao Zhang

#Load data
rm(list=ls())
wine <- 
  read.table("~/Desktop/UW - Madison/Fall 2022/GB 656/Module 6/Assignment 5/winequality-red.csv", sep=";", header=TRUE)
head(wine)

#Set up 
table(wine$quality)
wine$quality <- I(wine$quality > 6) * 1 #这一步，将quality改写，大于6的：7和8为1，其余为0
wine$quality
describe(wine)[,1:9]#查看对比，最后一行平均为0.14.

set.seed(42)
wine_trn <- runif(nrow(wine)) < .7
wine_train <- wine[wine_trn==TRUE,]
wine_test <- wine[wine_trn==FALSE,]
dim(wine_train)

#Module 2 model
wine_glm <- glm(quality ~ ., family="binomial", data=wine_train)

summary(wine_glm)
#我们运行从sample中构建的模型，带入Train中，查看每一个sample的概率
wine_train_predict <- predict(wine_glm, type="response")
wine_glm

#以0.5 为界限，对于预测结果和真正结果的对比，建立table
#Confusion table
table(wine_train$quality, (wine_train_predict >0.5))

#Apply to test
dim(wine_test)
wine_test$wine_test_predict <- predict(wine_glm, wine_test, type="response")
table(wine_test$quality, (wine_test$wine_test_predict > 0.5))

#create the roc curve and the auc
library("pROC")
wine.roc <- roc(wine_test$quality, wine_test$wine_test_predict, direction="<")
wine.roc
plot(wine.roc, lwd=3)

#create the classification tress
library(rpart)
wine_form1 <- formula(quality~.)
wine_t1 <- rpart(wine_form1, data=wine_train, cp=.001, method="class")

plot(wine_t1,uniform=T,compress=T,margin=.05,branch=0.3)
text(wine_t1, cex=.7, col="navy",use.n=TRUE)
plotcp(wine_t1)
summary(wine_t1)

wine_CP <- printcp(wine_t1)
wine_cp <- wine_CP[,1][wine_CP[,2] == 15]
wine_cp[1]

#Prune
wine_t2 <- prune(wine_t1,cp=wine_cp[1])
plot(wine_t2,uniform=T,compress=T,margin=.05,branch=0.3)
text(wine_t2, cex=.7, col="navy",use.n=TRUE)
summary(wine_t2)

#New tree
wine_t2_test <- predict(wine_t2, wine_test, type="prob")[,2]
table(wine_test$quality, (wine_t2_test > 0.5))

#AUC and ROC curve
library("pROC")

wine_t2.roc <- roc(wine_test$quality, wine_t2_test, direction="<")
wine_t2.roc
plot(wine_t2.roc, lwd=3)

#create a cv 
library(tree)
wine_t3 <- tree(quality ~ .,  data=wine_train)
plot(wine_t3)
text(wine_t3, pretty=0)

wine.t3.cv <- cv.tree(wine_t3, K = 5)
plot(wine.t3.cv$size, wine.t3.cv$dev, type='b')

wine_t4 = prune.tree(wine_t3, best = 15)
plot(wine_t4)
text(wine_t4)

# AUC and curve with the new tree after cv
wine_t4_test <- predict(wine_t4, newdata = wine_test)

table(wine_test$quality, (wine_t4_test > 0.5))
library("pROC")

wine_t4.roc <- roc(wine_test$quality, wine_t4_test, direction="<")
wine_t4.roc
plot(wine_t4.roc, lwd=3)


