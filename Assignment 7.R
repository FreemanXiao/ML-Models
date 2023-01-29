# Assignment 7: Neural Nets
# Xiao Zhang

#Load data
rm(list=ls())
library(psych)
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

wine_train$quality
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
wine_t2_test
table(wine_test$quality, (wine_t2_test > 0.5))

#AUC and ROC curve
library("pROC")

wine_t2.roc <- roc(wine_test$quality, wine_t2_test, direction="<")
wine_t2.roc
plot(wine_t2.roc, lwd=3)

#Fit a boosting model
# install.packages("gbm")
library(gbm)
wine_boost <- gbm(quality ~ ., data=wine_train, distribution = "adaboost", 
                  interaction.depth = 6, n.trees = 500, shrinkage = 0.005)
wine_boost_predict <- predict(wine_boost, wine_test,n.trees = 500, type = "response")
wine_boost_predict <- (wine_boost_predict-min(wine_boost_predict))/(max(wine_boost_predict)-min(wine_boost_predict))
wine_boost_predict
table(wine_boost_predict > 0.5,wine_test$quality)

#Boost AUC and ROC curve
wine_boost.roc <- roc(wine_test$quality, wine_boost_predict, direction="<")
wine_boost.roc
plot(wine_boost.roc, lwd=3)

# Deeper Neural Net
library(neuralnet)
str(wine_test)
wine_n2 <- neuralnet(quality ~., data=wine_train, hidden = c(3,2), linear.output=FALSE)
wine_3 <- model.matrix( ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar
                        + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH
                        + sulphates + alcohol, data = as.data.frame(wine_test))
dim(wine_3)
yhat.n2 <- compute(wine_n2, wine_3[,2:12])
table(yhat.n2$net.result > 0.5, wine_test$quality)

wine_n2.roc <- roc(wine_test$quality, yhat.n2$net.result, direction="<")
wine_n2.roc
plot(wine_n2.roc, lwd=3)


# Neural Net
#Set up
library(nnet)
wine_2_train <- wine_train
wine_2_test <- wine_test
wine_2_train$quality <- as.factor(wine_2_train$quality)
wine_2_test$quality <- as.factor(wine_2_test$quality)

#Fir a single neural net
wint_n1 <- nnet(quality ~ ., data = wine_2_train, size = 11, maxit = 1000, decay=0.001)
# Predict it to test data
yhat.n1 <- predict(wint_n1, wine_2_test)
table(yhat.n1 > 0.5,wine_2_test$quality)

#Auc and ROC curve
wine_n1.roc <- roc(wine_2_test$quality, yhat.n1, direction="<")
wine_n1.roc
plot(wine_n1.roc, lwd=3)




