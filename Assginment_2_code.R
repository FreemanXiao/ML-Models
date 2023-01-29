#GB 656 Assginment 2
#Xiao Zhang

#Import data
car_data <- 
  read.csv("~/Desktop/UW - Madison/Fall 2022/GB 656/Module2_SlidesAndScripts/Assginment 2/Caravan.csv")
dim(car_data)
#check the data
library(psych)
describe(car_data)[,1:9]
head(car_data)

#	Define buy or not, 
table(car_data$Purchase)
car_data$Purchase<- I(car_data$Purchase == 'Yes') * 1 
car_data$Purchase
dim(car_data)
describe(car_data)[,1:9]

#分数据 train and test
#分Test 和training data, rest is train, 1000 is Test 
set.seed(42)
car_data <- car_data[sample(1:5822),]
car_data_test  <- car_data[1:1000,]
car_data_train <- car_data[1001:5822,]
dim(car_data_test)

#建立一个预测purchase的Binominal模型，
#creat the logistics model
car_glm <- glm(Purchase ~ ., family="binomial", car_data_train)
car_glm
summary(car_glm)
#我们运行从sample中构建的模型，带入Train中，查看每一个sample的概率
#check the prediction result
car_train_predict <- predict(car_glm, type="response")
car_train_predict
#以0.5 为界限，对于预测结果和真正结果的对比，建立table
#set threshold as 0.5 and create table
#Confusion table
table(car_data_train$Purchase, (car_train_predict >0.5))

#建立一个预测TPR的公式
#TPR
7/(269+7)
TPR <- function(true_y,predict_y)  { sum(true_y==1 & predict_y==1) / sum(true_y==1) }
TPR(car_data_train$Purchase, (car_train_predict >0.5))

#在所有真正为0的sample中，我预测对的概率
#TNR
4539/(4539+7) #TRUE NEGATIVE RATE, SPECIFICITY
TNR <- function(true_y,predict_y)  { sum(true_y==0 & predict_y==0) / sum(true_y==0) }
TNR(car_data_train$Purchase, (car_train_predict >0.5))

#我们用train data预测的模型，来测试test的有效型
#apply to train data
dim(car_data_test)
car_data_test$car_test_predict <- predict(car_glm, car_data_test, type="response")
car_data_test$car_test_predict
#建立true&false table confusion table
table(car_data_test$Purchase, (car_data_test$car_test_predict > 0.5))
#在所有真实的1中，预测成功的概率, tpr
TPR(car_data_test$Purchase, (car_data_test$car_test_predic > 0.5))
#在所有真实的0中，预测成功的概率, tnr
TNR(car_data_test$Purchase, (car_data_test$car_test_predic > 0.5))



