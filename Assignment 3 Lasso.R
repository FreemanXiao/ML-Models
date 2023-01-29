#Assignment 3 LassoL:
#Xiao Zhang

#Load Data and preparation
library(glmnet)
rm(list = ls())
dat <- 
  read.csv("~/Desktop/UW - Madison/Fall 2022/GB 656/Module4_ScriptsAndSlides/Assginment 4/train.csv")
dim(dat)
dat[1:2,]
dat <- dat[,-1]
dim(dat)

#Preparing the data
loss <- dat$loss
hist(loss)
hist(log(loss))
qqnorm(log(loss))
qqline(log(loss), col="blue", lwd=3)

quantile(loss)
quantile(loss, p=seq(0,1,.01))
sum(loss < 100)
loss[loss<100]

# we take out the loss<100
dat <- dat[dat$loss>=100,]
dim(dat)
loss <- dat$loss
hist(log(loss))
qqnorm(log(loss))
qqline(log(loss), col="blue", lwd=3)

#Separate data, 20% for train, 
set.seed(652)
trn <- runif(nrow(dat)) < 0.2
table(trn)
Allstate_train <- dat[trn==TRUE,]
Allstate_test <- dat[trn==FALSE,]
dim(Allstate_train); dim(Allstate_test)
Allstate_test

#build linear regression
r0 <- lm(loss~., data=Allstate_train)
summary(r0)

Y <- Allstate_train$loss
Y.tst <- Allstate_test$loss
Allstate_test$loss

dim(Y.tst)

do.RMSE.trn <- function(yhat)  sqrt( mean( (Y-yhat)^2 ) )
do.RMSE.tst <- function(yhat)  sqrt( mean( (Y.tst-yhat)^2 ) )

RMSE.trn_OLS <- do.RMSE.trn(predict(r0, data = Allstate_train))
RMSE.tst_OLS <- do.RMSE.tst(predict(r0, data = Allstate_test))
RMSE.trn_OLS; RMSE.tst_OLS

#well.. lasso model. 
Allstate_train <- dat[trn==TRUE,]
Allstate_test <- dat[trn==FALSE,]
dim(Allstate_train)
dim(Allstate_test)

#Update
dim(dat)
dat$loss

Allstate_Matrix <- model.matrix(loss~., dat)
Lasso_Allstate_train <- Allstate_Matrix[trn==TRUE,]
Lasso_Allstate_test <- Allstate_Matrix[trn==FALSE,]

## Allstate_X_train <- data.matrix(Allstate_train[,c(1:130)])

# Create matrix
Allstate_X_train <- data.matrix(Lasso_Allstate_train[,-1])
Allstate_Y_train <- Allstate_train$loss
Allstate_X_test <- data.matrix(Lasso_Allstate_test[,-1])
Allstate_Y_test <- Allstate_test$loss


#Create Lasso model. 
lasso_mod <- glmnet(Allstate_X_train, Allstate_Y_train, family = "gaussian", alpha = 1, standardize = TRUE, nlambda=10)
#Check the plot. 
plot(lasso_mod, lwd=3, xvar = "lambda")
coef(lasso_mod)

#check the mse for train set
mse_train <- colMeans((replicate(10, Allstate_Y_train)-predict(lasso_mod,Allstate_X_train))^2)
plot(mse_train,type = "o", lwd=3,col="blue",xlab="model complexity")
#Check out of sample test
mse_test <- colMeans((replicate(10, Allstate_Y_test)-predict(lasso_mod,Allstate_X_test))^2)
lines(mse_test,type = "o", lwd=3,col="red")
mse_test

#Model
lasso_mod$lambda[6]
log(lasso_mod$lambda[6])

#Cross Validation
cv_lasso <- cv.glmnet(Allstate_X_train, Allstate_Y_train, alpha = 1, family="gaussian",k=5)
#Let's illustrate:
plot(cv_lasso)
#Check the result
lambda_lasso <- cv_lasso$lambda.min
lambda_lasso
log(lambda_lasso)

#Final model.
lasso_best <- glmnet(Allstate_X_train, Allstate_Y_train, family = "gaussian", alpha = 1, lambda = lambda_lasso, standardize = TRUE)
cor(predict(lasso_best,Allstate_X_test),Allstate_Y_test)^2
cor(predict(lasso_mod,Allstate_X_test)[,6],Allstate_Y_test)^2

lasso_mod$lambda[6]
lasso_best

#Evaluate:
RMSE.trn_OLS; RMSE.tst_OLS

Y <- Allstate_train$loss
Y.tst <- Allstate_test$loss

do.RMSE.trn <- function(yhat)  sqrt( mean( (Y-yhat)^2 ) )
do.RMSE.tst <- function(yhat)  sqrt( mean( (Y.tst-yhat)^2 ) )

RMSE.trn_lasso <- do.RMSE.trn(predict(lasso_best,Allstate_X_train))
RMSE.tst_lasso <- do.RMSE.tst(predict(lasso_best,Allstate_X_test))
RMSE.trn_lasso; RMSE.tst_lasso



