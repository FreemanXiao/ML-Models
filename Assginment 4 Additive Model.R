#Assginment 4: Building an Additive Model
#Xiao Zhang

#Import and review the data
library(mgcv)

wage <- 
  read.csv("~/Desktop/UW - Madison/Fall 2022/GB 656/Module5_ScriptsAndSlides/Assginment/Wage.csv")
head(wage)
summary(wage)

wage_gam <- gam(wage ~ s(year,bs="cr",k=7) + s(age,bs="cr",k=3) + education,data=wage)
wage_gam$education
plot(wage_gam, scale=0)

cor(predict(gam2,newdata = wage),wage$wage)^2




