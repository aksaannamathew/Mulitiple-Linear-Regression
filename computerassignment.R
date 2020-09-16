install.packages("readr")
library(readr)
Computer_Data <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\9 - Multi Linear\\Multiple linear regression\\Computer_Data.csv")
View(Computer_Data)
com <- Computer_Data[,-1]
View(com)
attach(com)

com$cd <- as.integer(factor(com$cd, levels = c("yes", "no"), labels = c(1, 0)))
com$multi <- as.integer(factor(com$multi, levels = c("yes", "no"), labels = c(1, 0)))
com$premium <- as.integer(factor(com$premium, levels = c("yes", "no"), labels = c(1, 0)))
View(com)
attach(com)

str(com)
summary(com) #1st business decision
install.packages("psych")
library(psych)
describe(com) #2nd business decision
plot(com)
pairs(com)
install.packages("GGally")
library(GGally)
install.packages("ggplot2")
library(ggplot2)
ggplot(data=com)+geom_histogram(aes(x=price,),bin=40)

#Mutiple Linear Regression
model_com1 <- lm(com$price~.,data = com)
summary(model_com1)                                  #R^2=0.7756
rmse1 <- sqrt(mean(model_com1$residuals^2))
rmse1                                                #RMSE=275.1298
pred1 <- predict(model_com1, newdata = com)
cor(pred1, com$price)                                #Accuracy=0.8806631
vif(model_com1)
avPlots(model_com1)
influenceIndexPlot(model_com1, grid = T, id = list(n=10, cex=1.5, col="blue"))
influence.measures(model_com1)
influencePlot(model_com1)
qqPlot(model_com1)

#Removing influencing Observations
model_com2 <- lm(price~., data = com[-c(1441, 1701),])
summary(model_com2)                                  #R^2=0.7777
rmse2 <- sqrt(mean(model_com2$residuals^2))
rmse2                                                #RMSE=272.8675
pred2 <- predict(model_com2, newdata = com)
vif(model_com2)
cor(pred2, com$price)                                #Accuracy=0.8806566
avPlots(model_com2)
qqPlot(model_com2)

#Applying Logarithmic Transformation
x <- log(com[, -1])
log_com <- data.frame(com[,1], x)
colnames(log_com)
attach(log_com)
View(log_com)
model_com3 <- lm(com...1.~speed+hd+ram+screen+cd+multi+premium+ads+trend, data=log_com)
summary(model_com3)                                   #R^2=0.7426
rmse3 <- sqrt(mean(model_com3$residuals^2))
rmse3                                                 #RMSE=294.653
pred3 <- predict(model_com3, newdata = log_com)
cor(pred3, log_com$com...1.)                          #Accuracy=0.8617343
vif(model_com3)
avPlots(model_com3)
qqPlot(model_com3)
influenceIndexPlot(model_com3, grid = T, id = list(n=10, cex=1.5, col="blue"))
influence <- as.integer(rownames(influencePlot(model_com3, grid = T, id = list(n=10, cex=1.5, col="blue"))))
influence

#Log Transformation with Removing Influencial Observations
model_com4 <- lm(com...1.~speed+hd+ram+screen+cd+multi+premium+ads+trend, data = log_com[-c(1441, 1701)])
summary(model_com4)                                   #R^2=0.7426
rmse4 <- sqrt(mean(model_com4$residuals^2))
rmse4                                                 #RMSE=294.653
pred4 <- predict(model_com4, newdata = log_com)
cor(pred4, log_com$com...1.)                          #Accuracy=0.8617343
avPlots(model_com4)
qqPlot(model_com4)

#model_com2 has the best model with high R^2 value and lesse RMSR
plot(model_com2)
