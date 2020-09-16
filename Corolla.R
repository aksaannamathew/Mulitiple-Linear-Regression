library(readr)
toyota <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\9 - Multi Linear\\Multiple linear regression\\ToyotaCorolla.csv")
View(toyota)
Corolla <- toyota[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)
attach(Corolla)

str(toyota)
summary(toyota)
describe(toyota)

str(Corolla)
summary(Corolla)
describe(Corolla)
plot(Corolla)
install.packages("psych")
library(psych)
pairs(Corolla)
round(cor(Corolla), 3)


#Multilinear Regression
model.corolla <- lm(Price~., Corolla)
summary(model.corolla) #R^2=0.8638
rmse <- sqrt(mean(model.corolla$residuals^2))
rmse #RMSE=1338.258
pred <- predict(model.corolla, newdata = Corolla)
cor(Corolla$Price, pred) #Accuracy=0.9293884
avPlots(model.corolla)
vif(model.corolla)
install.packages("car")
library(car)
influenceIndexPlot(model.corolla, grid=T, id = list(n=10, cex=1.5, col="blue"))
influence.measures(model.corolla)
influencePlot(model.corolla)
qqPlot(model.corolla)


#Removing the influencing observations
model.corolla1 <- lm(Price~., Corolla[-c(81, 222),])
summary(model.corolla1) #R^2=0.8778
rmse1 <- sqrt(mean(model.corolla1$residuals^2))
rmse1 #rmse=1265.72
pred1 <- predict(model.corolla1, newdata = Corolla)
cor(Corolla$Price, pred1) #Accuracy=0.8736161
vif(model.corolla1)
avPlots(model.corolla1)
influenceIndexPlot(model.corolla1, grid = T, id = list(n=10, cex = 1.5, col ="blue"))
influence.measures(model.corolla1)
influencePlot(model.corolla1)
qqPlot(model.corolla1)

#Applying Logarithmic Transformation
z <- log(Corolla)
View(z)
model.corolla2 <- lm(Price~., z)
summary(model.corolla2) #R^2=0.7559
rmse2 <- sqrt(mean(model.corolla2$residuals^2))
rmse2 #RMSE=0.146289
pred2 <- predict(model.corolla2, newdata = z)
cor(Corolla$Price, pred2) #Accuracy=0.9112864
vif(model.corolla2)
avPlots(model.corolla2)
influenceIndexPlot(model.corolla2, grid = TRUE, id = list(n = 10, cex = 1.5, col = "blue"))
influence.measures(model.corolla2)
influencePlot(model.corolla2)
qqPlot(model.corolla2)

#Removing Influencing Observation
model.corolla3 <- lm(Price~., z[-c(185, 186),])
summary(model.corolla3) #R^2=0.7736
rmse3 <- sqrt(mean(model.corolla3$residuals^2))
rmse3 #RMSE=0.1406392
pred3 <- predict(model.corolla3, newdata = z)
cor(Corolla$Price, pred3) #Accuracy=0.9089353
vif(model.corolla3)
avPlots(model.corolla3)
influenceIndexPlot(model.corolla3, grid = TRUE, id = list(n=10, cex=1.5, col="blue"))
influence.measures(model.corolla3)
influencePlot(model.corolla3)
qqPlot(model.corolla3)

#model1 posses the high R^2 value
plot(model.corolla1)
