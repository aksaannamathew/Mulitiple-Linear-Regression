install.packages("readr")
library(readr)
Startups <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\9 - Multi Linear\\Multiple linear regression\\50_Startups.csv") 
View(Startups)
str(Startups)
table(Startups$State)
Startups$State <- as.integer(factor(Startups$State, levels = c("California", "Florida", "New York"), labels = c(1, 2, 3)))
View(Startups)
str(Startups)
summary(Startups)     #Fisrt Business Decision
install.packages("psych")
library(psych) 
describe(Startups)    #Second Business Decision
install.packages("ggplot2")
library(ggplot2)
pairs(Startups)
ggplot(data = Startups)+geom_histogram(aes(x=Profit,), bins = 40)

#Multiple Linear Regression
model1 <- lm(Profit~., data = Startups)
summary(model1)                        #R^2=0.9507
rmse1 <- sqrt(mean(model1$residuals^2))
rmse1                                  #RMSE=8855.326
pred1 <- predict(model1, newdata = Startups)
cor(pred1, Startups$Profit)            #Accuracy=0.9750622
vif(model1)#No colinearity
influenceIndexPlot(model1, grid = T, id = list(n=10, cex=1.5, col = "blue"))
influencePlot(model1)
influence.measures(model1)
install.packages("car")
library(car)
avPlots(model1)
install.packages("e1071")
library(e1071)
qqPlot(model1)

#Removing the Influencing Variable
model2 <- lm(Profit~., data = Startups[-c(46, 50),])
summary(model2)                        #R^2=0.9644
rmse2 <- sqrt(mean(model2$residuals^2))
rmse2                                  #RMSE=7069.526
pred2 <- predict(model2, newdata = Startups)
cor(pred2, Startups$Profit)            #Accuracy=0.9745358
vif(model2) #No Colinearity
avPlots(model2)
influenceIndexPlot(model2, grid = T, id = list(n=10, cex=1.5, col = "blue"))
influence.measures(model2)
influencePlot(model2)
qqPlot(model2)


#Applying Logarithmic Transformation
z = log(Startups)
View(z)
z <- do.call(data.frame, lapply(z, function(x){
  replace(x, is.infinite(x)|is.na(x),0)
})
)
View(z)
model3 <- lm(Profit~., data = z)
summary(model3)                      #R^2=0.7765
rmse3 <- sqrt(mean(model3$residuals^2))
rmse3                                #RMSE=0.2167657
pred3 <- predict(model3, newdata = z)
cor(pred3, z$Profit)                 #Accuracy=0.8811805
vif(model3) #No colinearity                          
avPlots(model3)
influenceIndexPlot(model3, grid = T, id = list(n=10, cex=1.5, col = "blue"))
influence.measures(model3)
influencePlot(model3)
qqPlot(model3)

#Removing the Influencing Observation
model4 <- lm(Profit~., data = z[-c(45, 48),])
summary(model4)                     #R^2=0.872
rmse4 <- sqrt(mean(model4$residuals^2))
rmse4                               #RMSE=0.1590232
pred4 <- predict(model4, newdata = z)
cor(pred4, z$Profit)                #Accuracy=0.8746968
avPlots(model4)
vif(model4) #No colinearity
influenceIndexPlot(model4, grid = T, id = list(n=10, cex=1.5, col = "blue"))
influence.measures(model4)
influencePlot(model4)
qqPlot(model4)

#model2 posses high R^2 value, therefore it is the best model.
plot(model2)
