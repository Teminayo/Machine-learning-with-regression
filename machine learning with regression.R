
library(caret)
library(ggplot2)

library(readr)
Superstore_Dataset <- read_csv("Downloads/Superstore_Dataset.csv")
View(Superstore_Dataset)
superstore <- Superstore_Dataset
head(superstore)
summary(superstore)
attach(superstore)

#spliting the data into training and testing set
set.seed(12)
split <- createDataPartition(superstore$Sales, p=0.7, list = FALSE)
Training <- superstore[split,] # 
Testing <- superstore[-split,]
Training



#create model
model <- lm(Profit~Quantity+Discount, data = Training)
summary(model)

#predict
pred <- predict(model,Testing)
pred
#comparing predicted vs actual values
plot(Testing$Profit ,type = "l", lty =1.8,col ="red")
lines(pred,type = "l",col= "blue")
plot(pred ,type = "l", lty =1.8,col ="yellow")
plot(Testing$Quantity, col="red", lwd=3)

#acuracy
RMSE <- sqrt(mean(pred-superstore$Profit)^2)
RMSE
