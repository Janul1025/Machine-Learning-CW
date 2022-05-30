install.packages("nnfor")
install.packages("Metrics")
library(Metrics)
library(neuralnet)
library(caret)
library(nnfor)
library(Metrics)



uow_load <- read_excel("D:/IIT/2nd semester/ML/CW/UoW_load.xlsx")
uow_load$Dates <- as.numeric(uow_load$Dates)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


View(uow_load)

boxplot(uow_load)
maxminuow <- as.data.frame(lapply(uow_load,normalize))
boxplot(maxminuow)


colnames(maxminuow) <- c("Dates","Nine","Ten","Eleven")


#NN
trainuow = maxminuow[1:430,]
testuow = maxminuow[431:500,] 


#NNUoW
NNUoWLoad <- neuralnet(Eleven ~ Dates+Eleven+Ten+Nine, data=trainuow, hidden=c(1,2),linear.output=TRUE, threshold=0.01)
plot(NNUoWLoad) 


#Evaluation model performance
model1Result <- predict(NNUoWLoad, testuow)
model1Result



# and find its maximum & minimum value
min1 <- min(trainuow)
max1 <- max(trainuow)


 # display its contents
head(trainuow)

#Create the reverse of normalised function - renormalized
unnormalizing <- function(x, min, max) { 
  return( (max - min)*x + min )
}

renormormalized_prediction_value <- unnormalizing(model1Result, min1, max1)
renormormalized_prediction_value   # this is NN's output re-normalized to original ranges


myPred <- predict(NNUoWLoad, testuow)

results <- data.frame(actual = testuow$Eleven, prediction = myPred)

predicted= myPred* abs(diff(range(testuow$Eleven))) + min(testuow$Eleven)
actual= testuow$Eleven*abs(diff(range(testuow$Eleven)))+min(testuow$Eleven)
comparison= data.frame(predicted,actual)
deviation=((actual-predicted)/actual)

#RMSE
RMSE.NN =( sum((testuow$Eleven - predicted)^2)/nrow(testuow)) ^ 0.5
RMSE.NN
predicted=as.double(predicted)
rmse(actual,predicted)
plot(RMSE.NN)

rmse(actual,predicted)

#MSE/m
rmse(actual,predicted)
plot(RMSE.NN)

mae(actual,predicted)
#MAPE
mape(predicted,actual)

plot(testuow$Eleven, predicted, col="blue",ph=16,ylab="predicted",xlab= "actual")



# Predicted part
plot(testuow$Eleven , ylab = "Predicted vs Expected", type="l", col="red")
par(new=TRUE)
plot(predicted, ylab = " ", yaxt="n", type="l", col="green" ,main='Predicted 
Value Vs Expected Value Graph')
legend("topleft",
       c("Expected","Predicted"),
       fill=c("red","green")
)
 





 
 