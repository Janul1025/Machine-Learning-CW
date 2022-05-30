#Getting the package
install.packages("NbClust")
install.packages("factoextra")
install.packages("caret")
library(NbClust)
library(factoextra)
library(readxl) 
library(knitr)
library(caret)




wine <- read_excel("D:/IIT/2nd semester/ML/CW/Whitewine_v2.xlsx") #File path of the excel


View(wine)



#Replace blanks in column names with a dot

names(wine) <- make.names(names(wine), unique = TRUE)

names(wine)



#Checking outliers of every column

check <- wine$fixed.acidity


boxplot(check)

check <- wine$volatile.acidity

boxplot(check)

check <- wine$citric.acid

boxplot(check)


check <- wine$residual.sugar

boxplot(check)

check <- wine$chlorides

boxplot(check)

check <- wine$free.sulfur.dioxide


boxplot(check)

check <- wine$total.sulfur.dioxide

boxplot(check)

check <- wine$density

boxplot(check)

check <- wine$pH

boxplot(check)

check <- wine$sulphates

boxplot(check)

check <- wine$alcohol

boxplot(check)




summary(wine)



#Fixed acidity  remove

IQR_fixed_acidity <- (7.3 - 6.3)

Upf_fixed_acidity = 7.3 + 1.5 * IQR_fixed_acidity #Upper fence

Upf_fixed_acidity

Lowf_fixed_acidity = 7.3 - 1.5 * IQR_fixed_acidity #Lower fence

Lowf_fixed_acidity



#Volatile acidity

IQR_volatile_acidity  <- (0.32 - 0.21)

Upf_volatile_acidity = 0.32 + 1.5 * IQR_volatile_acidity #Upper fence

Upf_volatile_acidity



#Citric acid

IQR_citric_acid <- (0.39 - 0.27)

Upf_citric_acid = 0.39 + 1.5 * IQR_citric_acid #Upper fence

Upf_citric_acid

Lowf_citric_acid = 039 - 1.5 * IQR_citric_acid #Lower fence

Lowf_citric_acid



#Residual sugar

IQR_residual_sugar <- (10 - 1.7)

Upf_residual_sugar = 10 + 1.5 * IQR_residual_sugar #Upper fence

Upf_residual_sugar



#Chlorides

IQR_chlorides <-(0.05 - 0.036)

Upf_chlorides = 0.05 + 1.5 * IQR_chlorides #Upper fence

Upf_chlorides

Lowf_chlorides = 0.05 - 1.5 * IQR_chlorides #Lower fence

Lowf_chlorides




#Free sulphur dioxide

IQR_free_sulphur_dioxide <-(46 - 24)

Upf_free_sulphur_dioxide = 46 + 1.5 * IQR_free_sulphur_dioxide #Upper fence

Upf_free_sulphur_dioxide



#Total sulphur dioxide

IQR_total_sulphur_dioxide <- (167 - 109)

Upf_total_sulphur_dioxide = 167 + 1.5 * IQR_total_sulphur_dioxide #Upper fence

Upf_total_sulphur_dioxide

Lowf_total_sulphur_dioxide = 167 - 1.5 * IQR_total_sulphur_dioxide #Lower fence

Lowf_total_sulphur_dioxide



#Density

IQR_density <- (0.9961  - 0.9917)

Upf_density = 0.9961  + 1.5 * IQR_density #Upper fence

Upf_density



#PH

IQR_PH <- (3.28 - 3.09)

Upf_PH = 3.28 + 1.5 * IQR_PH #Upper fence

Upf_PH

Lowf_PH = 3.28 - 1.5 * IQR_PH #Lower fence

Lowf_PH



#Sulphates

IQR_sulphates <- (0.55 - 0.41)

Upf_sulphates = 0.55 + 1.5 * IQR_sulphates #Upper fence

Upf_sulphates



#Removing outliers

wine_clean = subset(wine, fixed.acidity <= 8.8 & fixed.acidity >= 5.8 &
                      
                      volatile.acidity <= 0.965 & citric.acid <= 0.57 &
                      
                      citric.acid >= 0.21 & residual.sugar <= 22.45 &
                      
                      chlorides <= 0.071 & chlorides >= 0.029 &
                      
                      free.sulfur.dioxide <= 79 & total.sulfur.dioxide <= 254 &
                      
                      total.sulfur.dioxide >= 80 & density <= 1.0027 & pH <= 3.565
                    
                    & pH > 2.995 & sulphates <= 0.76 & alcohol <= 14.2 & quality <= 8)

boxplot(wine_clean)

View(wine_clean)




#Scaling

wine_final <- wine_clean[,-12]
wine_clean <- as.data.frame(scale(wine_clean))
dim(wine_clean)
View(wine_final)

x = wine_clean$quality


#Apply NbClust method(One of these 3)

noOfClusters = NbClust(wine_final, distance = "euclidean", min.nc = 2,
                       
                       max.nc = 4, method = "kmeans", index = "all")

# Use manhattan for distance

cluster_manhattan = NbClust(wine_final,distance="manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")


#min.nc is minimum number of clusters, max.nc is the maximum number of clusters



#Elbow method

k = 2:4

# Elbow method
WSS = sapply(k, function(k) {kmeans(wine_clean, centers=k)$tot.withinss})
plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")



#Kmeans

results <- kmeans(wine_final, 3)
str(results)
results

results$centers




#Confusion matrix

table(x,results$cluster)

plot(wine_final[c("free.sulfur.dioxide","total.sulfur.dioxide")],col = results$cluster)

points(results$centers[,c("free.sulfur.dioxide","total.sulfur.dioxide")],col = 1:2, pch = 23, cex = 2)

columna<-factor(x)
qualitya<-as.numeric(columna)
confusionMatrix(data=as.factor(c(as.factor(results$cluster))), reference=as.factor(qualitya))

results <- kmeans(wine_final, 3)

results

results$centers



#Confusion matrix

table(x,results$cluster)


points(results$centers[,c("free.sulfur.dioxide","total.sulfur.dioxide")],col = 1:3, pch = 23, cex = 3)

columna<-factor(x)
qualitya<-as.numeric(columna)
confusionMatrix(data=as.factor(c(as.factor(results$cluster))), reference=as.factor(qualitya))




results <- kmeans(wine_final, 4)

results

results$centers




#Confusion matrix

table(x,results$cluster)

plot(wine_final[c("free.sulfur.dioxide","total.sulfur.dioxide")],col = results$cluster)

points(results$centers[,c("free.sulfur.dioxide","total.sulfur.dioxide")],col = 1:4, pch = 23, cex = 4)

columna<-factor(x)
qualitya<-as.numeric(columna)
confusionMatrix(data=as.factor(c(as.factor(results$cluster))), reference=as.factor(qualitya))



#To see the dataset column headings and datatypes

str(wine_final)



#First set the seed function to any value so that the result is reproducible.

#set.seed(26)




#set.seed(42)

#Calculate within sum of squares(withinss) of different no of clusters

WSS = sapply(k, function(k){kmeans(wine_final, centers = k)$tot.withinss})

plot(k, WSS, type = "l", xlab = "Number of k", ylab = "Within sum of squares")
#Scaling
wine_final <- as.data.frame(scale(wine_clean[9:12]))

#wine_final <- wine_clean[,-12]

View(wine_final)

x = wine_clean$quality

#PCA
jrwine.mp <- prcomp(wine_clean[,c(1:11)], center = TRUE, scale. = TRUE) 
summary(jrwine.mp)

results <- kmeans(wine_final[1:3], 2)


columna<-factor(x)


qualitya<-as.numeric(columna)
confusionMatrix(data=as.factor(c(as.factor(results$cluster))), reference=as.factor(qualitya))

results

results$centers



#Confusion matrix

table(x,results$cluster)

plot(wine_final[c("free.sulfur.dioxide","total.sulfurdioxide")],col = results$cluster)


columna<-factor(x)
qualitya<-as.numeric(columna)
confusionMatrix(data=as.factor(c(as.factor(results$cluster))), reference=as.factor(qualitya))


