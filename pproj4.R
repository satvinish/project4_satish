## Import data 
## by setting Working directory
setwd("C:/Users/satish/Desktop")
cancerTrain <- read.csv("CancerData.csv", na.strings = c(""," ","NA"))     

############################## Missing Values #########################
## Visualize Na terms
library(Amelia)
missmap(cancerTrain)
sapply(cancerTrain,function(x) sum(is.na(x)))

#### Delete Obervations with NA values
compTrain <- na.omit(cancerTrain)

#### Impute mean/median/mode 
library(ggplot2)

#### CancerData
ggplot(cancerTrain, aes(1, diagnosis)) + geom_boxplot()
cancerTrain$diagnosis<- as.integer(cancerTrain$diagnosis)
hist(cancerTrain$diagnosis)
# Impute by Median
cancerTrain$diagnosis[is.na(cancerTrain$diagnosis)]<-
  median(cancerTrain$diagnosis, na.rm = T)

## Mice Package
library(mice)
l<-cancerTrain[,c(2:32)]
imputed_Data <- mice(l, m=5, maxit = 50, method = 'pmm', seed = 500)

############################# Outliers Treatment ###################
## diagnosis Variable
library(ggplot2)
ggplot(l, aes(1,diagnosis)) + geom_boxplot(outlier.colour = "red",
                                            outlier.shape = 2)
## Labeling Outliers 
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

library(dplyr)
l %>%
  mutate(outlier = ifelse(is_outlier(diagnosis), diagnosis, as.numeric(NA))) %>%
  ggplot(.,aes(1,diagnosis)) + geom_boxplot(fill = "steelblue",outlier.colour = "red",
                                             outlier.shape = 2)+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)

## diagnosis
boxplot(l$diagnosis)
ggplot(l, aes(1,diagnosis)) + geom_boxplot(outlier.colour = "red",outlier.shape = 2)
qnt <- quantile(l$diagnosis, 0.75, na.rm = T)
caps <- quantile(l$diagnosis, 0.95, na.rm = T)
H <- 1.5 * IQR(l$diagnosis, na.rm = T)
l$diagnosis[l$diagnosis > (qnt +  H)] <- caps

## relationship among various features
contVars<-c("radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean","compactness_mean","concavity_mean","concave points_mean",
"symmetry_mean","fractal_dimension_mean","radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se",
"concave points_se","symmetry_se","fractal_dimension_se","radius_worst","texture_worst","perimeter_worst","area_worst","smoothness_worst",
"compactness_worst","concavity_worst","concave points_worst","symmetry_worst","fractal_dimension_worst")
cont_df<-l[,names(l) %in% contVars]

## Scatter plot
pairs(cont_df)
library(corrplot)
corrplot(cor(cont_df), type = "full", "ellipse")

# 
ggplot(l, aes(radius_mean , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(texture_mean , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(perimeter_mean , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(area_mean , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(smoothness_mean , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(compactness_mean , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(concavity_mean , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(concave_points_mean , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(symmetry_mean , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(fractal_dimension_mean , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(radius_se , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(texture_se , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(perimeter_se , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(area_se , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(smoothness_se , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(compactness_se , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(concavity_se , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(concave_points_se , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(symmetry_se , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(fractal_dimension_se , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(radius_worst , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(texture_worst , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(perimeter_worst , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(area_worst , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(smoothness_worst , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(compactness_worst , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(concavity_worst , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(concave_points_worst , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(symmetry_worst , diagnosis)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(fractal_dimension_worst , diagnosis)) + geom_boxplot(fill = "steelblue")

### Data Modelling

# creating train and test data
str(cancerTrain)
train_proj <- cancerTrain[1:455,] # creating train data
test_proj <- cancerTrain[456:569,]# creating test data

# removing Loan ID from  train and test data
train_proj <- subset(train_proj,select = -c(id))
test_proj <- subset(test_proj,select = -c(id))

## Logistic Regression
str(train_proj)
logistic<- glm(diagnosis~radius_mean+texture_mean+perimeter_mean+area_mean+smoothness_mean+compactness_mean+concavity_mean+concave.points_mean+ 
              symmetry_mean+fractal_dimension_mean+radius_se+texture_se+perimeter_se+area_se+smoothness_se+compactness_se+concavity_se+      
              concave.points_se+symmetry_se+fractal_dimension_se+radius_worst+texture_worst+perimeter_worst+area_worst+smoothness_worst+
              compactness_worst+concavity_worst+concave.points_worst+symmetry_worst+fractal_dimension_worst, data = train_proj)
summary(logistic)

logistic1<- glm(diagnosis~radius_mean+texture_mean+perimeter_mean+area_mean+smoothness_mean+compactness_mean+concavity_mean+concave.points_mean+ 
                 symmetry_mean+fractal_dimension_mean+radius_se+texture_se+perimeter_se+area_se+smoothness_se+compactness_se+concavity_se+      
                 concave.points_se+symmetry_se+fractal_dimension_se+radius_worst+texture_worst+perimeter_worst+area_worst+smoothness_worst+
                 compactness_worst+concavity_worst+concave.points_worst+symmetry_worst+fractal_dimension_worst, data = test_proj)
summary(logistic1)

# prediction #   

predict<- predict(logistic, type = "response")
head(predict, 3)
train_proj$predict<- predict
train_projRound<- round(predict, digits = 0)
table(train_proj$diagnosis, predict >= 0.5)

confusionMatrix<- confusionMatrix(predict,train_proj$diagnosis)
confusionMatrix

predict1<- predict(logistic1, type = "response")
head(predict1, 3)
test_proj$predict1<- predict1
test_projRound<- round(predict1, digits = 0)
table(test_proj$diagnosis, predict1 <= 0.5)

confusionMatrix<- confusionMatrix(predictions,test_proj$diagnosis)
confusionMatrix(glm.predict, train_proj$diagnosis, positive = "Yes")

confusionMatrix

#ROC curve 
library(ROCR)
# need to create prediction object from ROCR
pr <- prediction(diagnosis$predict, train_proj$diagnosis)
pr <- train_proj$predict

# plotting ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# AUC value
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#............Random forest.........

set.seed(1)
library(randomForest)
a.cancerTrain<-randomForest(diagnosis~.,cancerTrain,
                                         subset = train_proj,mtry = 3,importance = TRUE)
dim(a.cancerTrain)
importance(a.cancerTrain)

varImpPlot(a.cancerTrain,col = 'blue',pch = 10, cex = 1.25)

a.cancerTrain

test.pred.rf<-predict(a.cancerTrain, newdata = cancerTrain[-train_proj,],type = 'class')
table(test.pred.rf,test_proj)


  
  