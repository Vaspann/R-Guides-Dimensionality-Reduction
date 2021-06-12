#Using PCA on the wine data set
#Allows interpretation of many variables using a 2 dimensional biplot
#Can also be used to develop prediction models

#Load the packages

library(kohonen)
library(caTools)
library(psych)
library(ggplot2)
library(nnet)


#Load the data
data("wines")
wines <- as.data.frame(wines)
wines <- cbind(wines, vintages)
str(wines)
head(wines)

#train and test set
set.seed(123)
splitcc <- sample.split(wines, SplitRatio = 0.8)
train <- subset(wines, splitcc == "TRUE")
test <- subset(wines, splitcc == "FALSE")

#Predictors should be independent to avoid Multicollinearity
pairs.panels(train, pch = 21, bg = train$vintages)



# Apply pca ---------------------------------------------------------------

#scale the data with scale = TRUE
summary(wines)
p <- prcomp(train[,-14], scale = TRUE)
p
str(p)
p$x

#Plot
#PC1 and PC2 explain a significant proportion of the variance
summary(p)
plot(p, type = "l")
biplot(p, scale = 0)


#append pc1 and pc2 to train
train2 <- cbind(train, p$x[,1:2])
head(train2)


#plot with ggplot
# Use 95% confidence elipse
ggplot(train2, aes(PC1, PC2, col = vintages, fill = vintages)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5, level = 0.95) +
  geom_point(shape = 25)


# Predict -----------------------------------------------------------------

pred_train <- predict(p, train)
pred_train <- data.frame(pred_train, train[14])
pred_test <- predict(p, test)
pred_test <- data.frame(pred_test, test[14])


#Multinominal logistic regreesion with the first two principal components

pred_train$vintages <- relevel(pred_train$vintages, ref = "Barolo")
train_model <- multinom(vintages~PC1+PC2, data = pred_train)
summary(train_model)

#training accuracy 
P <- predict(train_model, pred_train)
matrix_1 <- table(P, pred_train$vintages)
matrix_1

#testing accuracy
P2 <- predict(train_model, pred_test)
matrix_2 <- table(P2, pred_test$vintages)
matrix_2
