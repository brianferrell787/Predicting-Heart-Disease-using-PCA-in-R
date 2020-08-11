library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(lubridate)
library(tidytext)
library("RColorBrewer")
library(randomForest)
library(tictoc)
library(e1071)
library(ggpubr)
library(rpart)
library(e1071)
# Load the data. 
heart_Data = read.csv("heart.csv", header=TRUE)
heart_Data %>% head()

# Disease distribution for age. 
# 0 - no disease
# 1 - disease
heart_Data %>% group_by(age, condition) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(age, count,   fill = as.factor(condition)), stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Age") + labs(fill = "Condition")

# condition sex wise

options(repr.plot.width = 20, repr.plot.height = 8) 

heart_Data %>% ggballoonplot(x = "age", y = "sex",
                                     size = "chol", size.range = c(5, 30), fill = "condition",show.label = FALSE,
                                     ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "C") + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) +
  ggtitle("Age vs. Sex Map") + labs(fill = "Condition")



# condition sex wise
heart_Data %>% ggballoonplot(x = "age", y = "cp",
                                     size = "chol", size.range = c(5, 30), fill = "sex",show.label = FALSE,
                                     ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "C") + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) +
  ggtitle("Age vs. Chest Pain Map") + labs(fill = "sex")


# Divide into train and validation dataset
set.seed(1)
test_index <- createDataPartition(y = heart_Data$condition, times = 1, p = 0.2, list= FALSE)
train_set <- heart_Data[-test_index, ]
validation <- heart_Data[test_index, ]

# Converting the dependent variables to factors
train_set$condition <- as.factor(train_set$condition)
validation$condition <- as.factor(validation$condition)

# Find the optimal parameter using cross validation

tune.out <- tune(svm, condition~., data = train_set, kernel = "linear",preProcess = c("center","scale"), tunecontrol = tune.control(cross = 10),
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000)))
summary(tune.out)
summary(tune.out$best.model)

# Fit the model
ypred <- predict(tune.out$best.model, validation)
table(ypred, y=validation$condition)


#Confusion Matrix
CM_svm <- confusionMatrix(ypred, validation$condition)

CM_svm





# conduct PCA on training dataset
pca <- prcomp(heart_Data[,0:13], retx=TRUE, center=TRUE, scale=TRUE)
pca


pca$rotation[,1:5] # rotation/loadings matrix
pca$x[,1:5]  # scores
pca

summary_pca_model = summary(pca)

plot(pca,type = "l", main ="Scree plot for PCA")
plot(summary_pca_model$importance[3,],type="l")

# Determine the number of PCs
summary(pca)
pca$sdev
myvar <- pca$sdev^2 # variance explained by each PC
myvar/sum(myvar) # % variance explained by each PC
barplot(myvar[0:5])

plot(pca)
biplot(pca, scale = 0)


data2 <- cbind(heart_Data[,0:13], pca$x)
library(ggplot2)
data2
ggplot(data2, aes(PC1, PC2, col = heart_Data$condition, fill = heart_Data$condition)) +
  geom_point(shape = 21, col = "black")

cor(heart_Data[, -14], data2[, 14:18])




# Prediction with Principal Components
trg <- predict(pca, train_set)
trg <- data.frame(trg, train_set[14])
tst <- predict(pca, validation)
tst <- data.frame(tst, validation[14])
trg
tst

tune.out2 <- tune(svm, condition~PC1+PC2+PC3+PC4+PC5, data = trg, kernel = "linear",tunecontrol = tune.control(cross = 10),
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000)))
summary(tune.out2)
summary(tune.out2$best.model)

# Confusion Matrix & Misclassification Error - training
p <- predict(tune.out$best.model, trg)
p
tab <- table(p, trg$condition)
tab
1 - sum(diag(tab))/sum(tab)

# Confusion Matrix & Misclassification Error - testing
p1 <- predict(tune.out$best.model, tst)
tab1 <- table(p1, tst$condition)
tab1
1 - sum(diag(tab1))/sum(tab1)
#Confusion Matrix
CM_pca <- confusionMatrix(p1, tst$condition)

CM_pca
#plotting PCA SVM
svmfit = svm(condition~PC1+PC2+PC3+PC4+PC5, data=trg,gamma=1, kernel="linear", cost=0.1)
summary(svmfit)
plot(svmfit, data=trg, PC2~PC1)

