setwd("C:/ISE 5984")
library(caret)
# The data is read in.
trainset <- read.table(file.path(getwd(), "zip.train.gz"))
testset <- read.table(file.path(getwd(), "zip.test.gz"))
## Filtering data set to desired variables 2's and 3's.
trainset <- trainset[trainset[,1] %in% c(2, 3),]
testset <- testset[testset[,1] %in% c(2, 3),]
pixl <- c("V1", "V3", "V5", "V7", "V15")
trainset <- trainset[,pixl]
testset <- testset[,pixl]
# Implementing linear regression.
lin.mod <- lm(trainset[,1]~ ., data=trainset[,-1])
weighted.ave <- predict(lin.mod, testset[,2:5])
predict.vals.lin <- ifelse(weighted.ave>2.5, 3, 2)
rate.error.lin <- mean(predict.vals.lin!=testset[,1])
# Implementing K-nearest neighbors algorithm.
require(class)
predict.vals.knn <- knn(trainset[,2:5], testset[,2:5], trainset[,1], k=5)
rate.error.knn <- mean(predict.vals.knn!=testset[,1])
print(rate.error.lin)
print(rate.error.knn)

