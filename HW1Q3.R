

# We set a working directory to read in files
setwd("C:/ISE 5984")

# Libraries are read in 
library(data.table)
library(tidyverse)
library(caret)
library(leaps)
library(glmnet)
library(pls)

# The prostate cancer data is read in
data = read.csv("prostate_data.csv", na.string = " ")
head(data)
data = as.data.table(data)



# The k fold function splits data into training and test sets as well as calculating error for BSS and Ridge

k.fold = function(formula = lpsa ~ .,data,name,bestlambda = 0, k){
  library(glmnet)
  library (pls)
  set.seed(1)
  length = nrow(data)
  size.fold = floor(length / k)
  index = seq(1,length)
  trunc_index = index
  acc_error=0
  for (i in 1:k){
    # Get length(data)/10 random number in index
    sample = sample(trunc_index,size.fold,replace = F)
    # test set, which is the data whose index obtained from sampling
    test = data[sample,]
    # training set, which is the remaining data
    train = data[which(! index %in% sample),]
  }
  x = model.matrix(formula, train)[, -1]
  y = train$lpsa
  x.val = model.matrix(formula, test)[, -1]
    if (name == "glm"){
      fit = glm(formula, data=train)
      fit.pred = predict(fit, s = bestlambda, newdata = test)
    } else if (name == "ridge"){
     fit = glmnet(x,y,alpha = 0,lambda = bestlambda)
     fit.pred = predict(fit, s = bestlambda, newx = x.val)
    }
} 



par(mfrow = c(1, 1))



model.BSS = regsubsets(lpsa ~ ., data) 
model.BSS.summary =  summary(model.BSS)

# Plotted Adjusted R^2 against number of variables and select the optimal number of variables
plot(model.BSS.summary$adjr2, xlab = "Number of Variables", ylab = "R-Squared Adjusted", type = "l")
optimal.var = which.max(model.BSS.summary$adjr2)
points(optimal.var, model.BSS.summary$adjr2[optimal.var], col = "red", cex = 2, pch = 20)

plot(model.BSS, scale = "adjr2")

# Get coefficients of Best Subset model for a given size
coefi = coef(model.BSS, optimal.var)

# The Best Subset model is fitted with selected variables
model.BSS = lm(lpsa ~ .-gleason,data)
summary(model.BSS)

# Calculate error for Best Subset 
error.BSS = k.fold(lpsa ~ .-gleason,data=data,name = "glm",k=10)
error.BSS


grids = 10^seq(10, -2, length = 100)

# Fit ridge regression for each lambda on the grid
x = model.matrix(lpsa ~ ., data)[, -1]
y = data$lpsa
ridge.mod = glmnet(x, y, alpha = 0, lambda = grids)

plot(ridge.mod, xvar = "lambda")

# Use cross-validation to estimate test MSE from training data
set.seed(1)
cv.out = cv.glmnet(x, y, alpha = 0)
plot(cv.out)

bestlambda.ridge = cv.out$lambda.min
out = glmnet(x,data$lpsa,alpha = 0)
predict(out , type ="coefficients",s= bestlambda.ridge)[1:9,]

# Calculate the error for ridge regression
err.ridge = k.fold(lpsa ~ .,data,name = "ridge",bestlambda = bestlambda.ridge,k=10)
err.ridge

