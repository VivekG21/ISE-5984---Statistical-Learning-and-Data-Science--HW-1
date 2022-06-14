setwd("C:/ISE 5984")
library(data.table)

# Download Data
d = read.csv("hmeq.csv", na.string = " ")
d = as.data.table(d)
dim(d)

# Remove cases with missing data
dc = d[complete.cases(d),]
dim(dc)
names(dc)
head(dc)

# Remove outliers
d1 = dc[(dc$BAD)==1,]
dim(d1)
d0 = dc[(dc$BAD)==0,]
head(d0)
dim(d0)

# Compute Mahalanobis distance for each group
mdist <- function(x){
  t <- as.matrix(x)
  p <- dim(t)[2]
  m <- mapply(t,2,mean)
  s <- var(t)
  mahalanobis(t,m,s)
}
source("mdist.R")
dataf = subset(d0, select = -c(1,5,6))
head(dataf)
md0 = mahalanobis(dataf,colMeans(dataf), cov(dataf))
head(md0)
tail(md0)
datag = subset(d1, select = -c(1,5,6))
head(datag)
md1 = mahalanobis(datag,colMeans(datag), cov(datag))

# Remove outliers for each group of calculated Mahalanobis distance
c = qchisq(0.99, df=10)
print(c)
x0= d0[md0< c,]
dim(x0)

x1 = d1[md1<c,]
dim(x1)

# Combine the two groups of data and write them in a CSV file
x = rbind(x0,x1)
dim(x)
write.table(x,file="hmeq1.csv",sep = ",",row.names=F)
