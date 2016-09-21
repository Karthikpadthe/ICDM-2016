rm(list=ls())
library("Matrix",lib.loc="")# pass path to package
library("methods",lib.loc="")# pass path to package
library("lattice",lib.loc="")# pass path to package
library('SLOPE',lib.loc="")# pass path to package
library("pROC",lib.loc="")# pass path to package
library("Metrics",lib.loc="")# pass path to package
library("boot",lib.loc="")# pass path to package

normalize = function(x){
# we use z-score normalization to standardize
y = (x-mean(x))/sd(x)
 return(y)
}

mse1 <- function(beta_hat,X){
#select beta based on dataset used
beta <- as.matrix(c(3,2,1.5,0,0,0,0,0)) # beta for synthetic-1
#beta <- as.matrix(c(3,0,0,1.5,0,0,0,2)) # beta for synthetic-2
#beta <- as.matrix(c(rep(0,10),rep(2,10),rep(0,10),rep(2,10))) # beta for synthetic-3
term1 <- beta_hat-beta# here we get the difference between predicted beta and orginal beta
term2 <- cov(X)# covariance of feature matrix
term3 <- term1
mse_value <- t(term1)%*%(term2%*%term3)# final MSE calculation
return(mse_value)
}

samp_mse <- numeric()
# we generate mse for 50 different datasets and select median of them as final mse
for(iter in 1:50){
path <- paste("",iter,sep="")# pass path to data to be used
path <- paste(path,".csv",sep="")
data <- read.csv(path,header=TRUE,sep=",")

x <-as.matrix(data[,-ncol(data)])#here features are extracted
centered_x <- matrix(0,nrow=nrow(x),ncol=ncol(x))
# here each column of the feature matrix is standardized
for(i in 1:ncol(x)){
centered_x[,i] <- normalize(x[,i])
}

y <- as.matrix(data[,ncol(data)])# here class lable is extracted from dataset
centered_y <- as.matrix(scale(y,center=TRUE))# here class is centered

n <-  nrow(data)
folds <- split(sample(1:n),rep(1:5,length=n))#here we generate 5 fold train and test datasets
loss <- numeric(5)# here a empty matrix is created to store loss values in each fold

# in the below loop we iterate through folds
for(i in 1:5){
fit <- SLOPE(x[-folds[[i]],],centered_y[-folds[[i]]],fdr=0.2,lambda='gaussian',normalize=TRUE,solver="default")# here we fit the model on train data
optim_info <- as.vector(fit$beta)# here we extract the beta_hat
loss[i] <- mse1(as.matrix(optim_info),as.matrix(x[folds[[i]],]))# here we extract loss value for test data
}

final_loss <- mean(loss)# here we calculate average mse
samp_mse <- c(samp_mse,min(final_loss))
}
# following funtion is used to get the standar deviation using bootstrap
boot_mse <- function(sample,indices){
sampled_mse <- sample[indices]
return (median(sampled_mse))
}
bootstrap <- boot(samp_mse,statistic=boot_mse,R=500)
sd(bootstrap$t[,1])
median(samp_mse)
