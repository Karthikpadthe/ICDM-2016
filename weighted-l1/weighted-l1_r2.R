rm(list=ls())
library("Matrix",lib.loc="")# pass path to the package
library("methods",lib.loc="")# pass path to the package
library("lattice",lib.loc="")# pass path to the package
library("spams",lib.loc="")# pass path to the package
library('SLOPE',lib.loc="")# pass path to the package
library("pROC",lib.loc="")# pass path to the package
library("Metrics",lib.loc="")# pass path to the package
library("boot",lib.loc="")# pass path to the package

normalize = function(x){
# we use z-score normalization to standardize
y = (x-mean(x))/sd(x)
 return(y)
}

r2_alt <- function(y_hat,y){
ss_r <- (sum((as.vector(y_hat)-mean(y)) ^2))# here we calculate explained sum of squares
ss_t <- (sum((as.vector(y)-mean(y))^2))# here total sum of squares is calculated
r_2 <- ss_r/ss_t# r-squared
return (r_2)
}

data <- read.csv("",header=TRUE,sep=",")# pass path to the dataset

x <-as.matrix(data[,-ncol(data)])#here feature matrix is extracted
centered_x <- matrix(0,nrow=nrow(x),ncol=ncol(x))
# here each column of the feature matrix is standardized
for(i in 1:ncol(x)){
centered_x[,i] <- normalize(x[,i])
}

y <- as.matrix(data[,ncol(data)])#  here class lable is extracted from dataset
centered_y <- as.matrix(scale(y,center=TRUE))# here class is centered

n <-  nrow(data)
folds <- split(sample(1:n),rep(1:5,length=n))#here we generate 5 fold train and test datasets
r2_all <- numeric()# empty vector to store r-squared for each fold

# in the below loop i iterate through folds
for(i in 1:5){
fit <- SLOPE(centered_x[-folds[[i]],],centered_y[-folds[[i]]],fdr=0.2,lambda='gaussian',normalize=FALSE,solver="default")# here we fit the model on train data
optim_info <- as.vector(fit$beta)# here we extract the beta_hat
y_test_hat <- as.matrix(x[folds[[i]],])%*%optim_info# here we make predictions on test data using the beta_hat from model
r2_all <- c(r2_all,r2_alt(y_test_hat,y[folds[[i]]]))# here r-squared is calculated and stored in vector
}
mean(r2_all)# here average r-squared for all folds is calculated
