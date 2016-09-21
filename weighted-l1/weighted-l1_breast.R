rm(list=ls())
library("Matrix",lib.loc="")# pass path to the package
library("methods",lib.loc="")# pass path to the package
library("lattice",lib.loc="")# pass path to the package
library("spams",lib.loc="")# pass path to the package
library('SLOPE',lib.loc="")# pass path to the package
library("pROC",lib.loc="")# pass path to the package
library("proxy",lib.loc="")# pass path to the package
library("Metrics",lib.loc="")# pass path to the package
library("e1071",lib.loc="")# pass path to the package
library("ggplot2",lib.loc="")# pass path to the package
library("caret",lib.loc="")# pass path to the package

normalize = function(x){
# we use z-score normalization to standardize
y = (x-mean(x))/sd(x)
 return(y)
}

data <- read.csv("",header=TRUE,sep=",")# pass path the dataset

x <-as.matrix(data[,-ncol(data)])#here feature matrix is extracted
y <- as.matrix(data[,ncol(data)])# here class lable is extracted

# we reduce class imbalance by duplicating positive class twice
positive <- which(y==1)
pos_x <- as.matrix(x[positive,])
x <- rbind(x,pos_x,pos_x)
y <- rbind(y,as.matrix(y[positive]),as.matrix(y[positive]))

# we shuffle the data instances to reduce class imbalance while selecting folds
data_samp <- sample(1:nrow(x))
x <- as.matrix(x[data_samp,])
y <- as.matrix(y[data_samp])

centered_x <- matrix(0,nrow=nrow(x),ncol=ncol(x))
# here each column of the feature matrix is standardized
for(i in 1:ncol(x)){
centered_x[,i] <- normalize(x[,i])
}

centered_y <- scale(y,center=TRUE)# here class label is centered

n <- nrow(x)
folds <- split(sample(1:n),rep(1:5,length=n))#here we generate 5 fold train and test datasets
auc <- numeric()

for(i in 1:5){
fit_new <- SLOPE(centered_x[-folds[[i]],],centered_y[-folds[[i]]],fdr=0.5,lambda="gaussian",normalize=TRUE,solver="default")# here we fit the model on train data
beta_hat <- as.vector(fit_new$beta)# here estimated beta is extracted
y_hat <- centered_x[folds[[i]],]%*%beta_hat# here y_hat is obtained on test data
pred <- sign(y_hat)# here final predictions are made based on the sign of y_hat value
auc <- c(auc,roc(as.vector(y[folds[[i]]]),as.vector(pred))$auc)# here we calculate AUC using predictions and store it in a vector
}
mean(auc)# here we calculate average AUC for all the folds
sd(auc)# here we calculate standard deviation AUC for all the folds
