rm(list=ls())
library("Matrix",lib.loc="")# pass path to the library
library("methods",lib.loc="")# pass path to the library
library("lattice",lib.loc="")# pass path to the library
library("spams",lib.loc="")# pass path to the library
library("pROC",lib.loc="")# pass path to the library
library("Metrics",lib.loc="")# pass path to the library
library("boot",lib.loc="")# pass path to the library

normalize = function(x){
# we use z-score normalization to standardize
y = (x-mean(x))/sd(x)
 return(y)
}

#here we create graph vector based on the original beta
groups <- function(beta){
values <- unique(beta)
indexList <- lapply(1:length(values),function(i) {
    which(beta==values[i])
})
group_vec <- numeric()
for(i in 1:length(indexList)){
for(j in 1:length(indexList[[i]])){
if(j!=1){
group_vec <- c(group_vec,c(indexList[[i]][1],indexList[[i]][j]))
}
}
}
return(group_vec)
}

mse1 <- function(beta_hat,X){
#select beta based on dataset used

#beta <- as.matrix(c(3,2,1.5,0,0,0,0,0)) # beta for synthetic-1
beta <- as.matrix(c(3,0,0,1.5,0,0,0,2)) # beta for syntetic-2
#beta <- as.matrix(c(rep(0,10),rep(2,10),rep(0,10),rep(2,10))) # beta for synthetic-3
term1 <- beta_hat-beta# here we get the difference between predicted beta and orginal beta
term2 <- cov(X)# covariance of feature matrix
term3 <- term1
mse_value <- t(term1)%*%term2%*%term3# final MSE calculation
return(mse_value)
}

samp_mse <- numeric()
# we generate mse for 50 different datasets and select median of them as final mse
for(iter in 1:50){
path <- paste("",iter,sep="")# pass path to data to be used
path <- paste(path,".csv",sep="")
data <- read.csv(path,header=TRUE,sep=",")

#select beta based on dataset used
#beta <- as.matrix(c(3,2,1.5,0,0,0,0,0))
beta <- as.matrix(c(3,0,0,1.5,0,0,0,2))
#beta <- as.matrix(c(rep(0,10),rep(2,10),rep(0,10),rep(2,10)))

x <-as.matrix(data[,-ncol(data)])#here features are extracted
centered_x <- matrix(0,nrow=nrow(x),ncol=ncol(x))
# here each column of the feature matrix is standardized
for(i in 1:ncol(x)){
centered_x[,i] <- normalize(x[,i])
}

y <- as.matrix(data[,ncol(data)])# here class lable is extracted from dataset
centered_y <- as.matrix(scale(y,center=TRUE))# here class is centered

graph <- groups(beta)# here we gererate graph by passing beta
W0 = matrix(c(0),nrow = ncol(x), ncol = ncol(y))# Initial estimation of beta

n <-  nrow(data)
folds <- split(sample(1:n),rep(1:5,length=n))#here we generate 5 fold train and test datasets
lambda <- 10^seq(0,-3,length=5)# here a lambda sequence is created
loss <- matrix(nrow=length(lambda)*length(lambda),ncol=5)# here a empty matrix is created to store loss values for each of the lambda in each fold

# in the below loop we iterate through folds and fit the model
for(i in 1:5){
l <- 1
# below two loops are used to iterate through lambda sequence and fit the model using each of the lambda combination
for(j in 1:length(lambda)){
for(k in 1:length(lambda)){
fit <- ncTLF(centered_x[-folds[[i]],],centered_y[-folds[[i]]],tp=graph,s1=lambda[j],s2=lambda[k],RaMaxIter=2000,Rtau=0.5,RmaxIter=200)# here we fit the model on train data
optim_info <- fit[[1]]# here we extract the beta_hat
loss[l,i] <- mse1(as.matrix(optim_info),as.matrix(centered_x[folds[[i]],]))# here we extract loss value for test data
l <- l+1
}
}
}

final_loss <- numeric()
# in below loop we calculate average mse for each of lambda combination
for(i in 1:nrow(loss)){
final_loss <- c(final_loss,sum(loss[i,])/5)
}
samp_mse <- c(samp_mse,min(final_loss))# here we select a minimum avg MSE among all the lambda combinations
}
# following funtion is used to get the standar deviation using bootstrap
boot_mse <- function(sample,indices){
sampled_mse <- sample[indices]
return (median(sampled_mse))
}
bootstrap <- boot(samp_mse,statistic=boot_mse,R=500)
sd(bootstrap$t[,1])
median(samp_mse)
