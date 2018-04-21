rm(list=ls())
source('./permutation_matrix.R')
APO <- function(v,lambda1,lambda2){
v <- as.vector(v)
n <- length(v)
w <- lambda1+lambda2*(n-as.matrix(c(1:n)))
p_x <- permutation(v)
term1_u <- sin(p_x$x_tilda)
vec_1 <- as.vector(t(p_x$x_tilda)-w)
j <- length(vec_1)
term2_u <- numeric()
for(i in 1:j){
term2_u <- c(term2_u,max(vec_1[i],0))
}
u <- term1_u * t(term2_u)
X_star <- t(p_x$permutation)%*%t(u)
X_star
}
