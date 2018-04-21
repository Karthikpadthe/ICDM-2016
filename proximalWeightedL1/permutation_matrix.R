rm(list=ls())
permutation <- function(x){
absolute_x <- abs(x)
x1 <- sort(absolute_x,index.return=TRUE,decreasing=TRUE)
ordr_indx <- x1$ix
p <- matrix(0,nrow=length(ordr_indx),ncol=length(ordr_indx))
for(i in 1:length(ordr_indx)){
p[ordr_indx[i],i] <- 1
}
p_x <- list(x1$x,p)
names(p_x) <- c('x_tilda','permutation')
p_x[[1]] <- x%*%p
return(p_x)
}
