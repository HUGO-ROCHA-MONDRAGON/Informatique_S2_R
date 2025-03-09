poiss.lik <- function(theta,y){
  y=as.matrix(y)
  n<-nrow(y)
  logl<-sum(y)*log(theta)-n*theta
  return (-logl)
}