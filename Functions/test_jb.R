JB<- function(x){
  n<- length(x)
  skew<- mean(x - mean(x)^3)/(sd(x)^3) #ou sum(x)/n
  kurt<- mean(x - mean(x)^4)/(sd(x)^4)
  JB<-n/6 *(skew^2 +(kurt-3)^2/4)
  param=2
  cval<- pchisq(JB, df=param)
  pval<- 1 - cval
  structure(list(statistic =JB , C.value=cval, p.value=pval))
}