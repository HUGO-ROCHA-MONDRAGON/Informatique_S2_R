NS<- function(B0,B1,B2,L,t){
  term1 <- B1*((1-exp(-t/L))/(t/L))
  term2 <- B2*((1-exp(-t/L))/(t/L)-exp(-t/L))
  NS_eq <- B0 + term1 + term2
  return(NS_eq)
}

NSS <- function(B0,B1,B2,B3,L1,L2,t){
  term1 <- B1*((1-exp(-t/L1))/(t/L1))
  term2 <- B2*((1-exp(-t/L1))/(t/L1)-exp(-t/L1))
  term3 <- B3*((1-exp(-t/L2))/(t/L2)-exp(-t/L2))
  NS_eq2 <- B0 + term1 + term2 + term3
  return(NS_eq2)}