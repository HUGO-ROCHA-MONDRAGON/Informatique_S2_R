invert_bs <- function(sigma,P,t,k,S,r){
  #Mettre sigma en premier, on va l'utiliser pour calculer le niveau de volatilité implicite
  spread = P - BS(S,K,sigma,r,t,type = "call");
  return(spread^2)}