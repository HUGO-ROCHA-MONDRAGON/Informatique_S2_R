BS <- function(S,K,sigma,r,t,type = "call"){
  d1 <- ((log(S/K)+(r+((sigma^2)/2)*t))/sigma*sqrt(t))
  d2 <- d1 - sigma*sqrt(t)
  call <- S*pnorm(d1)-K*exp(-r*t)*pnorm(d2)
  put <- K*exp(-r*t)*pnorm(-d2) - S*pnorm(-d1)
  
  if (type == "call") {
    return(call)
  } else if (type == "put") {
    return(put)
  } else {
    stop("Type invalide. Choisissez 'call' ou 'put'.")
  }
  
}