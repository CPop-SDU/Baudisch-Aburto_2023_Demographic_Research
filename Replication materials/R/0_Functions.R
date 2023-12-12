


#function for life expectancy from gompertz from Lenart & Missov
ex.gompertz <- function(A,B){
  ex <- (1/B)*exp(A/B)*expint_E1(A/B)
  ex
}

#function for e^dagger from Wrycza 2014
edx.gompertz <- function(A,B,ex){
  edx <- (1/B)*(1-A*ex)
  edx
}
