LogiSlope<-function(par=c(K=800, Td=15, N0=50, T_off=70, Bg=60)){
  K<-unname(par[1])
  Td<-unname(par[2])
  N0<-unname(par[3])
  T_off<-unname(par[4])
  Bg<-unname(par[5])
  r<-log(2)/Td
  slope<-(r*K)/4
  b<-(-(K*(1-(log(K/N0)/2))/2))
  intercept<-(b-(slope*T_off)-Bg)
  return(c(slope=slope, intercept=intercept, b=b))
}