coeflmer<-function(mod,fixefrows=2:4){
  round(summary(mod)$coefficients[fixefrows,],3)
}
