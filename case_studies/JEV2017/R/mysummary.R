mysummary<-function(res,rows=1:3){
  print(summary(res)[1]$statistics[rows,1:2]);
  print(gelman.diag(res)$psrf[rows,])
}
