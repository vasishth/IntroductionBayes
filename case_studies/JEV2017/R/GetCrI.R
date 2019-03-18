getCrI <- function(res=res,param="theta"){
  res<-as.matrix(res)
  params<-dim(res)[2]
  thetacol<-which(colnames(res)==param)
  post_prob<-mean(res[,thetacol]>0)
  
  quants<-matrix(rep(NA,params*3),ncol=3)
  
  for(i in 1:params){
    quants[i,2]<-mean(res[,i])
    quants[i,c(1,3)]<-quantile(res[,i],probs=c(0.025,0.975))
  }
  list(post_prob,quants)
}
