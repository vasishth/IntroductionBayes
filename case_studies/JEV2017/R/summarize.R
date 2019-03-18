summarize<-function(d,rows=1:2){
summaryres<-data.frame(summary(d)[2]$quantiles[rows,c(1,3,5)])
probs<-rep(NA,max(rows))
for(i in rows){
  probs[i]<-mean(as.matrix(d)[,i]>0)
}      
summaryres<-cbind(summaryres,summary(d)[1]$statistics[rows,1],probs)
colnames(summaryres)<-c("lower","median","upper","mean","prob(b>0)")
summaryres[,c(4,1,3,5)]
}