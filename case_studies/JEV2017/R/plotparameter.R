plotparameter<-function(res,col=2,xlabel='Interference effect size (ms)',
                        ylabel='Density',yloc=0.01,
                        title='Interference effect \n (Target Match)'){
  post<-data.frame(post=as.matrix(res)[,col])
  postquantiles<-quantile(post$post,prob=c(0.025,0.975))
  postplot<-ggplot(post, aes(x=post)) +
    geom_histogram(aes(y = ..density..),binwidth=2,color="black",fill="white")+ 
    #  geom_density(fill="orange", alpha = 0.2) +
    theme_bw() +
    xlab(xlabel) +
    ylab(ylabel) +
    geom_vline(xintercept=0,linetype="dashed")+
    geom_segment(aes(x = postquantiles[1], y = yloc, 
                     xend = postquantiles[2], yend = yloc),
                 linetype="solid")+
    ggtitle(title)
  
  postplot
}