plotposteriors<-function(d=Match,CrI=CrI,start=3,title="Target Match"){
  d$Publication<-factor(d$Publication,
                        levels=d$Publication[order(d$Effect)])
  
  df <- data.frame(x = d$Publication,
                   y = d$Effect,
                   lower = d$Effect-2*d$SE,
                   upper = d$Effect+2*d$SE)
  
  df$x <- factor(df$x,levels=df$x[order(df$y)])
  
  ## add new column marking whether this is data or posterior:
  df$datapost <- factor("data")
  
  ## posterior of each study:
  dpost<-data.frame(x=d$Publication,
                    y=CrI[[2]][start:dim(CrI[[2]])[1],2], 
                    lower=CrI[[2]][start:dim(CrI[[2]])[1],1],
                    upper=CrI[[2]][start:dim(CrI[[2]])[1],3])
  
  ## reorder publications by effect magnitude
  dpost$x<-factor(dpost$x,levels=dpost$x[order(dpost$y)])
  
  dpost$datapost<-factor("posterior")
  
  ## combine data and posteriors:
  dataposterior<-rbind(d,dpost)
  
  #post<-CrI[[2]][2,]
  
  pd <- position_dodge(.5)
  pmatch<-ggplot(data=dataposterior,
                 aes(x=x,y=y,col=datapost,group=datapost))+
    geom_point(position=pd, size=3, aes(shape=datapost), fill="white") +
    geom_errorbar(data=dataposterior,aes(ymin=lower,
                                         ymax=upper,
                                         linetype=datapost), 
                  width=.1, position=pd)+
    scale_colour_manual(values=c("black","black"))+
    #  scale_shape_manual(values=c(1,2))+
    geom_hline(yintercept=0,linetype="dashed")+
    ylab("Observed effect (ms)")+
    xlab("Study")+ggtitle(title)+
    #theme(legend.title=element_blank())+
    ## don't display posterior of theta:
    #    annotate("pointrange", x = 67.2, y = post[2], ymin = post[1], ymax = post[3],colour = "red", size = .1)+
    #    annotation_custom(g, ymin=-100, ymax=-50, xmin=64, xmax=65) +
    coord_flip()  + theme_bw()+theme(legend.title=element_blank())+  scale_shape_manual(values=c(17,2)) 
  
  pmatch
}
