regionplot<-function(data,
                        maintitle="Experiment 1",k=1,
                        x.lab="position",
                        ymin=250,
                        ymax=1200,
                        y.lab="reading time (ms)",d=0.4,legendpos=c(0,.5)){
  ggplot(data,aes(x=roi,y=M,
                  shape=condition,
                  group=condition))+
    geom_line(aes(linetype=condition),
              size = 1,
              position=position_dodge(d)) +    
    geom_point(position=position_dodge(d),size=4)+
    scale_shape_manual(values=c(0,0,1,1))+
    scale_linetype_manual(values=c("solid","dashed", "solid","dashed"))+
    theme_bw()+xlab("Region")+
    geom_errorbar(aes(ymin=M-2*SE,ymax=M+2*SE),width=.1,size=k,
                  position=position_dodge(d))+
    ylab("Reading time (ms)")+
    ylim(ymin,ymax)+
    ggtitle(maintitle)+
    theme(legend.justification=c(0,0), legend.position=legendpos)+
    magnifytext()
}  
