plotresults<-function(dat,maintitle="Experiment 1",ylabel="-1000/RT"){
  pd<-position_dodge(0.3)
  
#  decorate<-theme(axis.text=element_text(size=14),
#                  axis.title=element_text(size=14,face="bold"))
  
  resultsplot<-ggplot(dat, aes(x=rois, 
                               y=estimate, shape=contrasts, 
                               group=contrasts)) +
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=.5, size=0.5, colour="black", position=pd) +
    labs(title=maintitle) +
    xlab("Region")+
    ylab(ylabel)+
    geom_hline(yintercept=0)+
    geom_point(position=pd, size=2.5)+
    theme_bw()+
    magnifytext()
  
  resultsplot
}