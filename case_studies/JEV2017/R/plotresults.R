plotresults<-function(dat,ylabel="ms",maintitle="Posterior predictive distributions vs observed means"){
  pd <- position_dodge(0.5)
  resultsplot<-ggplot(dat, aes(x=study, 
                               y=mn,color=source,shape=source))+
    geom_errorbar(aes(ymin=lower, ymax=upper),width=0.1,position=pd)+
    ggtitle(maintitle)+
    xlab("Study")+
    ylab(ylabel)+
    geom_hline(yintercept=0)+
    geom_point(size=6,position=pd)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    magnifytext()
  resultsplot
}
