magnifytext<-function(){
  theme(plot.title = element_text(lineheight=.8, size=18,face="bold"))+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"))+
  theme(legend.text = element_text(colour="black", size = 18, face = "bold"))+
  theme(legend.title = element_text(colour="black", size = 18, face = "bold"))
}