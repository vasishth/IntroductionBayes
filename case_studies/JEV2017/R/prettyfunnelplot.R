## Adapted from: https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/
prettyfunnelplot<-function(d=dat,estimate,se,title){
#  estimate<-estimate_match
#  se<-se_match
  max_SE<-max(d$SE)
  se.seq<-seq(0, max_SE, 0.01)
  ll95 <- estimate-(1.96*(se.seq))
  ul95 <- estimate+(1.96*(se.seq))
  meanll95 <- estimate-(1.96*se)
  meanul95 <- estimate+(1.96*se)
  dfCI <- data.frame(ll95, ul95, se.seq, estimate, meanll95, meanul95)
  
  #APA-format theme
  apatheme<-theme_bw()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(),
          text=element_text(family='Times'),
          legend.position='none')
  
  fp <- 
    ggplot(aes(x = SE, y = Effect), data = d) +
    geom_point(size=4, shape=21,colour="black", fill="black") +
    xlab('Standard Error') + ylab('Effect (ms)')+
    geom_line(aes(x = se.seq, y = ll95), linetype = 'dotted', data = dfCI) +
    geom_line(aes(x = se.seq, y = ul95), linetype = 'dotted', data = dfCI) +
    geom_segment(aes(x = min(se.seq), y = meanll95, xend = max(se.seq), yend = meanll95), linetype='dotted', data=dfCI) +
    geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq), yend = meanul95), linetype='dotted', data=dfCI) +
    scale_x_reverse( lim=c(max(d$SE),0))+
    scale_y_continuous(breaks=seq(-100,100,20))+
    coord_flip()+
    ggtitle(title)+
    apatheme
  
  fp
}  