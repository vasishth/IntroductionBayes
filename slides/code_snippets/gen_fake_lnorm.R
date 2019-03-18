library(MASS)
gen_fake_lnorm <- function(nitem=16,nsubj=42,
                           alpha=NULL,beta=NULL,
                           Sigma_u=NULL,Sigma_w=NULL,sigma_e=NULL){
  ## prepare data frame for two condition in a latin square design:
  g1<-data.frame(item=1:nitem,
                 cond=rep(c("objgap","subjgap"),nitem/2))
  g2<-data.frame(item=1:nitem,
                 cond=rep(c("objgap","subjgap"),nitem/2))
  
  ## assemble data frame in long format:
  gp1<-g1[rep(seq_len(nrow(g1)),
              nsubj/2),]
  gp2<-g2[rep(seq_len(nrow(g2)),
              nsubj/2),]
  
  fakedat<-rbind(gp1,gp2)
  
  ## add subjects:
  fakedat$subj<-rep(1:nsubj,each=nitem)
  fakedat<-fakedat[,c(3,1,2)]  
  fakedat$so<-ifelse(fakedat$cond=="objgap",1,-1)
  ## subject random effects:
  u<-mvrnorm(n=length(unique(fakedat$subj)),
             mu=c(0,0),Sigma=Sigma_u)
  ## item random effects
  w<-mvrnorm(n=length(unique(fakedat$item)),
             mu=c(0,0),Sigma=Sigma_w)
  
  ## generate data row by row:
  N<-dim(fakedat)[1]
  rt<-rep(NA,N)
  for(i in 1:N){
    rt[i] <- rlnorm(1,alpha +
                      u[fakedat[i,]$subj,1] +
                      w[fakedat[i,]$item,1] +
                      (beta+u[fakedat[i,]$subj,2]+
                         w[fakedat[i,]$item,2])*fakedat$so[i],
                    sigma_e)}
  fakedat$rt<-rt
  fakedat$subj<-factor(fakedat$subj); fakedat$item<-factor(fakedat$item)
  fakedat}