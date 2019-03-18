fitmodel<-function(d,
                   m="JAGSModels/RandomEffectsMetaAnalysisM1.jag",
                   track=c("theta","thetai","tau"),
                   chains=4,
                   adapt=20000,
                   iter=40000,
                   thin=20,
                   quiet=T){
  mod <- jags.model(file = m,
                    data = d,
                    n.chains = chains,
                    n.adapt =adapt, 
                    quiet=quiet)
  res <- coda.samples( mod,
                       var = track,
                       n.iter = iter,
                       thin = thin )
  res
}
