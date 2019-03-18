regionmeans<-function(dataset){
data.rs <- melt(dataset, 
                id=c("condition","roi","subject"), 
                measure=c("rt"),
                na.rm=TRUE)

#get mean rt and no. of data points for each region, for each subject/condition
data.id  <- data.frame(cast(data.rs,subject+condition+roi ~ ., 
                            function(x) c(rt=mean(x), N=length(x) ) ))

data.id$GM<-mean(data.id$rt)

#deviation from the grandmean after considering intra-subject variability
#removing between subject variance
data.id <- ddply(data.id, .(subject), 
                 transform, rt.w = rt - mean(rt) + GM)

temp<-melt(data.id, id.var=c("subject","condition","roi"), 
           measure.var="rt.w")

M.id.w <- cast(temp, condition+roi  ~ ., 
               function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )

## specific to Wu et al data: need to generalize this:
M.id.w$classifier<-ifelse(M.id.w$condition%in%c("a","c"),"NoCL","CL")
M.id.w$bei<-ifelse(M.id.w$condition%in%c("a","b"),"NoBEI","BEI")

M.id.w$M<-round(M.id.w$M)
M.id.w$SE<-round(M.id.w$SE)
M.id.w
}