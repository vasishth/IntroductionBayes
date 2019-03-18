
## load data and prepare:

## reading time data analysis:

## preliminary data processing:
data <- read.table("../../data/e1_en_spr_data.txt")
colnames(data) <- c("subject","expt","item","condition","position","word","rt")

data <- subset(data,expt=="gug")
data$expt <- factor(data$expt)

## make every position start with 1 instead of 0
data$position <- as.numeric(data$position)+1

## don't need this
#dataq <- read.table("engugspr1q.txt")
#colnames(dataq) <- c("subj","expt","item","condition","dummy","response","correct","RT")

library(reshape)

## computations cached:
d.rs <- melt(data, id=colnames(data),
                measure="rt", variable_name="times",
                na.rm=TRUE)

unique(subset(d.rs,position==1)$word)


## recode regions of interest:

## The painter who the film that the friend liked admired the poet.  
##  1    2      3   4   5   6     7   8     9     10      11  12
##    1         2     3     4       5 

## recode the regions of interest:
d.rs$roi <- ifelse(d.rs$position==2,1, # NP1
               ifelse(d.rs$position==3,2, # who
                 ifelse(d.rs$position%in%c(4,5),3, #NP2
                            ifelse(d.rs$position==6,4, # that
                                   ifelse(d.rs$position%in%c(7,8),5, # NP3
                                                 d.rs$position)))))

## NP3:
pos05data <- subset(d.rs,roi==5)

d.rs.NP3 <- melt(pos05data, id=colnames(pos05data)[c(1,2,3,4,5,6)],
                measure=c("rt"), variable_name="times",
                na.rm=FALSE)

sum <- rep(NA,dim(d.rs.NP3)[1])
detrt <- 0

for(i in 1:dim(d.rs.NP3)[1]){
  if(i%%2==0){## even row                               
     sum[i] <- detrt+d.rs.NP3[i,]$value}
  else {detrt <- d.rs.NP3[i,]$value} ## odd row
}

d.rs.NP3$sumrt <- sum
d.rs.NP3 <- subset(d.rs.NP3,word!="the")
d.rs.NP3$value <- d.rs.NP3$sumrt
d.rs.NP3 <- d.rs.NP3[,1:8]

d.rs.NP3$gram <- ifelse(d.rs.NP3$condition%in%c("a","b"),"gram","ungram")
d.rs.NP3$int <- ifelse(d.rs.NP3$condition%in%c("a","c"),"hi","lo")

## means and CIs for plotting
d.cast.NP3.rt.i   <- cast(d.rs.NP3, int ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="rt")

## V3:
pos09data <- subset(d.rs,position==9)

d.rs.V3 <- melt(pos09data, id=colnames(pos09data)[c(1,2,3,4,5,6)],
                measure=c("rt"), variable_name="times",
                na.rm=FALSE)

## sanity check
unique(d.rs.V3$word)

## code up factors:
d.rs.V3$gram <- ifelse(d.rs.V3$condition%in%c("a","b"),"gram","ungram")
d.rs.V3$int <- ifelse(d.rs.V3$condition%in%c("a","c"),"hi","lo")

## means and CIs for plotting
d.cast.V3.rt.g   <- cast(d.rs.V3, gram ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="rt")

d.cast.V3.rt.i   <- cast(d.rs.V3, int ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="rt")

## V2, relevant only for conditions a and b

data.ab <- subset(data,condition%in%c("a","b"))
data.cd <- subset(data,condition%in%c("c","d"))

pos10data <- subset(data.ab,position==10) ## V2 in a,b

d.rs.V2ab <- melt(pos10data, id=colnames(pos10data)[c(1:6)],
                measure=c("rt"), variable_name="times",
                na.rm=TRUE)

d.rs.V2ab$gram <- ifelse(d.rs.V2ab$condition%in%c("a","b"),"gram","ungram")

d.rs.V2ab$int <- ifelse(d.rs.V2ab$condition%in%c("a"),"hi",
                        ifelse(d.rs.V2ab$condition%in%c("b"),"lo",NA))


d.cast.V2ab.g   <- cast(d.rs.V2ab, gram ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))),
                        subset=times=="rt")

d.cast.V2ab.i   <- cast(d.rs.V2ab, int ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))),
                        subset=times=="rt")

## V1:

pos11data <- subset(data.ab,position==11)
pos10data <- subset(data.cd,position==10)
pos1011data <- rbind(pos10data,pos11data)

d.rs.V1 <- melt(pos1011data, id=colnames(pos1011data)[1:6],
                measure="rt", variable_name="times",
                na.rm=TRUE)

d.rs.V1$gram <- ifelse(d.rs.V1$condition%in%c("a","b"),"gram","ungram")
d.rs.V1$int <- ifelse(d.rs.V1$condition%in%c("a","c"),"hi","lo")

d.cast.V1.g  <- cast(d.rs.V1, gram ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="rt")

d.cast.V1.i  <- cast(d.rs.V1, int ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="rt")

## Post V1:

pos12data <- subset(data.ab,position==12)
pos11data <- subset(data.cd,position==11)
pos1112data <- rbind(pos12data,pos11data)

d.rs.postV1 <- melt(pos1112data, id=colnames(pos1112data)[1:6],
                measure="rt", variable_name="times",
                na.rm=TRUE)

d.rs.postV1$gram <- factor(ifelse(d.rs.postV1$condition%in%c("a","b"),"gram","ungram"))
d.rs.postV1$int <- factor(ifelse(d.rs.postV1$condition%in%c("a","c"),"hi","lo"))


d.cast.postV1.g   <- cast(d.rs.postV1, gram ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="rt")

d.cast.postV1.i   <- cast(d.rs.postV1, int ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="rt")

verbrts.g <- rbind(d.cast.V3.rt.g,
                 d.cast.V2ab.g,
                 c("ungram","NA","NA","NA"),
                 d.cast.V1.g,
                 d.cast.postV1.g)
verbrts.g$verb <- rep(c("V3","V2","V1","Post-V1"),each=2)
verbrts.g$region <- rep(1:4,each=2)
names(verbrts.g$region) <- "Region"
levels(verbrts.g$region) <- verbrts.g$verb
verbrts.g$CI.upper <- as.numeric(verbrts.g$CI.upper)
verbrts.g$CI.lower <- as.numeric(verbrts.g$CI.lower)
verbrts.g$M <- as.numeric(verbrts.g$M)

verbrts.i <- rbind(d.cast.NP3.rt.i,
                   d.cast.V3.rt.i,
                   d.cast.V2ab.i,
                   d.cast.V1.i,
                   d.cast.postV1.i)

verbrts.i$verb <- rep(c("NP3","V3","V2","V1","Post-V1"),each=2)
verbrts.i$region <- rep(1:5,each=2)
names(verbrts.i$region) <- "Region"
levels(verbrts.i$region) <- verbrts.i$verb
verbrts.i$CI.upper <- as.numeric(verbrts.i$CI.upper)
verbrts.i$CI.lower <- as.numeric(verbrts.i$CI.lower)
verbrts.i$M <- as.numeric(verbrts.i$M)

d.rs.V3$gram <- factor(d.rs.V3$gram) 
d.rs.V3$times <- factor(d.rs.V3$times) 

d.rs.NP3 <- data.frame(region="NP3",d.rs.NP3)
d.rs.V3 <- data.frame(region="V3",d.rs.V3)
d.rs.V2ab <- data.frame(region="V2",d.rs.V2ab)
d.rs.V1 <- data.frame(region="V1",d.rs.V1)
d.rs.postV1 <- data.frame(region="postV1",d.rs.postV1)

critdata <- rbind(d.rs.NP3,d.rs.V3,d.rs.V2ab,d.rs.V1,d.rs.postV1)

critdata$times <- factor("rt")

## end preliminaries
