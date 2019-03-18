
## load data and prepare:

## reading time data analysis:

## preliminary data processing:
data <- read.table("../../data/e1_en_spr_data.txt")
colnames(data) <- c("subject","expt","item","condition","position","word","rt")

data <- subset(data,expt=="gug")
data$expt <- factor(data$expt)


if(0){ ## for emilsu08 exam
data$wordlength <- nchar(as.character(data$word))

plot(rt~wordlength,data)
summary(lm(rt~wordlength,data))

data.emil <- data[,c(1,3,6,7,8)]

write(t(data.emil),ncolumns=5,file="wordlengthrt.txt")

}


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


## V3:
pos09data <- subset(d.rs,position==9)

d.rs.V3 <- melt(pos09data, id=colnames(pos09data)[c(1,2,3,4,5,6)],
                measure=c("rt"), variable_name="times",
                na.rm=FALSE)

## sanity check
unique(d.rs.V3$word)

## code up factors:
d.rs.V3$gram <- ifelse(d.rs.V3$condition%in%c("a","b"),"gram","ungram")

## means and CIs for plotting
d.cast.V3.rt   <- cast(d.rs.V3, gram ~ .,
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

d.cast.V2ab   <- cast(d.rs.V2ab, gram ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="rt")


## V1:

pos11data <- subset(data.ab,position==11)
pos10data <- subset(data.cd,position==10)
pos1011data <- rbind(pos10data,pos11data)

d.rs.V1 <- melt(pos1011data, id=colnames(pos1011data)[1:6],
                measure="rt", variable_name="times",
                na.rm=TRUE)

d.rs.V1$gram <- ifelse(d.rs.V1$condition%in%c("a","b"),"gram","ungram")

d.cast.V1   <- cast(d.rs.V1, gram ~ .,
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

d.cast.postV1   <- cast(d.rs.postV1, gram ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="rt")


verbrts <- rbind(d.cast.V3.rt,d.cast.V2ab,c("ungram","NA","NA","NA"),d.cast.V1,d.cast.postV1)

verbrts$verb <- rep(c("V3","V2","V1","Post-V1"),each=2)
verbrts$region <- rep(1:4,each=2)
names(verbrts$region) <- "Region"
levels(verbrts$region) <- verbrts$verb

verbrts$CI.upper <- as.numeric(verbrts$CI.upper)
verbrts$CI.lower <- as.numeric(verbrts$CI.lower)
verbrts$M <- as.numeric(verbrts$M)


d.rs.V3 <- subset(d.rs.V3,times=="rt")

d.rs.V3$gram <- factor(d.rs.V3$gram) 
d.rs.V3$times <- factor(d.rs.V3$times) 

d.rs.V3 <- data.frame(verb="V3",d.rs.V3)
d.rs.V2ab <- data.frame(verb="V2",d.rs.V2ab)
d.rs.V1 <- data.frame(verb="V1",d.rs.V1)
d.rs.postV1 <- data.frame(verb="postV1",d.rs.postV1)

critdata <- rbind(d.rs.V3,d.rs.V2ab,d.rs.V1,d.rs.postV1)

critdata$times <- factor("rt")

## end preliminaries
