setwd("C:/Users/Farnoosh/Desktop/Experiment one/design method/linger/Linger/Experiments/Persiane2")
folder<-"Results"

for(subj in 1:45){
  s1 <-paste("Results", "/", subj, ".DAT", sep="")
    if(!file.exists(s1)){
      missing <- c(missing, s1)
     print(paste("Missing file:", s1, sep=" "))
    next
  
}
  d <- read.table(s1, sep=" ", header=FALSE)
  d$subj <- subj
  trialinfo <- rbind(trialinfo, d)
  filelist <- c(filelist, s1)
}

head(trialinfo)
summary(trialinfo)
filelist
missing
write.table(trialinfo, "trialinfo.DAT")

head("trialinfo")
summary("trialinfo")


