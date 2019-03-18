# how to prepare data :
setwd("C:/Users/Farnoosh/Desktop")
d1<-read.table("Persiane1original.DAT")
colnames(d1)[1]<-("subject")
colnames(d1)[2]<- ("type")
colnames(d1)[2]<- ("item")
colnames(d1)[2]<-("type")
colnames(d1)[3]<-("item")
colnames(d1)[4]<-("condition")
colnames(d1)[5]<-("position")
colnames(d1)[6]<-("word")
colnames(d1)[7]<-("response")
colnames(d1)[8]<-("rt")
colnames(d1)[9]<-("nothing")
head(d1)
tail(d1)
d1

head(d1)

write.table(d1, "d1.DAT")


###### subseting the target sentences (excluding fillers & pratice items) :
data1<-subset(d1, type=="Persian")
data1
head(data1)
dim(data1)

#### how to add a column and insert a value in it 
data1$roi<-NA
data1$roi[data1$cond=="a" & data1$position==4] <- "crit"
data1$roi[data1$cond=="b" & data1$position==8]<- "crit"
data1$roi[data1$cond=="c" & data1$position==4]<- "crit"
data1$roi[data1$cond=="d" & data1$position==8]<- "crit"
data1

#### fix the exceptions 
data1$roi[data1$cond=="d" & data1$item==13 & data1$position==9] <- "crit"
data1$roi[data1$cond=="d" & data1$item==13 & data1$position==8] <- NA

data1$roi[data1$cond=="b" & data1$item==19 & data1$position==9] <- "crit"
data1$roi[data1$cond=="b" & data1$item==19 & data1$position==8] <- NA

data1$roi[data1$cond=="c" & data1$item==15 & data1$position==5] <- "crit"
data1$roi[data1$cond=="c" & data1$item==15 & data1$position==4] <- NA

data1$roi[data1$cond=="c" & data1$item==6 & data1$position==5] <- "crit"
data1$roi[data1$cond=="c" & data1$item==6 & data1$position==4] <- NA

data1$roi[data1$cond=="b" & data1$item==33 & data1$position==9] <- "crit"
data1$roi[data1$cond=="b" & data1$item==33 & data1$position==8] <- NA

data1$roi[data1$cond=="d" & data1$item==19 & data1$position==9] <- "crit"
data1$roi[data1$cond=="d" & data1$item==19 & data1$position==8] <- NA

data1$roi[data1$cond=="d" & data1$item==6 & data1$position==9] <- "crit"
data1$roi[data1$cond=="d" & data1$item==6 & data1$position==8] <- NA

data1$roi[data1$cond=="d" & data1$item==26 & data1$position==9] <- "crit"
data1$roi[data1$cond=="d" & data1$item==26 & data1$position==8] <- NA

data1

#### subsetting "predep" (before dependant noun)
data1$roi[data1$position==0] <- "predep"
head(data1)


#### subsetting "dep" (pre-verbal element / object)
data1$roi[data1$position==1] <- "dep"
head(data1)


#### subsetting "precrit" (intervening material)
data1$roi[data1$cond=="a" & data1$position==2] <- "precrit"
data1$roi[data1$cond=="a" & data1$position==3] <- "precrit"
data1

data1$roi[data1$cond=="b" & data1$position==2]<- "precrit"
data1$roi[data1$cond=="b" & data1$position==3]<- "precrit"
data1$roi[data1$cond=="b" & data1$position==4]<- "precrit"
data1$roi[data1$cond=="b" & data1$position==5]<- "precrit"
data1$roi[data1$cond=="b" & data1$position==6]<- "precrit"
data1$roi[data1$cond=="b" & data1$position==7]<- "precrit"
data1

data1$roi[data1$cond=="c" & data1$position==2]<- "precrit"
data1$roi[data1$cond=="c" & data1$position==3]<- "precrit"
data1

data1$roi[data1$cond=="d" & data1$position==2]<- "precrit"
data1$roi[data1$cond=="d" & data1$position==3]<- "precrit"
data1$roi[data1$cond=="d" & data1$position==4]<- "precrit"
data1$roi[data1$cond=="d" & data1$position==5]<- "precrit"
data1$roi[data1$cond=="d" & data1$position==6]<- "precrit"
data1$roi[data1$cond=="d" & data1$position==7]<- "precrit"
data1

#exceptions :

data1$roi[data1$cond=="d" & data1$item==13 & data1$position==8] <- "precrit"

data1$roi[data1$cond=="b" & data1$item==19 & data1$position==8] <- "precrit"

data1$roi[data1$cond=="c" & data1$item==15 & data1$position==4] <- "precrit"

data1$roi[data1$cond=="c" & data1$item==6 & data1$position==4] <- "precrit"

data1$roi[data1$cond=="b" & data1$item==33 & data1$position==8] <- "precrit"

data1$roi[data1$cond=="d" & data1$item==19 & data1$position==8] <- "precrit"

data1$roi[data1$cond=="d" & data1$item==6 & data1$position==8] <- "precrit"

data1$roi[data1$cond=="d" & data1$item==26 & data1$position==8] <- "precrit"

head(data1)
data1

#### subsetting "postcrit" (immediately after crit)

data1$roi[data1$cond=="a" & data1$position==5] <- "postcrit"
data1$roi[data1$cond=="b" & data1$position==9] <- "postcrit"
data1$roi[data1$cond=="c" & data1$position==5] <- "postcrit"
data1$roi[data1$cond=="d" & data1$position==9] <- "postcrit"

#exceptions :

data1$roi[data1$cond=="d" & data1$item==13 & data1$position==10] <- "postcrit"

data1$roi[data1$cond=="b" & data1$item==19 & data1$position==10] <- "postcrit"

data1$roi[data1$cond=="c" & data1$item==15 & data1$position==6] <- "postcrit"

data1$roi[data1$cond=="c" & data1$item==6 & data1$position==6] <- "postcrit"

data1$roi[data1$cond=="b" & data1$item==33 & data1$position==10] <- "postcrit"

data1$roi[data1$cond=="d" & data1$item==19 & data1$position==10] <- "postcrit"

data1$roi[data1$cond=="d" & data1$item==6 & data1$position==10] <- "postcrit"

data1$roi[data1$cond=="d" & data1$item==26 & data1$position==10] <- "postcrit"

data1
#### subsetting "post" (anything after postcrit)

data1$roi[data1$cond=="a" & data1$position==6] <- "post"
data1$roi[data1$cond=="b" & data1$position==10] <- "post"
data1$roi[data1$cond=="c" & data1$position==6] <- "post"
data1$roi[data1$cond=="d" & data1$position==10] <- "post"
#exceptions :
data1$roi[data1$cond=="d" & data1$item==13 & data1$position==11] <- "post"

data1$roi[data1$cond=="b" & data1$item==19 & data1$position==11] <- "post"

data1$roi[data1$cond=="c" & data1$item==15 & data1$position==7] <- "post"

data1$roi[data1$cond=="c" & data1$item==6 & data1$position==7] <- "post"

data1$roi[data1$cond=="b" & data1$item==33 & data1$position==11] <- "post"

data1$roi[data1$cond=="d" & data1$item==19 & data1$position==11] <- "post"

data1$roi[data1$cond=="d" & data1$item==6 & data1$position==11] <- "post"

data1$roi[data1$cond=="d" & data1$item==26 & data1$position==11] <- "post"



# all NA to post except for question mark :



for(j in 1:nrow(data1)){
  roi <- data1[j,]$roi
  if(is.na(roi) & data1[j,]$position!="?"){
    data1[j,]$roi <- "post"
  }
}

data1

head(data1)

write.table(data1, "data1.DAT")



#### omiting the unnecessary columns 

data2<-data1[,-2]
data3<-data2[,-5]
finalpersiane1<-data3[,-7]

finalpersiane1
head(finalpersiane1)

write.table(finalpersiane1, "finalpersiane1.DAT")
read.table("finalpersiane1.DAT")

#Michela : to remove fillers : data2<-data1[data1$cond= !"filler",]

data<- data[!data$cond == "filler",]

deleting the column called datafile :
  d2<-d1[,-2]
head(d2)

#deleting list :
d3<-d2[,-3]
head(d3)
#deleting sentence & question :
d4<-d3[,-19:-20]
head(d4)

# for "em2" we need three columns exactly name : response, trial, subject ... so I rename subj to subject
colnames(d4)[19]<-("subject")

head(d4)
data1<-d4
# delete the column (19) subject :
data2<-data1[,-19]

colnames(data2)

# changing colnames :

colnames(data2)[1]<-("subject")
colnames(data2)[5]<-("rid")
colnames(data2)[7]<-("fixationdur")

colnames(data2)

# sorting the data first by trial and second by subject and third by fixation duration :

data3 <- data2[c(2,1,7,5,3,4,6,8,9,10,11,12,13,14,15,16,17,18)]
colnames(data3)

data3<-sort(data3, first="trial", second="subject", third="fixationdur")

data3 = data3[order(data3$trial,data3$subject,data3$fixationdur),]

data3$trial
length(unique(data3$trial))
unique(data3$trial)

#Martijn :
str(data3)

head(data3[data3$trial=='.',])
dim(data3[data3$trial=='.',])
tail(data3[data3$trial=='.',])
dat3 = data3[data3$trail!='.',]
dim(data3)
dim(dat3)
str(data3$trial)
sum(data3$trial!='.')
sum(data3)$trial=='.')
sum(nrow(data3[data3$trial!='.',]))
nrow(data3[data3$trial!='.',])
dat3=data3[data3$trial!='.',]
dim(dat3)
str(dat3)
dat3=droplevels(dat3)
str(dat3)
dat3$trial=as.numeric(as.character(dat3$trial))
str(dat3)
head(dat3)
table(dat3$trial)
table(dat3$subject)
str(dat3)
table(dat3$id)
table(unique(dat3,[,c("subject","id")]))
table(unique(dat3,[,c("subject","trial")]))
dat3=dat3[order(dat3$trial, dat3$subject, dat3$fixationdur),]

