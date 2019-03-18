setwd("~/Documents/Projects/StatisticalSignificanceFilter/StatisticalSignificanceFilter/VMJG2018/data")

f <- read.table('freqs.txt', header=T)
sum(is.na(f$type_freq))
print(f[is.na(f$type_freq),])
# unclear why these words are not in the corpus; they are not extremely infrequent

# replace NANs with minimum
f$type_freq <- ifelse(is.na(f$type_freq), min(f$type_freq, na.rm=T), f$type_freq)
sum(is.na(f$type_freq))
f$type_logFreq <- log(f$type_freq)
f<-f[, c('word_position', 'item','word', 'type_logFreq')]
colnames(f)<-c('roi', 'itemid', 'word', 'type_logFreq')


# load data exp2
e2<-read.table('E2ETlevykellerExp1.txt', header=T)
e2<-subset(e2, condition=='f')
# add freq and word length
e2 <- merge(e2, f) # rows with 'empty' rois, i.e., rois with a higher index than the number of words of this sentence will be removed
e2$word<-as.character(e2$word)
e2$len <- nchar(e2$word)
# center len and freq
e2$type_logFreq_z <- (e2$type_logFreq-mean(e2$type_logFreq))/sd(e2$type_logFreq)
e2$len_z <- (e2$len-mean(e2$len))/sd(e2$len)
write.table(e2, file='Exp2fillersFreq.txt', col.names = TRUE, row.names = FALSE)



# load data exp4
e4<-read.table('E4ETlevykellerExp2.txt', header=T)
e4<-subset(e4, condition=='f')
# add freq and word length
e4 <- merge(e4, f) # rows with 'empty' rois, i.e., rois with a higher index than the number of words of this sentence will be removed
e4$word<-as.character(e4$word)
e4$len <- nchar(e4$word)
# center len and freq
e4$type_logFreq_z <- (e4$type_logFreq-mean(e4$type_logFreq))/sd(e4$type_logFreq)
e4$len_z <- (e4$len-mean(e4$len))/sd(e4$len)
write.table(e4, file='Exp4fillersFreq.txt', col.names = TRUE, row.names = FALSE)

# load data exp6
e6<-read.table('E6ETlevykellerExp12.txt', header=T)
e6<-subset(e6, condition=='f')
# add freq and word length
e6 <- merge(e6, f) # rows with 'empty' rois, i.e., rois with a higher index than the number of words of this sentence will be removed
e6$word<-as.character(e6$word)
e6$len <- nchar(e6$word)
# center len and freq
e6$type_logFreq_z <- (e6$type_logFreq-mean(e6$type_logFreq))/sd(e6$type_logFreq)
e6$len_z <- (e6$len-mean(e6$len))/sd(e6$len)
write.table(e6, file='Exp6fillersFreq.txt', col.names = TRUE, row.names = FALSE)

