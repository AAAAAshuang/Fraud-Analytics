library(stringr)
mydata<-read.csv('NY property data.csv')
mydata$ownerlower<-tolower(mydata$OWNER)
mydata2<-mydata[c('RECORD','OWNER','ownerlower')]
mydata2$test<-str_detect(mydata2$ownerlower,"[j]{1}[ohns]{2,5}.*[s]{1}[mith]{4}")
mydata2$test2<-str_detect(mydata2$ownerlower,'[s]{1}[mith]{4}.*[j]{1}[ohn]{2,5}')

mydata2$test3<-mydata2$test+mydata2$test2
end<-mydata2[mydata2$test3!=0,]


write.csv(end,'fuzzymatch.csv')
