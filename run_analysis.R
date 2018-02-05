setwd("./data/UCI HAR Dataset")
library(dplyr)
library(reshape2)
library(tidyr)
library(readr)

# reading data
testf = list.files(path = "./test",pattern = ".*.txt")
trainf = list.files(path = "./train",pattern = ".*.txt")
test = lapply(testf, function(x) read.table(paste0("./test/",x), header=F)) 
train =lapply(trainf, function(x) read.table(paste0("./train/",x), header=F)) 

# combine the test with train data
datafr.test = do.call("cbind", test) 
datafr.train = do.call("cbind", train) 
mydf = rbind(datafr.test,datafr.train)

# reading features & labels, find filter index with features file
features <-  read.table("features.txt",header = F)
labels <- read.table("activity_labels.txt",header = F)
extract <- features[grep("mean\\()|std\\()",features$V2),]
extract$V1 <- paste0("V",extract$V1)
mydf <- data.frame(mydf[,extract$V1], "person"=mydf[,562],"activity"=mydf[,563])

# merging the data with label names and
mydf <- melt(mydf, id.vars = c("person","activity"))
mydf <- merge(mydf,labels,by.x = "activity",by.y = "V1")
mydf <- merge(mydf, extract,by.x = "variable",by.y = "V1")
mydf <- separate(mydf,V2.y,into = c("subject","variable","xyz"))
mydf <- mydf[,2:7]
colnames(mydf)[3] <- "activity"
mydf$activity <- tolower(mydf$activity)
mydf$activity <- gsub("_"," ",mydf$activity)
mydf %>% select(subject, variable, xyz, activity, person, value)
result <- mydf%>% group_by(subject,variable,activity) %>% summarise(average = mean(value)) %>% spread(variable, average)
write.table(result, file = "resultdata.txt",row.names = FALSE)
