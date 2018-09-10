#own load data from kaggle
unzip("all.zip")

#read data
train<-read.csv("train.csv")
dim(train)
#in the train data, first col. is label
test<-read.csv("test.csv")
dim(train)
sample_submission<-read.csv("sample_submission.csv")

