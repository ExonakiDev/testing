library(randomForest)
require(caTools)
library(caret)

bcdata<-read.csv("bcdata.csv")
dim(bcdata)
names(bcdata)

summary(bcdata)
sapply(bcdata,class)
bcdata<- transform(bcdata,diagnosis=as.factor(diagnosis))
colSums(is.na(bcdata))

sample= sample.split(bcdata$diagnosis, SplitRatio = 0.75)
train=subset(bcdata, sample==TRUE)
test= subset(bcdata, sample==FALSE)

rf<-randomForest(diagnosis~.,data=train, mtry=4,ntree=500, importance=TRUE)
rf
varImpPlot(rf)

pred<- predict(rf, train, type="class")
table(pred, train$diagnosis)

validation<- predict(rf, test, type="class")
mean(validation==test$diagnosis)
table(validation, test$diagnosis)