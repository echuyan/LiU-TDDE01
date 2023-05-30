#----------------------------lab1. assignment1. question1---------------------------
#loading the dataset
optdigit=read.csv("optdigits.csv",header=FALSE)

#factorizing the last column
optdigit$V65=as.factor(optdigit$V65)

#----------------------------lab1. assignment1. question2---------------------------
#choosing training, validation and test sets
n=dim(optdigit)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.50))
train=optdigit[id,]

id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=optdigit[id2,]

id3=setdiff(id1,id2)
test=optdigit[id3,]

install.packages("kknn")
library(kknn)

#building kknn model for training and test data
#model = kknn(V65~., train, valid, na.action = na.omit(),k = 30, kernel = "rectangular", scale=TRUE)
modelTrain = kknn(V65~., train, train, na.action = na.omit(),k = 30, kernel = "rectangular", scale=TRUE)
modelTest = kknn(V65~., train, test, na.action = na.omit(),k = 30, kernel = "rectangular", scale=TRUE)
#kknn.dist(train, valid, k = 30, distance = 2)     

#calculating confusion matrices
CMTrain=table(ytrainpred = modelTrain$fitted.values,ytraintrue = train$V65)
CMTest=table(ytrainpred = modelTest$fitted.values,ytraintrue = test$V65)

#calculating miclassification errors
MCETrain=mean(modelTrain$fitted.values != train$V65) 
MCETest=mean(modelTest$fitted.values != test$V65)


#----------------------------lab1. assignment1. question3---------------------------
#and comment on whether these cases seem to be hard or easy to recognize visually
#temporary dataframe for V65 column in initial training dataset and probabilities for digit 8
df <- data.frame(train$V65,modelTrain$prob[,9])
#by looking at his new dataframe we defined that:
#rows 520, 431, 1294 - hardest to predict
#rows 129, 195 - easiest to predict
#reshape:
row520<- c(train[520,-65])
row520df <- data.frame(row520[1:8])
row520df[nrow(row520df)+1,] <- row520[9:16]
row520df[nrow(row520df)+1,] <- row520[17:24]
row520df[nrow(row520df)+1,] <- row520[25:32]
row520df[nrow(row520df)+1,] <- row520[33:40]
row520df[nrow(row520df)+1,] <- row520[41:48]
row520df[nrow(row520df)+1,] <- row520[49:56]
row520df[nrow(row520df)+1,] <- row520[57:64]

#build heatmap and comment
heatmap(as.matrix(row520df),Rowv = NA,Colv = NA)

#install.packages("dplyr")

