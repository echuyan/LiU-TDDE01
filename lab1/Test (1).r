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

#install.packages("kknn")
#library(kknn)
#help("kknn")

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
#!!!!!!STUCK!!!!
#extracting row numbers where 8 is the actual digit in v65
rowsWith8FromTrain = rownames(train[train$V65 == 8,]) 

#find probabilities for each 8 in train model
#ProbTrain8 = modelTrain$prob%>%select(rownum:rowsWith8FromTrain,)
#colnames(modelTrain$prob)[9]="eight"
#ProbTrain8 = modelTrain$prob[modelTrain$prob.eight %in% rowsWith8FromTrain, ]   

#install.packages("dplyr")
