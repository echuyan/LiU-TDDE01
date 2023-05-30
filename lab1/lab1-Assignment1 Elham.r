
#----------------------------lab1. assignment1. question1---------------------------
#loading the dataset
optdigit=read.csv("optdigits.csv",header=FALSE)

#factorizing the last column
optdigit$V65=as.factor(optdigit$V65)

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

#----------------------------lab1. assignment1. question2---------------------------
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
#matrix1294 <- matrix((train[1294, -65]), nrow = 8, ncol = 8)

#reshape row1294 with the probability of ::::::::::::::
# As we can see in the plot it is not easy to recognize it as 8 
row1294<- c(train[1294,-65])
row1294df <- data.frame(row1294[1:8])
row1294df[nrow(row1294df)+1,] <- row1294[9:16]
row1294df[nrow(row1294df)+1,] <- row1294[17:24]
row1294df[nrow(row1294df)+1,] <- row1294[25:32]
row1294df[nrow(row1294df)+1,] <- row1294[33:40]
row1294df[nrow(row1294df)+1,] <- row1294[41:48]
row1294df[nrow(row1294df)+1,] <- row1294[49:56]
row1294df[nrow(row1294df)+1,] <- row1294[57:64]
#build heatmap
heatmap(as.matrix(row1294df),Rowv = NA,Colv = NA)


#reshape row520 with the probability of 0.10000000:
# As we can see in the plot it is not easy to recognize it as 8 
row520<- c(train[520,-65])
row520df <- data.frame(row520[1:8])
row520df[nrow(row520df)+1,] <- row520[9:16]
row520df[nrow(row520df)+1,] <- row520[17:24]
row520df[nrow(row520df)+1,] <- row520[25:32]
row520df[nrow(row520df)+1,] <- row520[33:40]
row520df[nrow(row520df)+1,] <- row520[41:48]
row520df[nrow(row520df)+1,] <- row520[49:56]
row520df[nrow(row520df)+1,] <- row520[57:64]
#build heatmap
heatmap(as.matrix(row520df),Rowv = NA,Colv = NA)


#reshape row431 with the probability of 0.13333333:
row431<- c(train[431,-65])
row431df <- data.frame(row431[1:8])
row431df[nrow(row431df)+1,] <- row431[9:16]
row431df[nrow(row431df)+1,] <- row431[17:24]
row431df[nrow(row431df)+1,] <- row431[25:32]
row431df[nrow(row431df)+1,] <- row431[33:40]
row431df[nrow(row431df)+1,] <- row431[41:48]
row431df[nrow(row431df)+1,] <- row431[49:56]
row431df[nrow(row431df)+1,] <- row431[57:64]
#build heatmap
heatmap(as.matrix(row431df),Rowv = NA,Colv = NA)


#reshape row129 with probability of 1.00000000:
row129<- c(train[129,-65])
row129df <- data.frame(row129[1:8])
row129df[nrow(row129df)+1,] <- row129[9:16]
row129df[nrow(row129df)+1,] <- row129[17:24]
row129df[nrow(row129df)+1,] <- row129[25:32]
row129df[nrow(row129df)+1,] <- row129[33:40]
row129df[nrow(row129df)+1,] <- row129[41:48]
row129df[nrow(row129df)+1,] <- row129[49:56]
row129df[nrow(row129df)+1,] <- row129[57:64]
#build heatmap and comment
heatmap(as.matrix(row129df),Rowv = NA,Colv = NA)



#reshape row195 with the probability of 1.00000000:
row195<- c(train[195,-65])
row195df <- data.frame(row195[1:8])
row195df[nrow(row195df)+1,] <- row195[9:16]
row195df[nrow(row195df)+1,] <- row195[17:24]
row195df[nrow(row195df)+1,] <- row195[25:32]
row195df[nrow(row195df)+1,] <- row195[33:40]
row195df[nrow(row195df)+1,] <- row195[41:48]
row195df[nrow(row195df)+1,] <- row195[49:56]
row195df[nrow(row195df)+1,] <- row195[57:64]

#build heatmap and comment
heatmap(as.matrix(row195df),Rowv = NA,Colv = NA)

install.packages("dplyr")



#----------------------------lab1. assignment1. question4---------------------------

#loading the dataset
optdigit=read.csv("optdigits.csv",header=FALSE)

#factorizing the last column
optdigit$V65=as.factor(optdigit$V65)

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
#help("kknn")

#calculating models for training and validation sets with respect to different K and saving MCEs to respectful vectors
vectorTrainMCE=c()
vectorValidMCE=c()

i=0
K <- c(1:30)
size <- c(1:nrow(valid))

for (i in 1:30) {
  
modelTrain = kknn(V65~., train, train, na.action = na.omit(),k = i, kernel = "rectangular", scale=TRUE)
modelValid = kknn(V65~., train, valid, na.action = na.omit(),k = i, kernel = "rectangular", scale=TRUE)
    

CMTrain=table(ytrainpred = modelTrain$fitted.values,ytraintrue = train$V65)
CMValid=table(ytrainpred = modelValid$fitted.values,ytraintrue = valid$V65)

MCETrain=mean(modelTrain$fitted.values != train$V65) 
MCEValid=mean(modelValid$fitted.values != valid$V65)

vectorTrainMCE=append(vectorTrainMCE,MCETrain)
vectorValidMCE=append(vectorValidMCE,MCEValid)


}

#building a plot of MCEs

matplot(K,cbind(vectorValidMCE,vectorTrainMCE),type="l",col=c("red","blue"),lty=c(1,1))
legend(15,0.01,legend=c("Validation MCE","Training MCE"),col=c("red","blue"),pch=c(16,1)) 
install.packages("Hmisc")
library(Hmisc)
minor.tick(nx = 5,  tick.ratio = 1)


#!!! Find optimal K!!!!
#it is 3 - where the MCE for validation data is minimal

#estimate the test error for the model having the optimal K, compare it with the training and validation
#errors and make necessary conclusions about the model quality!!!!!
modelTest = kknn(V65~., train, test, na.action = na.omit(),k = 3, kernel = "rectangular", scale=TRUE)
CMTest=table(ytrainpred = modelTest$fitted.values,ytraintrue = test$V65)
MCETest=mean(modelTest$fitted.values != test$V65) 
matpoints(5,MCETest,col="green")


#----------------------------lab1. assignment1. question5---------------------------
#5 calculating cross-entropy for validation data
#plot the dependance
#What is the optimal 𝐾value here? Assuming that response has
#multinomial distribution, why might the cross-entropy be a more suitable choice
#of the error function than the misclassification error for this problem?
#Eli 

#CrossEntropyValid = 0
#CrossEntropyValidResult=c()
#j=0
#for (j in 1:30) {
 # modelValid1 = kknn(V65~., train, valid, na.action = na.omit(),k =j , kernel = "rectangular", scale=TRUE)
  #for (m in 1:length(valid$V65))
  {
   # CrossEntropyValid = CrossEntropyValid + ((as.numeric(valid$V65[m]))*log2(as.numeric(modelValid1$fitted.values[m])+(10^-15)))
 #   CrossEntropyValid = -CrossEntropyValid - ((as.numeric(valid$V65[m]))*log2(as.numeric(modelValid1$prob[m])+(10^-15)))

  }
    
 #   CrossEntropyValidResult=append(CrossEntropyValidResult,CrossEntropyValid)
  #print(CrossEntropyValid)
}
#print( CrossEntropyValidResult)
#Eli End



#introduce vector with our classes
vectorValidCrossEntropy=c()
C <- train$V65
C<-unique(C)
C<-sort(C)

#calculate CE
i=0
m=0
j=1
CrossEntropyValid = 0
for (j in 1:30) {
  
  modelValid1 = kknn(V65~., train, valid, na.action = na.omit(),k = j, kernel = "rectangular", scale=TRUE)
  
  CrossEntropyValid = 0
  for (i in 1:length(valid$V65)){
    for (m in 1:10){
      CrossEntropyValid = CrossEntropyValid + as.numeric(as.numeric(valid$V65[i])==as.numeric(C[m])) * log(as.double(modelValid1$prob[i,m])+10^-5,base=exp(1))
    }
  }
  CrossEntropyValid=-1*CrossEntropyValid/length(valid$V65)
  vectorValidCrossEntropy=append(vectorValidCrossEntropy,CrossEntropyValid)
}

print(vectorValidCrossEntropy)
matplot(K,cbind(vectorValidMCE,vectorTrainMCE,vectorValidCrossEntropy),type="l",col=c("red","blue","black"),lty=c(1,1))
matpoints(5,MCETest,col="green")
legend(15,0.01,legend=c("Validation MCE","Training MCE"),col=c("red","blue"),pch=c(16,1))
minor.tick(nx = 5,  tick.ratio = 1)
