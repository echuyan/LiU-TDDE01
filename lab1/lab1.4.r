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

#install.packages("kknn")
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
#install.packages("Hmisc")
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


