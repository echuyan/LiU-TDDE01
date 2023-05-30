#setwd("/Users/eli/Desktop/Machine learning/Lab 1/")
optdigit=read.csv("optdigits.csv",header=FALSE)

optdigit$V65=as.factor(optdigit$V65)

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

matplot(size,cbind(valid$V65,modelValid$fitted.values),type="l",col=c("red","blue"),lty=c(1,1))


}

print(vectorTrainMCE)
print(vectorValidMCE)




#plot(K,vectorValidMCE,col="red",ylim=range(c(vectorValidMCE,vectorTrainMCE)))
#par(new=TRUE)
#points(K,vectorTrainMCE,col="blue")

#matplot(K,cbind(vectorValidMCE,vectorTrainMCE),type="l",col=c("red","blue"),lty=c(1,1))


