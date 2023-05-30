#----------------------------lab1. assignment2. linear and ridge regression---------------------------
#setwd("/Users/eli/Desktop/Machine learning/Lab 1/")
#loading the dataset
parkinson=read.csv("parkinsons.csv")
#REMOVE UNNECESSARY COLUMNS FIRST
parkinson$subject.=c()
parkinson$sex=c()
parkinson$test_time=c()
parkinson$age=c()
parkinson$total_UPDRS=c()
#parkinson$Jitter...=c()

#choosing training and test sets
set.seed(12345)
n=nrow(parkinson)
id=sample(1:n, floor(n*0.6))
train=parkinson[id,]
test=parkinson[-id,]


#scaling 
library(caret)
scaler=preProcess(train)
trainS=predict(scaler,train)
testS=predict(scaler,test)


#Compute a linear regression model from the training data, estimate training
#and test MSE and comment on which variables contribute significantly to the model
#no intercept is needed in the modelling


fit = lm(motor_UPDRS~.-1 , data=trainS)

sum=summary(fit)
#calculate MSE for train dataset
MSEtrain=mean(sum$residuals^2)
#run model on test dataset
fitTest=predict(fit,testS,interval = "prediction")
#calculate MSE for test dataset
MSEtest=mean((testS$motor_UPDRS-fitTest)^2)

#comment on which variables contribute significantly to the model.
# Shimmer.DDA
# Shimmer.APQ3
# we scaled the data => resulting coefficients can be compared and we can compare absolute values

#??????question?????????? (significant contribution) - should some other variables be picked? How to define a threshold?
#answer : look at p-values and ***
#CHECK IN GOOGLE
summary(fit)


#Implement 4 following functions

#3a) Log-likelihood function that for a given parameter vector 𝜽 and
#dispersion 𝜎 computes the log-likelihood function log 𝑃(𝑇|𝜽, 𝜎) for
#the stated model and the training data
#Going by that motor_UPDRS is a normal distribution as mentioned in 1.
#Formula log-likelihood F(𝜽)=log(L(𝜽))=sum(over i=1 to n) log(fi(yi|𝜽))

#??????question??????????: is the set of 𝜽we are trying to optimize equal to the vector of coefficients in LR model   - YES
########################################
#install.packages("tidyr")
#install.packages("dplyr")
#library(tidyr)
#library(dplyr)

vectorTheta = fit$coefficients
names(vectorTheta) <- NULL
dispersionSigma = summary(fit)$sigma
logLikelihood<-function(theta,sigma){
  #population size n
  
  #n<-length(theta)
  n<-nrow(trainS)
  #print(n)
  actualValue<-trainS$motor_UPDRS #y in the formula. 
  #print(actualValue)
  #print(length(parkinson$motor_UPDRS))
  #ERROR!
  #Expected value, estimated by using the average value.
  #ev=mean(theta) 
  #changed it to transpose of the in parametr theta to match p44 book. 

  #predictedTheta=theta
  #predictedTheta=vectorTheta
  #print(ev)
  #Done here because when calculations were done in "sum" in the formula, errors appeared.
  #library(dplyr)
  x=(predictedTheta-actualValue)^2 #is vector of
  #lastTermForFormula=sum( (as.matrix(trainS[,-1]) %*% as.matrix(predictedTheta) - actualValue)^2 )
  #print("printing x")
  #print(x)
  #ERROR!
 
  #Formula
  #loglikelihood = (-n*1/2*log(2*pi*sigma^2))-(1/(2*sigma^2))*sum((data_Y - ((data_X) %*% theta))^2)
  #result=((-1*n/2)*log(2*pi,base=exp(1))-(n/2)*log(sigma^2,base=exp(1)) -1/(2*sigma^2)*sum(x, na.rm=FALSE))
  #result=((-1*n/2)*log(2*pi,base=exp(1))-(n/2)*log(sigma^2,base=exp(1))-1/(2*sigma^2)*sum(x, na.rm=FALSE))
  #result=((-n/2)*log(2*pi*sigma^2)-1/(2*sigma^2)*lastTermForFormula)
  print(theta)
  result=-n/2*log(2*pi*sigma^2)-1/(2*sigma^2)*(sum((actualValue-as.matrix(trainS[,-1])%*%theta)^2))
 
  #return (result)
  return(result)
}
print(logLikelihood(vectorTheta,dispersionSigma))
logLik(fit)

########################################
#3b) Ridge function that for given vector 𝜽𝜽, scalar 𝜎𝜎 and scalar 𝜆𝜆 uses function from 3a and adds up a Ridge penalty 𝜆‖𝜽𝜽‖2 to the minus loglikelihood
mylambda=1

myridge<-function(theta,sigma,lambda){
  
 # print(theta)
  #print(sigma)
  #print(lambda)
  ridgePenalty=lambda*sum(theta^2)
  
  #print(ridgePenalty)
 # print(logLikelihood(theta,sigma ))
  print(theta)
  result =  -logLikelihood(theta,sigma )+ ridgePenalty
  
 # print(result)
  return (result)
  
}
print(myridge(vectorTheta,dispersionSigma,mylambda))

#3bb) to help to split parameters
splitparamsridge <- function(param,lambda) {
  theta <- c(param[1:16])
  sigma <- c(param[17])
  return(myridge(theta,sigma,lambda))
}
########################################
# 3c) RidgeOpt function that depends on scalar 𝜆 , uses function from 3b
#and function optim() with method=”BFGS” to find the optimal 𝜽 and 𝜎 for the given 𝜆.
#ANSWER: initiate with 0 and then compare the result of our custom prediction with true values
par1 <- rep(c(0),each=16)
sigmaPar=1
par1 <- append(par1,sigmaPar)

myRidgeOpt=function(lambdaIn) {
  
  
  
  
  result<- optim(par1,fn=splitparamsridge,lambda=lambdaIn,method="BFGS")
  
  #WE SHOULD OPTIMIZE THEM TOGETHER
 
  return (result)
}

optimal=myRidgeOpt(mylambda)
print(optimal)

########################################
#3d) Df function that for a given scalar 𝜆 computes the degrees of freedom
#of the Ridge model based on the training data
#install.packages("glmnet")
library(glmnet)
#install.packages("psych")
library(psych)

lambdaIn=1
DF=function(lambdaIn) {
  
  # Compute hat-matrix and degrees of freedom   
  ld <- lambdaIn * diag(ncol(trainS[,-1])) 
  trainmatrix<-data.matrix(trainS[,-1]) 
  H <- trainmatrix %*% solve((t(trainmatrix) %*% trainmatrix + ld)) %*% t(trainmatrix)
  #library("psych")
  df <- tr(H)
  return(df)

}

print(DF(lambdaIn))


#----------------------------4. -------
#By using function RidgeOpt, compute optimal 𝜽𝜽 parameters for 𝜆𝜆 = 1, 𝜆𝜆 =
#  100 and 𝜆𝜆 = 1000. Use the estimated parameters to predict the
#motor_UPDRS values for training and test data and report the training and
#test MSE values. Which penalty parameter is most appropriate among the
#selected ones? Compute and compare the degrees of freedom of these models
#and make appropriate conclusions.

#compute
myRidgeOpt(1)$par
myRidgeOpt(100)$par
myRidgeOpt(1000)$par

#Predicted motor_UPDRS for train set with lamda=1
data.matrix(trainS[,2:17])%*%myRidgeOpt(1)$par[1:16]
data.matrix(trainS[,2:17])%*%myRidgeOpt(100)$par[1:16]
data.matrix(trainS[,2:17])%*%myRidgeOpt(1000)$par[1:16]

#MSE calculation train set
MSEtrainLambda1 <- mean((data.matrix(trainS[,2:17])%*%myRidgeOpt(1)$par[1:16]-trainS$motor_UPDRS)^2)
MSEtrainLambda100 <- mean((data.matrix(trainS[,2:17])%*%myRidgeOpt(100)$par[1:16]-trainS$motor_UPDRS)^2)
MSEtrainLambda1000 <- mean((data.matrix(trainS[,2:17])%*%myRidgeOpt(1000)$par[1:16]-trainS$motor_UPDRS)^2)

#MSE calculation for test
MSEtestLambda1 <- mean((data.matrix(testS[,2:17])%*%myRidgeOpt(1)$par[1:16]-testS$motor_UPDRS)^2)
MSEtestLambda100 <- mean((data.matrix(testS[,2:17])%*%myRidgeOpt(100)$par[1:16]-testS$motor_UPDRS)^2)
MSEtestLambda1000 <- mean((data.matrix(testS[,2:17])%*%myRidgeOpt(1000)$par[1:16]-testS$motor_UPDRS)^2)

#Pick penalty value lambda 100 it gives the smallest error

#Compute and compare degrees of freedom
DF(1)
DF(100)
DF(1000)

#higher lambda more complex
#higher lambda means fewer independent variables, fewer variables that can vary in value <-
#more degrees of freedom better approximation. 

#When we have lambda is 100 error is lowest 

#We want low degrees of freedom and the penalty factor lambda high but with higher lambda but 
#also low ammounts of errors and after a 100 lambda the errors went up again.  