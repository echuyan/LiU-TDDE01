#----------------------------lab1. assignment2. linear and ridge regression---------------------------

#######################################################Task1##########################################
#if you need to install packages and plugin libraries
#install.packages("tidyr")
#install.packages("dplyr")
#library(tidyr)
#library(dplyr)
#install.packages("glmnet")
#library(glmnet)
#install.packages("psych")
#library(psych)

#loading the dataset
parkinson=read.csv("parkinsons.csv")
#REMOVE UNNECESSARY COLUMNS FIRST
parkinson$subject.=c()
parkinson$sex=c()
parkinson$test_time=c()
parkinson$age=c()
parkinson$total_UPDRS=c()

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

#######################################################Task2##########################################
#Compute a linear regression model from the training data, estimate training and test MSE and comment on which variables contribute significantly 
fit = lm(motor_UPDRS~.-1 , data=trainS)
sum=summary(fit)
#calculate MSE for train dataset
MSEtrain=mean(sum$residuals^2)
#run model on test dataset
fitTest=predict(fit,testS,interval = "prediction")
#calculate MSE for test dataset
MSEtest=mean((testS$motor_UPDRS-fitTest)^2)

#comment on which variables contribute significantly to the model.
summary(fit)


#######################################################Task3##########################################
#Implement 4 following functions

#3a) Log-likelihood function that for a given parameter vector 𝜽 and dispersion 𝜎 computes the log-likelihood function log 𝑃(𝑇|𝜽, 𝜎) 
#for the stated model and the training data

#taking parameters from the stated model
vectorTheta = fit$coefficients
names(vectorTheta) <- NULL
dispersionSigma = summary(fit)$sigma

#loglik function
logLikelihood<-function(theta,sigma){
  n<-nrow(trainS)
  actualValue<-trainS$motor_UPDRS #y in the formula. 
  result=-n/2*log(2*pi*sigma^2)-1/(2*sigma^2)*(sum((actualValue-as.matrix(trainS[,-1])%*%theta)^2))
  return(result)
}
print(logLikelihood(vectorTheta,dispersionSigma))
#logLik(fit) - just to compare

########################################
#3b) Ridge function that for given vector 𝜽𝜽, scalar 𝜎𝜎 and scalar 𝜆𝜆 uses function from 3a and 
#adds up a Ridge penalty 𝜆‖𝜽𝜽‖2 to the minus loglikelihood

#function
myridge<-function(theta,sigma,lambda){
   ridgePenalty=lambda*sum(theta^2)
   result =  -logLikelihood(theta,sigma )+ ridgePenalty
   return (result)
}
###for test purposes:
#mylambda=1
#print(myridge(vectorTheta,dispersionSigma,mylambda))

#3bb) to help to split parameters when later we call this function from optim
splitparamsridge <- function(param,lambda) {
  theta <- c(param[1:16])
  sigma <- c(param[17])
  return(myridge(theta,sigma,lambda))
}

########################################
# 3c) RidgeOpt function that depends on scalar 𝜆 , uses function from 3b
#and function optim() with method=”BFGS” to find the optimal 𝜽 and 𝜎 for the given 𝜆.
#initial values
par1 <- rep(c(0),each=16)
sigmaPar=1
par1 <- append(par1,sigmaPar)
#function
myRidgeOpt=function(lambdaIn) {
  result<- optim(par1,fn=splitparamsridge,lambda=lambdaIn,method="BFGS")
  return (result)
}

###for test purposes:
#optimal=myRidgeOpt(mylambda)
#print(optimal)

########################################
#3d) Df function that for a given scalar 𝜆 computes the degrees of freedom
#of the Ridge model based on the training data

#function
DF=function(lambdaIn) {
  # Compute hat-matrix and degrees of freedom   
  ld <- lambdaIn * diag(ncol(trainS[,-1])) 
  trainmatrix<-data.matrix(trainS[,-1]) 
  H <- trainmatrix %*% solve((t(trainmatrix) %*% trainmatrix + ld)) %*% t(trainmatrix)
  df <- tr(H)
  return(df)
}
###for test purposes:
#lambdaIn=1
#print(DF(lambdaIn))


#######################################################Task4##########################################
#By using function RidgeOpt, compute optimal 𝜽 parameters for 𝜆 = 1, 𝜆 =  100 and𝜆 = 1000. 
#Use the estimated parameters to predict the motor_UPDRS values for training and test data and 
#report the training and test MSE values. Which penalty parameter is most appropriate among the
#selected ones? Compute and compare the degrees of freedom of these models and make appropriate conclusions.

#compute
myRidgeOpt(1)$par
myRidgeOpt(100)$par
myRidgeOpt(1000)$par

#Predicted motor_UPDRS for train set with lambda=1
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
#When we have lambda is 100 error is lowest 
