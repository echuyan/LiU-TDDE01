# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

library(kernlab)
set.seed(1234567890)

data(spam)
#random order????
foo <- sample(nrow(spam))
spam <- spam[foo,]
#scaling
spam[,-58]<-scale(spam[,-58])
#splitting
tr <- spam[1:3000, ] #train
va <- spam[3001:3800, ] #valid
trva <- spam[1:3800, ] # train and valid
te <- spam[3801:4601, ] #test

by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  #calculating svm with different regularization terms
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  #making a prediction
  mailtype <- predict(filter,va[,-58])
  #confusion matrix
  t <- table(mailtype,va[,58])
  #storing misclassification into the vector
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}
#we get the C from err_va which gives the lowest MCR to use it in the filters further

#Smaller data sets than trva. And no usage of test
filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0

#Smaller training dataset and no usage of validation dataset
filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

#filter returned to user. Why? Largest usage of available data while still keeping some seperate for testing. 
filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2

#Training dataset contained test dataset
filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?
#POSSIBLE ANSWER: filter 2 because obtained on largerst dataset and tested on unseen data
#!!!!QUESTION!!! Is it correct?

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?
#POSSIBLEANSWER: we reject err0 
#Take into account: unseen data used, unseen data for C (reg term), size of the error and size of the dataset => 
# => err1 or err 2. But err1 is obtained on a smaller dataset (tr) and is a bit bigger than err2. Possibly err2
#!!!!QUESTION

# 3. Implementation of SVM predictions.
install.packages("kernlab")
library(kernlab)
sv<-alphaindex(filter3)[[1]] #The index of the resulting support vectors in the data matrix. Note that this index refers to the pre-processed data (after the possible effect of na.omit and subset)
co<-coef(filter3)[[1]] #The corresponding coefficients times the training labels.
inte<- - b(filter3) #The negative intercept.
k<-0

rbfkernel <- rbfdot(sigma = 0.05)
rbfkernel
kpar(rbfkernel)
for(i in 1:10){ # We produce predictions for just the first 10 points in the dataset.
  k2<-0
  for(j in 1:length(sv)){
    #k2<- # Your code here
    x= as.vector(spam[sv[j],-58])
    names(x)<-NULL
    x<-as.numeric(x)
    y= as.vector(spam[i,-58])
    names(y)<-NULL
    y<-as.numeric(y)
    RBFkernel= rbfkernel(y,x)
    #cat("RBF: ",RBFkernel)
    k2 <- k2+co[j]*RBFkernel
   # cat("k2 ",k2)
  }
  k2<-k2+inte
 # cat("k2 after intercept: ",k2)
  sig<-sign(k2)
  #cat("sig: ",sig)
  k<-c(k,sig) # Your code here)
}
k
predict(filter3,spam[1:10,-58], type = "decision")


#see formula 8.35c in lecture 3b slide 8. 