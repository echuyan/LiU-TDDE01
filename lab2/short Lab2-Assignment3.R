
############### assignment 3  #############  Task1 ################

communities=read.csv("communities.csv")

#scaling 
library(caret)
scaler=preProcess(communities[1:100])
communitiesScaled=predict(scaler,communities[1:100])
communitiesScaled$ViolentCrimesPerPop=communities$ViolentCrimesPerPop
data=communitiesScaled
data$ViolentCrimesPerPop=c()

#Covariance matrix and eigenvalues/vectors
covarianceMatrix <- cov(data) 
eig=eigen(covarianceMatrix)
eigenValues=eig$values
print(eigenValues)
eigenVectors=eig$vectors
print(eigenVectors)

#giving nice names to eigenvectors
names <- rep("PC",100)
for (i in 1:100) {
  names[i] = paste(names[i],as.character(i));}
colnames(eigenVectors) <- names
eigenVectors=eigenVectors*(-1)

#calculate proportion of variation
sprintf("%2.3f",eigenValues/sum(eigenValues)*100)
#create handy dataframe to analyze variance and answer questions
variances=data.frame(cbind(
  eigenValues,
  sd = sqrt(eigenValues),
  variance.percent = paste0(signif((eigenValues/sum(eigenValues)),2)*100,"%"),
  cumulative.variance.percent = paste0(cumsum(eigenValues/sum(eigenValues))*100,"%")))
print(variances)

#implementing score matrix
score_matrix <-  as.matrix(data) %*% as.matrix(eigenVectors) 

###############################Task2#####################

#Traceplot for PC1
xy=princomp(data)

#this trace plot is not easy to interpret ( barplot is easier to interpret):
plot(xy$loadings[,1],main="traceplot")

# this one is also not easy to interpret
autoplot(xy,data,loadings = TRUE,loadings.label = TRUE) 

#this is more nice but not a trace plot:
par(mar=c(8, 3, 3, 1))
barplot(xy$loadings[,1], main="PC 1 Loadings Plot", las=2,xlab="Features",ylab="loadings",xpd=FALSE,cex.names=0.6)


#----Report which 5 features contribute mostly (by the absolute value) to the first principle component.
vector1 = xy$loadings[,1] #storing loadings for PC1 in a vector
vector2 = vector1[order(abs(vector1))] #sort this vector by abs
tail(vector2,5) # getting 5 biggest values, those are most important variables:


# a plot of the PC scores (PC1, PC2) in which the color of the points is given by ViolentCrimesPerPop.
autoplot(xy,communitiesScaled,colour = 'ViolentCrimesPerPop')


###########################Task3#####################################


#scaling 
library(caret)
scaler1=preProcess(communities)
communitiesScaled1=predict(scaler1,communities)


#dividing the train and test
set.seed(12345)
n=nrow(communitiesScaled1)
id=sample(1:n, floor(n*0.5))
train=communitiesScaled1[id,]
test=communitiesScaled1[-id,]



#model based on training data and prediction on test
linearModelCrimes=lm(ViolentCrimesPerPop~.-1, data=train)
predictionTestCrime=predict(linearModelCrimes,newdata=test,interval = "prediction")

MSEtest=mean((test$ViolentCrimesPerPop-predictionTestCrime)^2)
sum=summary(linearModelCrimes)
MSEtrain=mean(sum$residuals^2)

#ANSWER: the model seems overfitted because test error is 6 times higher than train error, but it's hard to judge because 
#mse is not enough to interpret the model quality


########################################Task4#####################################
#Implement a function that depends on parameter vector 𝜃 and represents the
#cost function for linear regression without intercept on the training data set.
k=0
ListMSETrainErrors=list()
ListMSETestErrors=list()
costfunction<-function(theta,datax,datatarget){
 
  result=(t(as.matrix(datatarget)-as.matrix(datax)%*%as.matrix(theta))%*%(as.matrix(datatarget)-as.matrix(datax)%*%as.matrix(theta)))
  .GlobalEnv$k= .GlobalEnv$k+1

  .GlobalEnv$ListMSETrainErrors[[k]]=mean((as.matrix(train[,-101])%*%as.matrix(theta)-as.matrix(train[,101]))^2)
  .GlobalEnv$ListMSETestErrors[[k]]=mean((as.matrix(test[,-101])%*%as.matrix(theta)-as.matrix(test[,101]))^2)
  
  return (result)
}

#use BFGS method (optim() function without gradient specified)
# compute training and test errors for every iteration number. 
#adding global variables that were called inside the function. 
par1 <- rep(c(0),each=100)
result<- optim(par1,fn=costfunction,datax=input,datatarget=output,method="BFGS")
print(result)

#remove some huge numbers in MSEs appear  from vectors of MSEs
newlist = as.vector(ListMSETrainErrors)
newlist=newlist[newlist <1]
newlist1 = as.vector(ListMSETestErrors)
newlist1=newlist1[newlist1 <1]
#The outliers are 5% of data and they change the plot significantly. 

#newlist and newlist1 have different lengths so we fit their lengths to each other to be able to plot them
diff = length(newlist) - length (newlist1)
if (diff<0) {
newlist = newlist[-c(1:500)]
newlist1 = newlist1[-c(1:500)]
diff = -1*diff
newlist1 = newlist1[-c(1:diff)]
} else
{
  newlist = newlist[-c(1:500)]
  newlist = newlist[-c(1:diff)]
  newlist1 = newlist1[-c(1:500)]
}

#install.packages("Hmisc")
library(Hmisc)

#iterationNumber=length(newlist)
matplot(y=cbind(newlist,newlist1),type="l",col=c("red","blue"),xlim=c(0,20000), ylim=c(0,1))
minor.tick(nx = 5,  tick.ratio = 1)

#No significant improvement of the error for 10000 iterations. 
print("train from optimal model by early stopping criteria")
print(newlist[6000])
print("test error from optimal model by early stopping criteria")
print(newlist1[6000])





