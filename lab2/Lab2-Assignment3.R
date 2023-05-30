#Task1
communities=read.csv("communities.csv")

#Scale all variables except of ViolentCrimesPerPop and implement PCA by using function eigen(). 
#Report how many components are needed to obtain at least 95% of variance in the data. 
#What is the proportion of variation explained by each of the first two principal components?

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
  names[i] = paste(names[i],as.character(i));
}
colnames(eigenVectors) <- names

eigenVectors=eigenVectors*(-1)

#Eigenvalue associated with each principal component tells you how much variation in the data set it explains 
#Each eigen vector represents a direction of variance. 
#The eigen vector corresponding to the largest eigen value will give the direction of maximum variance. 
#This is the first principal component. Then, the eigen vector corresponding to the 2nd largest eigen value will give the direction of the 
#second largest variance. This is the second principal component. And, so on.

#calculate proportion of variation
sprintf("%2.3f",eigenValues/sum(eigenValues)*100)
#create handy dataframe to analyze variance and answer questions
variances=data.frame(cbind(
  eigenValues,
  sd = sqrt(eigenValues),
  variance.percent = paste0(signif((eigenValues/sum(eigenValues)),2)*100,"%"),
  cumulative.variance.percent = paste0(cumsum(eigenValues/sum(eigenValues))*100,"%")))
print(variances)
#ANSWER: from the table above it is needed 35 components to reach at least 95% variance in the data
#ANSWER: the proportion of variation by two fist PC is 25.017+16.936  sums up to 41.953

#PCA
#implementing score matrix
#QUESTION!!! Is this the correct implementation?
score_matrix <-  as.matrix(data) %*% as.matrix(eigenVectors) 

#some nice graphs for PC1 for example
#note - this is from the internet and i can't explain all
plot(communitiesScaled$ViolentCrimesPerPop ~ communitiesScaled$state, col = "red", 
     main="Distance to First EigenV", pch = 19, 
     xlim=c(-5, 5), ylim = c(-5,5), data = communitiesScaled)
abline(v=0,h=0, col = "dark gray")
abline(0, eigenVectors[2,1]/eigenVectors[1,1],col='purple')
abline(0, eigenVectors[1,2]/eigenVectors[2,2],col='orange')

arrows(x0 = 0, y0 = 0, x1 = eigenValues[1]*eigenVectors[1,1], 
       y1 = eigenValues[1]*eigenVectors[2,1],col="purple",lwd=2)
arrows(x0 = 0, y0 = 0, x1 = eigenValues[2]*eigenVectors[1,2], 
       y1 = eigenValues[2]*eigenVectors[2,2],col="orange", lwd=2)


#FIRST DOUBLE-CHECK (REMOVE BEFORE REPORTING) for score matrix - gave the same result - OK
#prc=prcomp(data)$x
#View(prc)

#DOUBLE-CHECK another way, just to check that results match with standard functions - they do
#res=prcomp(data)
#lambda=res$sdev^2#eigenvalues
#print(lambda)
#print(eigenValues)
#View(res$rotation)
#screeplot(res)


###############################Task2#####################
#Repeat PCA analysis by using princomp() function and make the trace plot of the first principle component. 
#Do many features have a notable contribution to this component? 
#Report which 5 features contribute mostly (by the absolute value) to the first principle component. 
#Comment whether these features have anything in common and whether they may have a logical relationship to the crime level. 
#Also provide a plot of the PC scores in the coordinates (PC1, PC2) in which the color of the points is given by ViolentCrimesPerPop.
#Analyse this plot (hint: use ggplot2 package ).


#install.packages("ggplot2")
#install.packages("ggfortify")
#install.packages("rstan")
library(rstan)
library(ggplot2)
library(ggfortify)

#--Traceplot for PC1
xy=princomp(data)

autoplot(xy,data,loadings = TRUE,loadings.label = TRUE) ####it's a mess
#this is more nice but not a trace plot:
par(mar=c(8, 3, 3, 1))
barplot(xy$loadings[,1], main="PC 1 Loadings Plot", las=2,xlab="Features",ylab="loadings",xpd=FALSE,cex.names=0.6)

#traceplot they wanted (but barplot is easier to interpret):
#plot(xy$loadings[,1],main="Trshkgfls")

#QUESTION! To plot data we need an x and a y for the axis, we understand that the x value should be PC1, however we are a bit 
#lost on what should be on the y axis, could you give us a hint??
#ANSWER: We may check the slides for trtaceplot but this barplot is also okay!

#-----Do many features have notable contribution to PC1?
#judging from a barplot there might be around 10 important variables for PC1


#----Report which 5 features contribute mostly (by the absolute value) to the first principle component.
vector1 = xy$loadings[,1] #storing loadings for PC1 in a vector
vector2 = vector1[order(abs(vector1))] #sort this vector by abs
tail(vector2,5) # getting 5 biggest values, those are most important variables:
#PctPopUnderPov     pctWInvInc    PctKids2Par      medIncome      medFamInc 
#0.1737978     -0.1748683     -0.1755423     -0.1819830     -0.1833080 

#-------Comment whether these features have anything in common and whether they may have a logical relationship to the crime level. 
#1. medFamInc: median family income (differs from household income for non-family households) 
#2. medIncome: median household income
#3. PctKids2Par: percentage of kids in family housing with two parents
#4. pctWInvInc: percentage of households with investment / rent income in 1989
#5. PctPopUnderPov: percentage of people under the poverty level
#They all refer to wealth, well-being, profitability. First 4 contribute to crime level in a negative way, the last - in a positive way


#-------Also provide a plot of the PC scores in the coordinates (PC1, PC2) in which the color of the points is given by ViolentCrimesPerPop.
#-------Analyse this plot (hint: use ggplot2 package ).
autoplot(xy,communitiesScaled,colour = 'ViolentCrimesPerPop')
#,loadings = TRUE,loadings.label = TRUE
#PC1 is related to crime increase
#positive PC2 is associated with lower crime levels

###########################Task3#####################################
#Split the original data into training and test (50/50) and scale both features
#and response appropriately, and estimate a linear regression model from
#training data in which ViolentCrimesPerPop is target and all other data
#columns are features. Compute training and test errors for these data and
#comment on the quality of model.

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


#for general info: check r-squared for model 

########################################Task4#####################################
#Implement a function that depends on parameter vector 𝜃 and represents the
#cost function for linear regression without intercept on the training data set.
k=0
Fs=list()
Params=list()
ListMSETrainErrors=list()
ListMSETestErrors=list()
costfunction<-function(theta,datax,datatarget){
 
  #datatarget should be a vector representing target
  #datax are variable values from dataset
  #formula from lecture 1d, slide 7
  #the global variables are to fill lists of our errors for each iteration of optim. Optim uses a gradient function to find optim and it iterates multiple times. 
  result=(t(as.matrix(datatarget)-as.matrix(datax)%*%as.matrix(theta))%*%(as.matrix(datatarget)-as.matrix(datax)%*%as.matrix(theta)))
  .GlobalEnv$k= .GlobalEnv$k+1
 # print(k)
  .GlobalEnv$ListMSETrainErrors[[k]]=mean((as.matrix(train[,-101])%*%as.matrix(theta)-as.matrix(train[,101]))^2)
  #if (.GlobalEnv$ListMSETrainErrors[[k]]>10) {
   # print(.GlobalEnv$ListMSETrainErrors[[k]])
    #print(k)
    #print(theta)
  #}
  .GlobalEnv$ListMSETestErrors[[k]]=mean((as.matrix(test[,-101])%*%as.matrix(theta)-as.matrix(test[,101]))^2)
  #.GlobalEnv$Fs[[k]]=result
  #.GlobalEnv$Params[[k]]=theta
  return (result)
}

#test to check if costfunction works
theta=linearModelCrimes$coefficients
input=train[,-101]
output=train[,101]
print(costfunction(theta,input,output))


#Afterwards, use BFGS method (optim() function without gradient specified)
#to optimize this cost with starting point 𝜃𝜃 0 = 0 and compute training and
#test errors for every iteration number. 

#We solved this by adding global variables that were called inside the function. 
par1 <- rep(c(0),each=100)
result<- optim(par1,fn=costfunction,datax=input,datatarget=output,method="BFGS")
print(result)
  
#Present a plot showing dependence of
#both errors on the iteration number and comment which iteration number is
#optimal according to the early stopping criterion. 

#iterationNumber=c(rep(1:(k-500)))



#for whatever reason those huge numbers in MSEs appear, we are not interested in them at all.
#so we remove them from vectors of MSEs
newlist = as.vector(ListMSETrainErrors)
newlist=newlist[newlist <1]
newlist1 = as.vector(ListMSETestErrors)
newlist1=newlist1[newlist1 <1]
#The outliers are 5% of data and they change the plot significantly. 

#after we trimmed both newlist and newlist1, they have different lengths. so we need to fit their lengths to each other to be able to plot them
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

#plot(iterationNumber,newlist,col="green")
#points(iterationNumber,newlist1,col="red")

#install.packages("Hmisc")
library(Hmisc)

#iterationNumber=length(newlist)
matplot(y=cbind(newlist,newlist1),type="l",col=c("red","blue"),xlim=c(0,20000), ylim=c(0,1))
minor.tick(nx = 5,  tick.ratio = 1)



#Compute the training and
#test error in the optimal model, compare them with results in step 3 and
#make conclusions.

#we interpret an early stopping criterion as a stop at an arbitrary iteration that is good enough in terms of size of error.
#in this case we could pick for example 2300 because after that point there seems to be relatively no difference in error size

#No significant improvement of the error for 10000 iterations. 
print("train from optimal model by early stopping criteria")
print(newlist[6000])
print("test error from optimal model by early stopping criteria")
print(newlist1[6000])

#conclusion based on comparison, train MSE was higher in the optimal model, but the error on the test data was less than 
#half of the error for the default linear regression model. 
#The optimal model overfits less, much less than the default model. 

#a. Hint 1: don’t store parameters from each iteration (otherwise it will
#take a lot of memory), instead compute and store test errors directly.
#b. Hint 2: discard some amount of initial iterations, like 500, in your plot
#to make the dependences visible












