library(neuralnet)

#####Task 1#####

set.seed(1234567890)
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test

# Random initialization of the weights in the interval [-1, 1]
# Question: It is only for task 4 or we should have it in other tasks too?!
winit <-  runif(25, -1, 1)# Your code here

nn <- neuralnet(Sin~Var, data=tr, hidden=10, startweights=winit)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)


#Comment. The model is good. But we see a slight error in valley of the curve. 
 

#####Task 2#####
winit <-  runif(25, -1, 1)
linear <- function(x) x
nnLinear <- neuralnet(Sin~Var, data=tr, hidden=10, act.fct = linear, startweights=winit)

winit <-  runif(25, -1, 1)
softplus <- function(x) log(1 + exp(x))
nnSoftPlus <- neuralnet(Sin~Var, data=tr, hidden=10, act.fct = softplus)

#there is a problem with plugging in max function into neuralnet because it is not differentiable  
#see https://stackoverflow.com/questions/34532878/package-neuralnet-in-r-rectified-linear-unit-relu-activation-function
#so we use workaround
#install.packages('sigmoid')
library(sigmoid)

#relu() 
# you do not need to implement this just explain why you cannot implement max
winit <-  runif(25, -1, 1)
nnReLu <- neuralnet(Sin~Var, data=tr, hidden=10, act.fct =relu, startweights=winit)

#plotting
#linear
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nnLinear,te), col="red", cex=1)
#It is a straight line. A bit to high up, 0.25. 

#softplus
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nnSoftPlus,te), col="red", cex=1)
 

#ReLu
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nnReLu,te), col="red", cex=1)

#the linear activation function gives a straight line and can't approximate non-linear sine function
# The soft plus fits as the best
# the softplus and relu are non-linear activation functions that considers unknown variables to the difference of the linear
#activation function. 
#"Number of hidden nodes affect complexity and how well it fits to the data. "

#For some reason max(0,a) doesn't work. 
#Max(0,x) doesn't work because it is not differentiable function. It can't be derived at zero. 
#aka the left and right side of the derivative are  not identical, which leads to problems and error for rstudio, 
#there are functions with workarounds but we need not implement them. 


##### Task 3 #####

set.seed(1234567890)
Var <- runif(500, 0, 50)
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test
#plot(mydata)
#plot(tr)
plot(te)

# Random initialization of the weights in the interval [-1, 1]
winit <-  runif(25, -1, 1)

nntask3 <- neuralnet(Sin~Var, data=tr, hidden=10, startweights=winit)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nntask3,te), col="red", cex=1)
library(Hmisc)
minor.tick(ny = 5,  tick.ratio = 1) #line seems to be exactly at -0.347 

# The prediction up to 10 variables is similar to task 1 (quite good), but for the rest it gets worse.


##### Task 4 #####

print(nntask3$weights)
#print(nn$weights)

# Look at some (large) x value (1 is the bias)
x <- c(1, 50)
print(x)

# Print the hidden units
hidden <- c(1, 1/(1+exp(-x %*% nntask3$weights[[1]][[1]])))
print(hidden)

# Print the prediction
prediction <- hidden %*% nntask3$weights[[1]][[2]]
print(prediction)




# Print the weights
print(nntask3$weights)
plot(nntask3)

# It can be seen that 

#If we increase training data in proportion to test data the model improves. 
#if we increase the number of nodes in the hidden layer the model improves. 
#We can see that the graph converges to the value -0.347 . We think this happens because the model is not complex enough considering 
#number of hidden nodes and amount of training data to fit to changes beyond 10 variables. 
#Weights are attached to inputs and convey the importance of that input in predicting final output
# Sigmoid function approaches 1 if hidden node times weight for the hidden is a positive value and 0 if the opposite
# So the final output will be close to 1*(sum of weights corresponding to non-zero hidden units). 
# Demonstration: as there only 5 non-zero hidden units + bias, only bias and weights ## 4,6,7,9,10 are taken into consideration 
# and they sum up exactly to the value the prediction converges to
# [4,] -1.2142028
#[6,]  0.6218254
#[7,] -1.2120181
#[9,] -1.2250588
#[10,]  2.1300140
summm<- sum(-1.2142028, 0.6218254,-1.2120181,-1.2250588,2.1300140,0.5520328)
#exactly -0.3474075 


plot(nntask3)

##### Task 5 #####
#predict x from sin(x)

set.seed(1234567890)
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))

# Random initialization of the weights in the interval [-1, 1]
winit <-  runif(25, -1, 1)

#NN that predicts x from sin(x) on the whole dataset (all point are training)
nnreversed <- neuralnet(Var~Sin, data=mydata, hidden=10, startweights=winit,threshold = 0.1)
reversedprediction=predict(nnreversed,mydata)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(mydata, cex=2) #black dots = training data
points(reversedprediction,mydata[,2],col="red", cex=1)
#prediction is very bad, not matching the data at all
#because we are trying to get a linear prediction from non-linear function of sine. for example at the point of sin(x)=0 there may be 4 different x.




