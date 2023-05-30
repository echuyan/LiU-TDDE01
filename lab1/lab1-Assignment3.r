#----------------------------lab1. assignment3. logistic regression and BFE---------------------------
#loading the dataset
diabetes=read.csv("pima-indians-diabetes.csv",header=FALSE)


#-------------------------------------------------------------question1----------------------------------------------------------
#1. Make a scatterplot showing a Plasma glucose concentration on Age where observations are colored by Diabetes levels.
#-----------------------------------------------------------------------------------------

library("ggplot2")

pl <-  ggplot(diabetes,aes(x=diabetes$V8,y=diabetes$V2,col=diabetes$V9))+geom_point()
pl + labs(x = "Age",y = "Plasma glucose", title = "Diabetes dataset",colour = "1 = Yes, 0 = No")


#-----------------------------------------------------------------------------------------
#Do you think that Diabetes is easy to classify by a standard logistic regression model that uses these two variables as features? 
#-----------------------------------------------------------------------------------------

#answer - relatively easy, as there are only two possible outputs (whether a person has diabetes or not) and there is a certain interconnection between 
#actually having diabetes and a pair of parameters (Age+Blood sugar levels)

#-------------------------------------------------------------question2----------------------------------------------------------
#2 Train a logistic regression model with 𝑦 =Diabetes as target, 𝑥1 =Plasma glucose concentration and 
#𝑥2 =Age as features and make a prediction for all observations by using 𝑟 = 0.5 as the classification threshold. 
#-----------------------------------------------------------------------------------------

#building a logistic regression model based on the whole dataset
model=glm(diabetes$V9~diabetes$V2+diabetes$V8,family=binomial())
summary(model)
prediction = predict(model,diabetes,type="response")

#building a vector of predictions based on the threshold of 0.5
#Yes - diabetes, No - healthy
vectorPred=rep("No",nrow(diabetes))
vectorPred[prediction>0.5] = "Yes"
View(vectorPred)

#building a confusion matrix
table(vectorPred,diabetes$V9)
#result => more than 25% errors, i.e. out of 768 objects 202 were false positive or false negative

#-----------------------------------------------------------------------------------------
#Report the probabilistic equation of the estimated model (i.e., how the target depends on the features and the estimated model parameters probabilistically).
#-----------------------------------------------------------------------------------------

print(model$coefficients)
#acquired coefficients from the model
#(Intercept) diabetes$V2 diabetes$V8 
#-5.91244906  0.03564404  0.02477835 

#formula is as following:
#   p(y|X)= 1 / (1+exp(-(-5.91+0.035x1+0.024x2)))

#-----------------------------------------------------------------------------------------
#Compute also the training misclassification error and make a scatter plot of the same kind as in step 1 but showing the predicted values of Diabetes 
#as a color instead. 
#-----------------------------------------------------------------------------------------

MSE=mean(model$residuals^2)
pl2 <- ggplot(diabetes,aes(x=diabetes$V8,y=diabetes$V2,col=vectorPred))+geom_point()
pl2 + labs(x = "Age",y = "Plasma glucose", title = "Prediction, r=0.5",colour = "1 = Yes, 0 = No")

#-----------------------------------------------------------------------------------------
#Comment on the quality of the classification by using these results.
#-----------------------------------------------------------------------------------------

#The quality is pretty mediocre because as it is seen on the plot, there is no concrete boundary between two domains of decisions so there is a high amount of errors


#-------------------------------------------------------------question3----------------------------------------------------------
#3. Use the model estimated in step 2 to 
#a) report the equation of the decision boundary between the two classes 
#-----------------------------------------------------------------------------------------
#  -5.91+0.035x1+0.024x2 = 0


#-----------------------------------------------------------------------------------------
#b) add a curve showing this boundary to the scatter plot in step 2. Comment whether the decision boundary seems to catch the
#data distribution well
#-----------------------------------------------------------------------------------------


pl3=ggplot(diabetes,aes(x=diabetes$V8,y=diabetes$V2,col=vectorPred))+geom_point()
fun.1 <- function(x1) (5.91244906 - 0.03564404*x1)/ 0.02477835
pl3 + stat_function(fun = fun.1) + xlim(20,100) + labs(x = "Age",y = "Plasma glucose", title = "Prediction, r=0.5",colour = "1 = Yes, 0 = No")

#looking at the plot we would not say that the decision boundary catches the data distribution well which is approved by the confusion matrix


#-------------------------------------------------------------question4----------------------------------------------------------
#Make same kind of plots as in step 2 but use thresholds 𝑟 = 0.2 and 𝑟 = 0.8. 
#By using these plots, comment on what happens with the prediction when 𝑟value changes.
#-----------------------------------------------------------------------------------------
  
#building a vector of predictions based on the threshold of 0.2
#Yes - diabetes, No - healthy
vectorPred02=rep("No",nrow(diabetes))
vectorPred02[prediction>0.2] = "Yes"
View(vectorPred02)
#building a confusion matrix
table(vectorPred02,diabetes$V9)
#result => more than 37% errors, i.e. out of 768 objects 286 were false positive or false negative
pl4 <- ggplot(diabetes,aes(x=diabetes$V8,y=diabetes$V2,col=vectorPred02))+geom_point()
pl4 +  labs(x = "Age",y = "Plasma glucose", title = "Prediction, r=0.2",colour = "1 = Yes, 0 = No")
#it can be seen both from the graph and confusion matrix that with r=0.2 the number of FALSE POSITIVEs increased significantly
#hence low threshold is not the best choice


#building a vector of predictions based on the threshold of 0.8
#Yes - diabetes, No - healthy
vectorPred08=rep("No",nrow(diabetes))
vectorPred08[prediction>0.8] = "Yes"
View(vectorPred08)
#building a confusion matrix
table(vectorPred08,diabetes$V9)
#result => approx 32% errors, i.e. out of 768 objects 242 were false positive or false negative
pl5 <- ggplot(diabetes,aes(x=diabetes$V8,y=diabetes$V2,col=vectorPred08))+geom_point()
pl5 +  labs(x = "Age",y = "Plasma glucose", title = "Prediction, r=0.8",colour = "1 = Yes, 0 = No")
#although the number of error slightly decreased, it is still high. This time there are more FALSE NEGATIVES (which probably is worse than in previous case)
#high threshold is even worse in this case because it misleads in a detrimental way



#-------------------------------------------------------------question5----------------------------------------------------------
#Perform a basis function expansion trick by computing new features𝑧1 = , adding them to the data set and
#then computing a logistic regression model with 𝑦 as target and #𝑥1, 𝑥2,𝑧1, … , 𝑧5 as features. 
#-----------------------------------------------------------------------------------------

#x1 = diabetes$V2 = plasma glucose level
#x2=diabetes$V8 = age
BFEdiabetes= diabetes
BFEdiabetes$Z1=BFEdiabetes$V2^4
BFEdiabetes$Z2=BFEdiabetes$V2^3 * BFEdiabetes$V8
BFEdiabetes$Z3=BFEdiabetes$V2^2 * BFEdiabetes$V8^2
BFEdiabetes$Z4=BFEdiabetes$V2 * BFEdiabetes$V8^3
BFEdiabetes$Z5=BFEdiabetes$V8^4

modelBFE=glm(BFEdiabetes$V9~BFEdiabetes$V2+BFEdiabetes$V8+BFEdiabetes$Z1+BFEdiabetes$Z2+BFEdiabetes$Z3+BFEdiabetes$Z4+BFEdiabetes$Z5,family=binomial())
summary(model)


predictionBFE = predict(modelBFE,BFEdiabetes,type="response")

#-----------------------------------------------------------------------------------------
#Create a scatterplot of the same kind as in step 2 for this model and compute the training misclassification rate. 
#-----------------------------------------------------------------------------------------

#building a vector of predictions based on the threshold of 0.5 (after basis function expansion)
#Yes - diabetes, No - healthy
vectorPredBFE=rep("No",nrow(BFEdiabetes))
vectorPredBFE[predictionBFE>0.5] = "Yes"
View(vectorPredBFE)

#building a confusion matrix
table(vectorPredBFE,BFEdiabetes$V9)


MSEBFE=mean(modelBFE$residuals^2)
pl6 <- ggplot(BFEdiabetes,aes(x=BFEdiabetes$V8,y=BFEdiabetes$V2,col=vectorPredBFE))+geom_point()
pl6 +  labs(x = "Age",y = "Plasma glucose", title = "Prediction after BFE, r=0.5",colour = "1 = Yes, 0 = No")

#-----------------------------------------------------------------------------------------
#What can you say about the quality of this model compared to the previous logistic
#regression model? How have the basis expansion trick affected the shape of the decision boundary and the prediction accuracy?
#You can visualize the decision boundary by plotting the scatter plots and coloring the samples by your prediction from this new model. 
#You do not need to plot the decision function as such.
#-----------------------------------------------------------------------------------------

#result => out of 768 objects 188  were false positive or false negative which makes 24% errors
#also there can be seen that the amount of false negatives decreased a bit


#take a look at the coefficients
print(modelBFE$coefficients)
#(Intercept)   BFEdiabetes$V2   BFEdiabetes$V8 BFEdiabetes$Z1 BFEdiabetes$Z2  BFEdiabetes$Z3 BFEdiabetes$Z4  BFEdiabetes$Z5 
#-9.309821e+00   3.793014e-02   1.456805e-01   1.278015e-08   -1.779600e-07   8.515150e-07   -1.698011e-06   8.126623e-07 
#   1/(1+exp(-(-9.309821e+00 + 3.793014e-02*x1 + 1.456805e-01*x2 + 1.278015e-08*x1^4 - 1.779600e-07*x1^3*x2 + 8.515150e-07*x1^2*x2^2 - 1.698011e-06*x1*x2^3 +  8.126623e-07*x2^4 )))=0.5





