##################################################Preparations#########################################################
tecator=read.csv("tecator.csv")

#REMOVE UNNECESSARY COLUMNS FIRST
tecator$Protein=c()
tecator$Moisture=c()
tecator$Sample=c()

#dividing the train and test
set.seed(12345)
n=nrow(tecator)
id=sample(1:n, floor(n*0.5))
train=tecator[id,]
test=tecator[-id,]

#model based on training data
linearModel=lm(Fat~., data=train)
sum=summary(linearModel)


######################################################Task 1.##########################################################
#it is a linear model so I guess that the underlying probabilistic model, is Yi=B0+B1X1+B2X2+...+BPXP+E(noise)

#MSE for train and test
MSEtrain=mean(sum$residuals^2)

#calculate MSE for test data set
predictionTest=predict(linearModel,newdata=test,interval = "prediction")
MSEtest=mean((test$Fat-predictionTest)^2)

#Training Error, see section 4.1 page 71 in book, formula fig 4.4 page 65

#Test error, Enew, definition fig 4.2 on page 64??? see page 71 paragraph right before 4.3 for 
#"The error function evaluated on the test data set could indeed be called ‘test error’". 

#package caret??

#Comment model fit looks good for train data set, looks awful for test data set. Based on MSE. 
#--Mse was so high becuae model to complex, overfitted to training, memorized training data set. Numbers are correct, just look att lecture slides. 


######################################################Task 2.##########################################################


#cost function to be optimized. 
#Formula for cost function can be seen lecture 2d slide 20 

#enough to show cost function and explain its parts or do we need to apply real values from model, for example model coefficients. 

#--Answer. IN report show formula and explain what the terms correspnd to in our model for example y is 
#acutal value like fat, theta is our coefficents which corresponds to channels
# pnumber of vairables, n number of elements, x independent variables channels again. 

######################################################Task 3.##########################################################
library(glmnet)
library(dplyr)
#creating lasso regression model
x=as.matrix(train%>%select(-Fat))
y=as.matrix(train%>%select(Fat))
LASSOmodel=glmnet(x, y, alpha=1)

plot(LASSOmodel, xvar = "lambda")
#Interpretation. High lambda, fewer variables that stay in the model. https://stats.stackexchange.com/questions/68431/interpretting-lasso-variable-trace-plots
#Question, we interpret it that log lambda is based on the lambdas in the mB model, but lowest log lambda from those 
#values does not correspond to lowest log lambda in the plot. 
#--Answer try another base for log. Personal opinion: don't think it is improtant just curiousity. 

#Assuming we did everything correctly we should want a log lambda close to zero from the negative side to have 3 features.
#we pick lambda = 0.853045182 for three features

#no clue we were curious??
#tmpmodel=glmnet(x, y, lambda=0.853045182, alpha=1)
#plot(tmpmodel)

######################################################Task 4.##########################################################
#Ridge regression and  plot of lambda task 4
ridgeRegressionModel=glmnet(x, y, alpha=0)
plot(ridgeRegressionModel, xvar="lambda")
#As lambda increases our coefficients are forced to 0. 
#A good lambda pick is neither of the extremes but somewhere in between. 
#Good lambda can be determined with cross validation, page 110. 
#In contrast to Ridge, the LASSO regularization will actually set less-important predictors to 0 and help you with choosing the predictors that can be left out of the model
#One difference is that in lasso many coefficients are forced to zero at first than in ridge. 
#lasso plot produces simples and more readable plots with a reduced set of predictors. 
#suppose you have a high dimensionality and high correlation in your dataset, then you would want to prefer L1(lasso) regularisation since it penalises less important features more and makes them zero which gives you the benefit of algorithmic feature selection and would make robust predictions than L2(ridge) regularisation but sometimes it can remove certain signals from the model even when they have information so it should be used carefully.

######################################################Task 5.##########################################################
#Task 5. What to do, find opt lambda decide is stat signif. 
#cross validation happenings
modelCrossValidation <- cv.glmnet(x=x, y=y, alpha=1, family="gaussian")
plot(modelCrossValidation)
#of note seen in modelCrossValidation lambda min is 0.05745
#print(modelCrossValidation$lambda.min)
#QUESTION!!! why the min lambda is near zero but MSE is not the lowest possible. 
#ANSWER!!!!! TAKE NATURAL LOG
#Curious Question. The dotted line on the far left indicates the value of λλ which minimises CV error.
#minLambda=modelCrossValidation$lambda.min
#-2.85684027469 is a log of optimal lambda

#--Answer: look at the index above the plot and interpret it. Lambda min is not neccesarily the optimal lambda. 
#Maybe we picked wrong optimal lambda. Look at the red grapth again. modelCrossValidation graph

#we need to build a model with minLambda and see how many variables are there
LASSOmodelOptimalLambda=glmnet(x, y, alpha=1,lambda=modelCrossValidation$lambda.min)
print(LASSOmodelOptimalLambda$beta)

#there are 7 variables for the model with optimal lambda

#Does the information displayed in the plot suggests that the optimal λ value results in a statistically
#significantly better prediction than log λ = −4
# 0.01831563888 is lambda for  log λ = −4, it is located somewhere between 63 and 64 position in lambda array of cross-validation dataset
print(modelCrossValidation$cvm[63])
print(modelCrossValidation$cvm[64])
print(modelCrossValidation$cvm[51])
print(modelCrossValidation$cvsd[63])
print(modelCrossValidation$cvsd[64])
print(modelCrossValidation$cvsd[51])
#QUESTION!!! we picked lamba#51 before the log lambda = -4 because it has lower cvm (mean cross-validated error) and cvsd. Is this correct?
#--Answer: In general we compare two lambdas from one of the error and deviation is smaller than the first one is 
#better more optimal. General approach is good. 

#Task5 Finally, create a scatter plot of the original test versus predicted test values for the model corresponding to optimal lambda and 
#comment whether the model predictions are good
x1=as.matrix(test%>%select(-Fat))
predictionOptimalLambda=predict(object = LASSOmodelOptimalLambda,newx=x1,s="lambda.min")

#plot(x=predictionOptimalLambda,y=test$Fat,col=c("green","red"))
matplot(y=cbind(predictionOptimalLambda,test$Fat),type="l",col=c("red","blue"),lty=c(1,1))

#comment good or not
#compare with linear regression model. 
#remember to chech mse for linear regression model in task 1 and compare with mse for 
#this graph, plot(x=predictionOptimalLambda,y=test$Fat,col=c("green","red"))

MSEOptimalLambda=mean((predictionOptimalLambda-test$Fat)^2)
print(MSEOptimalLambda)
#better, compared with first one almost nothing. 

#we expect an implemented linear model, we expect a line, something close to it. predict on fat on yvariable. y real value x predicted. 