##################################################Preparations##########################################
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


######################################################Task 1.###########################################
#MSE for train and test
MSEtrain=mean(sum$residuals^2)

#calculate MSE for test data set
predictionTest=predict(linearModel,newdata=test,interval = "prediction")
MSEtest=mean((test$Fat-predictionTest)^2)


######################################################Task 2.############################################

#cost function - see the report

######################################################Task 3.############################################
library(glmnet)
library(dplyr)
#creating lasso regression model
x=as.matrix(train%>%select(-Fat))
y=as.matrix(train%>%select(Fat))
LASSOmodel=glmnet(x, y, alpha=1)
plot(LASSOmodel, xvar = "lambda")


######################################################Task 4.############################################
#Ridge regression and  plot of lambda 
ridgeRegressionModel=glmnet(x, y, alpha=0)
plot(ridgeRegressionModel, xvar="lambda")


######################################################Task 5.##########################################################
#cross validation 
modelCrossValidation <- cv.glmnet(x=x, y=y, alpha=1, family="gaussian")
plot(modelCrossValidation)
#lambda min is 0.05745
print(modelCrossValidation$lambda.min)

#build a model with minLambda and see how many variables are there
LASSOmodelOptimalLambda=glmnet(x, y, alpha=1,lambda=modelCrossValidation$lambda.min)
print(LASSOmodelOptimalLambda$beta)
#there are 7 variables for the model with optimal lambda


# 0.01831563888 is lambda for  log λ = −4, it is located somewhere between 63 and 64 position in lambda array of cross-validation dataset
print(modelCrossValidation$cvm[63])
print(modelCrossValidation$cvm[64])
print(modelCrossValidation$cvm[51])
print(modelCrossValidation$cvsd[63])
print(modelCrossValidation$cvsd[64])
print(modelCrossValidation$cvsd[51])

#plot of the original test versus predicted test values for the model corresponding to optimal lambda 
x1=as.matrix(test%>%select(-Fat))
predictionOptimalLambda=predict(object = LASSOmodelOptimalLambda,newx=x1,s="lambda.min")
matplot(y=cbind(predictionOptimalLambda,test$Fat),type="l",col=c("red","blue"),lty=c(1,1))


#check mse for linear regression model in task 1 and compare with mse for optimal model

MSEOptimalLambda=mean((predictionOptimalLambda-test$Fat)^2)
print(MSEOptimalLambda)
