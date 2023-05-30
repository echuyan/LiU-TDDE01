#####Task 1####
bank=read.csv("bank-full.csv",sep=";")

#REMOVE UNNECESSARY COLUMNS FIRST
bank$duration=c()

#dividing the train, valid and test
n=dim(bank)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.4))
train=bank[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.3))
valid=bank[id2,]
id3=setdiff(id1,id2)
test=bank[id3,]

#####Task2 decision trees#####
#default tree
#install.packages("tree")
library("tree")
defaultTree=tree(as.factor(y)~.,data=train)
plot(defaultTree)
print(defaultTree)
#tree with the smallest node size of 7000
Tree7000=tree(as.factor(y)~.,data=train,minsize=7000)
plot(Tree7000)
print(Tree7000)

#tree with the deviance to 0.0005
TreeDeviance00005=tree(as.factor(y)~.,data=train,mindev=0.0005)
plot(TreeDeviance00005)
print(TreeDeviance00005)

#confusion matrixes for the trees
#default
TreeFit=predict(defaultTree,newdata=train, type="class")
table(train$y,TreeFit)
TreeFitValid=predict(defaultTree,newdata=valid, type="class")
table(valid$y,TreeFitValid)

#nodesize7000
TreeFit2=predict(Tree7000,newdata=train, type="class")
table(train$y,TreeFit2)
TreeFit2Valid=predict(Tree7000,newdata=valid, type="class")
table(valid$y,TreeFit2Valid)

#deviance minimum0.0005
TreeFit3=predict(TreeDeviance00005,newdata=train, type="class")
table(train$y,TreeFit3)
TreeFit3Valid=predict(TreeDeviance00005,newdata=valid, type="class")
table(valid$y,TreeFit3Valid)

#missclassifications rate training and test
MCETrainDefaultTree=mean(TreeFit != train$y) 
MCETrainTree7000=mean(TreeFit2 != train$y)
MCETrainDeviance00005=mean(TreeFit3 != train$y)
MCEValid=mean(TreeFit != valid$y) 
MCEValidTree7000=mean(TreeFit2 != valid$y)
MCEValidDeviance00005=mean(TreeFit3 != valid$y)

#####Task 3#####
#creating empty vectors
trainScore=rep(0,50)
validScore=rep(0,50)

#Filling empty vectors with deviance scores from the pruned tree
for(i in 2:50) {
  prunedTree=prune.tree(TreeDeviance00005,best=i)
  pred=predict(prunedTree, newdata=valid,type="tree")
  trainScore[i]=deviance(prunedTree)
  validScore[i]=deviance(pred)
}

#graph of the dependence of deviances for the training and the validation data on the number of leaves
plot(2:50, trainScore[2:50], type="b", col="red", ylim=c(8000,13000))
points(2:50, validScore[2:50], type="b", col="blue")
min(validScore[2:50])

#fit for tree with optimal amount of leaves
bestTree = prune.tree(TreeDeviance00005,best=20)
#info for besttree such as variables actually used
summary(bestTree)

#Plotting bestTree to see structure
plot(bestTree)
#adding text for readability
text(bestTree)
#more info
print(bestTree)

#####Task4#####
#Creating predicted values for test data using decision tree bestTree. 
predictionTest=predict(object=bestTree, newdata=test,type="class")

#Creating confusion matrix for the prediction
CMBestTree = table(test$y,predictionTest)
print(CMBestTree)

#calculating accuracy for the model. 
accuracy <- sum(diag(CMBestTree)) / sum(CMBestTree)
cat(accuracy*100,"%")

#Precision: Correct positive predictions relative to total positive predictions
precision = CMBestTree[2,2]/(CMBestTree[2,2]+CMBestTree[2,1])
#Recall: Correct positive predictions relative to total actual positives
recall=CMBestTree[2,2]/(CMBestTree[2,2]+CMBestTree[1,2])
F1score=2*recall*precision/(recall+precision)
cat(F1score*100,"%")

#imablanced dataset?
print(length(train$y[train$y=="no"]))
print(length(train$y[train$y=="yes"]))

#####Task5#####
#Predicted loss matrix
LossMatricePredict = predict(bestTree, newdata = test)
#applying lossmatrix to predition table
weightedLosspred = ifelse(LossMatricePredict[,1]/LossMatricePredict[,2] > 5, "no", "yes")
#building confusion matrix between prediction with loss matrix and real test data
CMLossMatriceTree = table(yreal=test$y,ypred=weightedLosspred)
print(CMLossMatriceTree)

#calculating accuracy
accuracy1 <- sum(diag(CMLossMatriceTree)) / sum(CMLossMatriceTree)
cat(accuracy1*100,"%")
#Precision: Correct positive predictions relative to total positive predictions
precision1 = CMLossMatriceTree[2,2]/(CMLossMatriceTree[2,2]+CMLossMatriceTree[2,1])
#Recall: Correct positive predictions relative to total actual positives
recall1=CMLossMatriceTree[2,2]/(CMLossMatriceTree[2,2]+CMLossMatriceTree[1,2])
#calculating F1-score
F1score1=2*recall1*precision1/(recall1+precision1)
cat(F1score1*100,"%")

#####Task6#####
#fit for logistic regression model
logisticRegressionModel=glm(as.factor(train$y)~.,data=train,family=binomial())
#creating a vector of the threshholds
vectorOfπ=seq(0.05, .95, by=0.05)

#empty lists
TPRforTree = c()
FPRforTree = c()

#loop for calculating TPR and FPR for each threshold value of pi for decision tree
for(i in 1:19) {
  #predicting y for the decision tree
  predictionTree2 = predict(object=bestTree, newdata = test)
  # we check if predictionTree2 for "Yes" > Each π
  YHatclassifiedTree= ifelse (predictionTree2[,2] > vectorOfπ[i] , "yes", "no")
  print(vectorOfπ[i])

  CMforTree= table(ytraintrue=test$y ,ytrainpred=factor(YHatclassifiedTree,levels=c("no","yes")))
  print(CMforTree)
  #now we can compute TPR and FPR
  TPRforTree[i] = CMforTree[2,2]/sum(CMforTree[2,])
  FPRforTree[i] = CMforTree[1,2]/sum(CMforTree[1,])
}

#empty lists
TPRforLM = c()
FPRforLM = c()


#the same for logistic regression
for(i in 1:19) {
  
  predictionLM = predict(object=logisticRegressionModel, newdata = test,type="response")
  # we check if prediction for "Yes" > Each π
  YHatLM= ifelse (predictionLM > vectorOfπ[i] , "yes", "no")
  print(vectorOfπ[i])
  
  CMforLM= table(ytraintrue=test$y ,ytrainpred=YHatLM)
  print(CMforLM)
  #now we can compute TPR and FPR
  TPRforLM[i] = CMforLM[2,2]/sum(CMforLM[2,]) #TP/P True positive divided with all positive
  FPRforLM[i] = CMforLM[1,2]/sum(CMforLM[1,]) #FP/N False positive divided by all negatives 
}


library(ggplot2)

df <- data.frame(tprtree=TPRforTree,fprtree=FPRforTree,tprlm=TPRforLM,fprlm=FPRforLM)
#ROC plot for the tree
ggplot(data=df)+
  geom_line(mapping=aes(FPRforTree,TPRforTree), color = "blue",  size = 2, alpha = 0.7)+
  geom_line(mapping=aes(FPRforLM, TPRforLM), color = "red", size = 2, alpha = 0.7)+
  labs(title= "ROC curve", x = "False Positive Rate", y = "True Positive Rate")+
  scale_color_manual(labels = c("Tree", "LM"), values = c("blue", "red"))