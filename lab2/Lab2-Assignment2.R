#Task1
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

#Task2 decision trees
##############################default tree
#install.packages("tree")
library("tree")
defaultTree=tree(as.factor(y)~.,data=train)
plot(defaultTree)
print(defaultTree)
##############################tree with the smallest node size of 7000
Tree7000=tree(as.factor(y)~.,data=train,minsize=7000)
plot(Tree7000)
print(Tree7000)

##############################tree with the deviance to 0.0005
TreeDeviance00005=tree(as.factor(y)~.,data=train,mindev=0.0005)
plot(TreeDeviance00005)
print(TreeDeviance00005)

#confusion matrixes for the trees
TreeFit=predict(defaultTree,newdata=train, type="class")
table(train$y,TreeFit)
#we got all no. 
TreeFitValid=predict(defaultTree,newdata=valid, type="class")
table(valid$y,TreeFitValid)
#all no

TreeFit2=predict(Tree7000,newdata=train, type="class")
table(train$y,TreeFit2)
#all no
TreeFit2Valid=predict(Tree7000,newdata=valid, type="class")
table(valid$y,TreeFit2Valid)
#all no

TreeFit3=predict(TreeDeviance00005,newdata=train, type="class")
table(train$y,TreeFit3)
#some yes
TreeFit3Valid=predict(TreeDeviance00005,newdata=valid, type="class")
table(valid$y,TreeFit3Valid)
#some yes

#missclassifications rate training and test
MCETrainDefaultTree=mean(TreeFit != train$y) 
MCETrainTree7000=mean(TreeFit2 != train$y)
MCETrainDeviance00005=mean(TreeFit3 != train$y)
MCEValid=mean(TreeFit != valid$y) 
MCEValidTree7000=mean(TreeFit2 != valid$y)
MCEValidDeviance00005=mean(TreeFit3 != valid$y)
#missclassification rate can be gotten from the summary() of the tree, might be faster/easier. 

#Report how changing the node size and deviance affect the size of tress and explain why. 
#From what we can see Deviance lead to better models 
#From what we can see node size didn't affect the prediction from the default tree. 
#Yet to be confirmed, check book , might be wrong. Deviance minimum made the tree bigger thus better predictions on training, node size didn't affect size, thus same prediction. 
#Setting node size number larger causes smaller trees and thus lower purity of the leafs => lower accuracy of the tree. This conerns the tree that set node size = 7000. 
#Very deep trees are no necessarily good becuse they overfit the training data and thus the prediction quality might become worse. Page 29&36 paragraph 4. 
#
#Why does 

#Probably the tree with deviance=0.0005 is overfitted and that's why the MCE grew up on valid. 

#Which model was he best?
#Deviance tree overfitted, and the other two gave similar errors and confusion matrices, but one of default and node size 7000. 
#QUESTION! How to chose if give the same result? 
#ANSWER COULD BE. INTERPRET IT!
#hint - majority vote in nodes. try with different values of minsize


###################Task3
#Use training and validation sets to choose the optimal tree depth in the model 2c: study the trees up to 50 leaves. 

trainScore=rep(0,50)
validScore=rep(0,50)
for(i in 2:50) {
  prunedTree=prune.tree(TreeDeviance00005,best=i)
  pred=predict(prunedTree, newdata=valid,type="tree")
  trainScore[i]=deviance(prunedTree)
  validScore[i]=deviance(pred)
}

#Present a graph of the dependence of deviances for the training and the validation data on the number of leaves

plot(2:50, trainScore[2:50], type="b", col="red", ylim=c(8000,13000))
points(2:50, validScore[2:50], type="b", col="blue")
min(validScore[2:50])
#Question!!!, why deviance higher for train than for validation dataset wgat can interpreted? valid set smaller less complex?
#ANSWER. Number of observation matters (we were right). But check the formula for deviance

#and interpret this graph in terms of bias-variance tradeoff. 
#POSSIBLE ANSWER: we had to choose the point with the lower bias but the variance wasn't the lowest at this point

#overfitting = high variance (predicts worse the test data than training data)
#low model complexity = high bias () = high deviance
#more leaves = more complex model
#variance is the difference in the fits between different datasets, in the graph = the distance between train and valid lines - not correct actually
#ANSWER Check the book. Variance regards outputs of the models
#decision trees are flexible models => they adapt better to training data and the noise in it
#QUESTION!!! Are the statements above correct?
#QUESTION!!! is the relationship between bias and variance always linear (if bias grows then variance shrinks)?
#ANSWER. No, it's not linear



#Report the optimal amount of leaves and which variables seem to be most important for decision making in this tree. 
#POSSIBLEANSWER: The optimal number of leaves is 20 because at this point the tree has the lowest deviance for validation dataset
bestTree = prune.tree(TreeDeviance00005,best=20)
summary(bestTree)
#Variables actually used in tree construction:
#  [1] "pdays"    "age"      "balance"  "campaign" "day"
#pdays number of days that passed by after the client was last contacted from a
#previous campaign 
#balance = account balance
#campaign  number of contacts performed during this campaign and for this client
#day last contact day of the week
#result = has the client subscribed a term deposit? no or yes

#Interpret the information provided by the tree structure (not everything but most important findings).
plot(bestTree)
print(bestTree)
text(bestTree)
#POSSIBLEANSWER
#There are redundant branches (left part pdays<8.5 and left part of the pdays<175.5) leading to
#all nos as outcomes
#This model is a bit too complex

##################Task4############################
#Estimate the confusion matrix, accuracy and F1 score for the test data by using the optimal model from step 3. 

predictionTest=predict(object=bestTree, newdata=test,type="class")

CMBestTree = table(test$y,predictionTest)
print(CMBestTree)
#predictionTest
#      no   yes
#no  11764   215
#yes  1354   231

accuracy <- sum(diag(CMBestTree)) / sum(CMBestTree)
cat(accuracy*100,"%")
#accuracy is 88.43262 % indicate our model has good accuracy

#Precision: Correct positive predictions relative to total positive predictions
precision = CMBestTree[2,2]/(CMBestTree[2,2]+CMBestTree[2,1])
#Recall: Correct positive predictions relative to total actual positives
recall=CMBestTree[2,2]/(CMBestTree[2,2]+CMBestTree[1,2])
F1score=2*recall*precision/(recall+precision)
cat(F1score*100,"%")
#F1 score is is 22.74742% indicates our model has poor performance

#imablanced dataset?
print(length(train$y[train$y=="no"]))
print(length(train$y[train$y=="yes"]))

#Comment whether the model has a good predictive power and which of the measures (accuracy or F1-score) should be preferred here
#We have a imbalanced data set, many more of our classes are of class no then yes. 
#F1 is better when we have an imbalanced data set. https://towardsdatascience.com/a-look-at-precision-recall-and-f1-score-36b5fd0dd3ec
#F1 takes into account imbalane between classes while accuracy does not. See le 2a slide 38. 
#For train data set just always saying no will give an accuracy of somewhere around 87%.

#####################################Task5#############################################
#   Perform a decision tree classification of the test data with the following loss matrix
#            predicted
#Observed     yes   no   
#        yes ( 0    5 )
#         no  (1     0)
#and report the confusion matrix for the test data. 
#Compare the results with the results from step 4 and discuss how the rates has changed and why.
#

#We apply loss matrix to predicted probabilities. 
#lecture 2a slide 7 - the same matrix and principle
LossMatricePredict = predict(bestTree, newdata = test)
weightedLosspred = ifelse(LossMatricePredict[,1]/LossMatricePredict[,2] > 5, "no", "yes")
#build confusion matrix between prediction with loss matrix and real test data
CMLossMatriceTree = table(yreal=test$y,ypred=weightedLosspred)
print(CMLossMatriceTree)
accuracy1 <- sum(diag(CMLossMatriceTree)) / sum(CMLossMatriceTree)
cat(accuracy1*100,"%")
#Precision: Correct positive predictions relative to total positive predictions
precision1 = CMLossMatriceTree[2,2]/(CMLossMatriceTree[2,2]+CMLossMatriceTree[2,1])
#Recall: Correct positive predictions relative to total actual positives
recall1=CMLossMatriceTree[2,2]/(CMLossMatriceTree[2,2]+CMLossMatriceTree[1,2])

F1score1=2*recall1*precision1/(recall1+precision1)
cat(F1score1*100,"%")

#The accuracy of the prediction with loss matrix is slightly worse.
#But precision got better and F1 score has improved. So the last model is better.
#

#Question. We understand that we should apply the lossfunction when we grow/create the tree, but we couldn't do it with 
#tree(), got errors when trying to use weights=. so we imported rpart() and used the loss function in that package. 
#However are we allowed t o utilize that particular package?
#If we are should we calculate the previous trees with rpart? Do we get comparable results. 
#APLLY Loss Matrix to probabilities. Check the slides!

####################################Task6###############################################
#Use the optimal tree and a logistic regression model to classify the test data by using the following principle:
#Yhat= yes if   P(Y=yes|X)>pi  and no otherwise
#where 𝜋=0.05, 0.1, 0.15,…0.9, 0.95. 
#Compute the TPR and FPR values for the two models and plot the corresponding ROC curves. 
#Conclusion? Why precision-recall curve could be a better option here?

logisticRegressionModel=glm(as.factor(train$y)~.,data=train,family=binomial())
vectorOfπ=seq(0.05, .95, by=0.05)


#logisticRegressionModel=glm(as.factor(train$y)~.,data=train,family=binomial())
#vectorOfπ=seq(0.05, .95, by=0.05)


#We want to classify our prediction by pi, so if our predict > pi then classify it as "yes" otherwise "no"
TPRforTree = c()
FPRforTree = c()

for(i in 1:19) {
  
  predictionTree2 = predict(object=bestTree, newdata = test)
  # we check if predictionTree2 for "Yes" > Each π
  YHatclassifiedTree= ifelse (predictionTree2[,2] > vectorOfπ[i] , "yes", "no")
  print(vectorOfπ[i])
  #length(YHatclassifiedTree)
  # we should have a CM to get TPR and FPR
  #CMforTree=matrix(0, 2, 2)
  CMforTree= table(ytraintrue=test$y ,ytrainpred=factor(YHatclassifiedTree,levels=c("no","yes")))
  print(CMforTree)
  #now we can compute TPR and FPR
  TPRforTree[i] = CMforTree[2,2]/sum(CMforTree[2,])
  FPRforTree[i] = CMforTree[1,2]/sum(CMforTree[1,])
}

#We want to classify our prediction by pi, so if our predict > pi then classify it as "yes" otherwise "no"
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
  scale_color_manual(labels = c("Tree", "LM"), values = c("blue", "red"))#+
  #theme_bw()

#Conclusion LM is the better classifier according to ROC curve. Lecture 2a slide 37
#Lecture 2a slide 39, Precision recall curve is a better option since we have imbalanced classes, there are many more class no then yes. 