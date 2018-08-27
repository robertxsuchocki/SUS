#lab13: Random forests
#author: Andrzej Janusz
#translated by Marcin Szczuka

#1. Tree classifiers and their weaknesses.
#2. Bagging trees.
#3. Random feature selection in bagged trees.


#Motivational example:

#load required libraries ? rpart for classification and regression trees
#install.packages("rpart")
library(rpart)

#mlbench for benchmark datasets
#install.packages("mlbench")
library(mlbench)

# Bagging decision trees
#install.packages("adabag")
library(adabag)

#load required package ? randomForest
#install.packages("randomForest")
library(randomForest)

#gbm -  Generalized Boosting Models 
#install.packages("gbm")
library(gbm)

#c50 - andvanced decision trees and rule models
#install.packages("C50")
library(C50)


#load Glass data set
data('Glass')
?Glass
dim(Glass)
names(Glass)

#set seed to ensure reproducible results
set.seed(42)

#split into training and test sets
Glass[,"train"] = ifelse(runif(nrow(Glass))<0.8,1,0)

#separate training and test sets
trainGlass= Glass[Glass$train==1,]
testGlass= Glass[Glass$train==0,]

#get column index of train flag
trainColNum= grep("train",names(trainGlass))

#remove train flag column from train and test sets
trainGlass <- trainGlass[,-trainColNum]
testGlass <- testGlass[,-trainColNum]

#get column index of predicted variable in dataset
typeColNum= grep("Type",names(Glass))

#build tree model
rpart_model= rpart(Type ~.,data = trainGlass, method="class")

#show the resulting tree
plot(rpart_model);
text(rpart_model)

#?and the moment of truth
rpart_predict= predict(rpart_model,testGlass[,-typeColNum],type="class")
mean(rpart_predict==testGlass$Type)

#Why is this tree so bad?
#Well, decision tree algorithms tend to display high variance so the hit rate from any one tree is likely to be misleading.
#So, we build a bunch of them and average results.


#function to do multiple iterations
multiple_runs <- function(train_fraction,n,dataset){
  fraction_correct = rep(NA,n)
  set.seed(42)
  for (i in 1:n){
    dataset[,"train"] = ifelse(runif(nrow(dataset))<0.8,1,0)
    trainColNum = grep("train",names(dataset))
    typeColNum = grep("Type",names(dataset))
    trainset = dataset[dataset$train==1,-trainColNum]
    testset = dataset[dataset$train==0,-trainColNum]
    rpart_model = rpart(Type~.,data = trainset, method="class")
    rpart_test_predict = predict(rpart_model,testset[,-typeColNum],type="class")
    fraction_correct[i] = mean(rpart_test_predict==testset$Type)
  }
  return(fraction_correct)
}

# now we do 50 runs, no pruning of constructed trees
n_runs = multiple_runs(0.8,50,Glass)

# and we get on average
mean(n_runs)
# with deviation
sd(n_runs)

# this does not seem to work properly, we need more sophisticated approach.

# Bagging decision trees

l <- length(Glass[,1])
sub <- sample(1:l,2*l/3)
GlassBag = bagging(Type~.,data=Glass[,-1],mfinal=50, control=rpart.control(maxdepth=3))
GlassBag.pred = predict.bagging(GlassBag,newdata=Glass[-sub,-1])
GlassBag.pred$confusion
GlassBag.pred$error

# Better, but still not great.
# The problem is that our forest consists of almost identical trees.
# To deal with that we randomly change the attribute sets in subsequent iterations.
# So, the Random Forest is created.

#set seed to ensure reproducible results
set.seed(42)

#Now we build an ensemble model
Glass.rf <- randomForest(Type ~.,data = trainGlass, importance=TRUE, xtest=testGlass[,-typeColNum],ntree=50)

#Summary of our forest
Glass.rf

#accuracy for test set
mean(Glass.rf$test$predicted==testGlass$Type)

#confusion matrix
table(Glass.rf$test$predicted,testGlass$Type)


# We can trace the importance of variables (attributes)
#by measuring their influence on accuracy and their Gini index

varImpPlot(Glass.rf)

#Task 1:
#Using 5-fold cross validation on  "WBC" data compare the  accuracy of model obtained from randomForest with  gbm (version of adaBoost) 
#that uses rpart trees as  component models and with a C5.0 tree.

library(rpart)
library(mlbench)
library(adabag)
library(randomForest)
library(gbm)
library(C50)

dataSet = read.table(file = "wdbc.data", header = F, sep=',', row.names=1, na.strings="?")
rownames(dataSet) = NULL
colnames(dataSet) = c("decision",
                      paste(c(rep("mean",10), rep("SE",10), rep("worst",10)),
                            rep(c("radius", "texture", "perimeter", "area",
                                  "smoothness", "compactness", "concavity",
                                  "concave_points", "symmetry", "fractal_dimension"),3),
                            sep="_"))
decisionColNum = grep("decision",names(dataSet))

set.seed(42)
dataSet = dataSet[sample(nrow(dataSet)),]
folds = cut(seq(1,nrow(dataSet)),breaks=5,labels=FALSE)
rf_mean = 0
gbm_mean = 0
cfive_mean = 0
for(i in 1:5){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = dataSet[testIndexes,]
  trainData = dataSet[-testIndexes,]
  
  rf_model = randomForest(decision ~., data=trainData, importance=TRUE, xtest=testData[,-decisionColNum], ntree=50)
  rf_mean = rf_mean + (mean(rf_model$test$predicted==testData$decision) / 5)
  
  cfive_model = C50::C5.0(trainData[,-decisionColNum], trainData[,decisionColNum])
  cfive_pred = predict(cfive_model, testData[,-decisionColNum])
  cfive_mean = cfive_mean + (mean(cfive_pred==testData$decision) / 5)
  
  trainData$decision = as.integer(trainData$decision == "M")
  testData$decision = as.integer(testData$decision == "M")
  gbm_model = gbm(decision ~., data = trainData, shrinkage=0.01, distribution='bernoulli', n.trees=10000)
  gbm_pred = predict(gbm_model, testData[,-decisionColNum], n.trees = 10000)
  gbm_pred = as.integer(gbm_pred > 0.5)
  gbm_mean = gbm_mean + (mean(gbm_pred==testData$decision) / 5)
}

print(rf_mean)
print(gbm_mean)
print(cfive_mean)

#> print(rf_mean)
#[1] 0.9648502
#> print(gbm_mean)
#[1] 0.9754075
#> print(cfive_mean)
#[1] 0.9314547
