#lab12: Classifier ensembles - part 1
#author: Andrzej Janusz
#translated by Marcin Szczuka

#1. Example of classifier ensemble.
#2. Bagging in R.
#3. Boosting.


#Motivational example:

#Let's assume that the proper class assignment is given by trueClass
trueClass = as.integer(sample(rep(c(T,F),100),200))

#We managed to build several classification models, but none of them really good
#Predictions provided by these models have been stored on a list - predsList
prediction = function(x) {idx = sample(1:length(x), floor(length(x)/2) - 1);
                          x[idx] = -1*(x[idx] - 1);  return(x)}

predsList = replicate(100000, prediction(trueClass), simplify = F)

#Accuracy of all models is exactly  0.505 - not impressive
summary(sapply(predsList, function(x,y) return(mean(x == y)), trueClass))

#What will be the accuracy if we join the models into ensemble?

#Let's try the majority voting (we average answers):
ensemblePreds = integer(length(trueClass))
for(i in 1:length(predsList)) ensemblePreds = ensemblePreds + predsList[[i]]
ensemblePreds = round(ensemblePreds/length(predsList))

#With quite good probability we get a perfect prediction:
mean(ensemblePreds == trueClass)

#Why?

#Various techniques for ensemble classification concentrate on building accurate
#but at the same time independant prediction models.

#Bagging (bootstrap aggregating).

#We diversify member models by training on different (randomly selected) data subset.
library(nnet)
dataSet = read.table(file = "wdbc.data", header = F, sep=',', row.names=1, na.strings="?")
rownames(dataSet) = NULL
colnames(dataSet) = c("decision",
                      paste(c(rep("mean",10), rep("SE",10), rep("worst",10)),
                            rep(c("radius", "texture", "perimeter", "area",
                                  "smoothness", "compactness", "concavity",
                                  "concave_points", "symmetry", "fractal_dimension"),3),
                            sep="_"))
trueClass = as.integer(dataSet$decision == "M")
dataSet = dataSet[-1]

minList = lapply(dataSet, mean)
sdList = lapply(dataSet, sd)
dataSet = (dataSet - minList)/sdList

trainingIdx = sample(1:nrow(dataSet), floor(nrow(dataSet)*0.67))
trainingSet = dataSet[trainingIdx,]
trainingCls = trueClass[trainingIdx]

nnetModel = nnet(trainingSet, trainingCls, size = 2, entropy = T, trace = F)

singleModelPreds = predict(nnetModel, dataSet[-trainingIdx,], type = "class")

#Accuracy:
mean(singleModelPreds == trueClass[-trainingIdx])

#bagged models:
baggedNNet = function(dataS, clsVec) {
    bootstrapIdx = sample(1:nrow(dataS), nrow(dataS), replace=T)
    mSize = sample(1:3,1)
    return(nnet(dataS[bootstrapIdx,], clsVec[bootstrapIdx], size = mSize, entropy = T, trace = F))
}

modelList = replicate(100, baggedNNet(trainingSet, trainingCls), simplify=F)

modelPreds = lapply(modelList, predict, dataSet[-trainingIdx,], type = "class")

#What is the quality of component models?
#Accuracy:
accVec = sapply(modelPreds, function(x,y) return(mean(x == y)), trueClass[-trainingIdx])
summary(accVec)

ensemblePreds = integer(length(trueClass[-trainingIdx]))
for(i in 1:length(modelPreds)) ensemblePreds = ensemblePreds + as.integer(modelPreds[[i]])
ensemblePreds = round(ensemblePreds/length(modelPreds))

round(mean(ensemblePreds == trueClass[-trainingIdx]),4)

#Obviously, this is not the only possible voting (consensus) mechanism ...

#Tasks:
#Calculate classification outcome for the ensemble above with:
#T1. voting, such that each model's vote is multiplied by a weight corresponding
# to its estimeted quality on validation sample. Use accuracy and Laplace estimates. 
# Validation sample consists of the training objects that were not used in construction of the particular component classifier.

#2. voting, such that each model's vote is multiplied by a weight corresponding
#   to certainty of the given model for the given object.
install.packages("nnet")
library(nnet)
dataSet = read.table(file = "wdbc.data", header = F, sep=',', row.names=1, na.strings="?")
rownames(dataSet) = NULL
colnames(dataSet) = c("decision",
                      paste(c(rep("mean",10), rep("SE",10), rep("worst",10)),
                            rep(c("radius", "texture", "perimeter", "area",
                                  "smoothness", "compactness", "concavity",
                                  "concave_points", "symmetry", "fractal_dimension"),3),
                            sep="_"))
trueClass = as.integer(dataSet$decision == "M")
dataSet = dataSet[-1]

dataSet = (dataSet - lapply(dataSet, mean))/lapply(dataSet, sd)

trainingIdx = sample(1:nrow(dataSet), floor(nrow(dataSet)*0.67))
trainingSet = dataSet[trainingIdx,]
trainingCls = trueClass[trainingIdx]

nnetModel = nnet(trainingSet, trainingCls, size = 2, entropy = T, trace = F)

singleModelPreds = predict(nnetModel, dataSet[-trainingIdx,], type = "class")

mean(singleModelPreds == trueClass[-trainingIdx])

baggedNNet = function(dataS, clsVec) {
  bootstrapIdx = sample(1:nrow(dataS), nrow(dataS), replace=T)
  mSize = sample(1:3,1)
  
  trainDS = dataS[bootstrapIdx,]
  trainCl = clsVec[bootstrapIdx]
  testDS = dataS[-bootstrapIdx,]
  testCl = clsVec[-bootstrapIdx]
  
  net = nnet(trainDS, trainCl, size = mSize, entropy = T, trace = F)
  locPred = predict(net, testDS, type = "class")
  
  Nd = sum(locPred == testCl)
  N = length(testCl)
  accWage = Nd / N
  lapWage = (Nd + 1) / (N + length(unique(trainingCls)))
  
  return(list(net, accWage, lapWage))
}

modelWageList = replicate(100, baggedNNet(trainingSet, trainingCls), simplify=F)
modelList   = sapply(modelWageList, function(x) x[1])
accWageList = sapply(modelWageList, function(x) x[2])
lapWageList = sapply(modelWageList, function(x) x[3])

modelPreds = lapply(modelList, predict, dataSet[-trainingIdx,], type = "class")

accVec = sapply(modelPreds, function(x,y) return(mean(x == y)), trueClass[-trainingIdx])
summary(accVec)


accWageList = sapply(accWageList, function(x) return(x / mean(unlist(accWageList))))
lapWageList = sapply(lapWageList, function(x) return(x / mean(unlist(lapWageList))))


ensemblePreds = integer(length(trueClass[-trainingIdx]))
for(i in 1:length(modelPreds)) ensemblePreds = ensemblePreds + as.integer(modelPreds[[i]])
ensemblePreds = round(ensemblePreds/length(modelPreds))

ensembleAccPreds = integer(length(trueClass[-trainingIdx]))
ensembleLapPreds = integer(length(trueClass[-trainingIdx]))
for(i in 1:length(modelPreds)) ensembleAccPreds = ensembleAccPreds + ((accWageList[[i]] ** 15) * as.integer(modelPreds[[i]]))
for(i in 1:length(modelPreds)) ensembleLapPreds = ensembleLapPreds + ((lapWageList[[i]] ** 15) * as.integer(modelPreds[[i]]))
ensembleAccPreds = round(ensembleAccPreds/length(modelPreds))
ensembleLapPreds = round(ensembleLapPreds/length(modelPreds))


round(mean(ensemblePreds == trueClass[-trainingIdx]),4)

round(mean(ensembleAccPreds == trueClass[-trainingIdx]),4)
round(mean(ensembleLapPreds == trueClass[-trainingIdx]),4)

#In order to make the ensemble more  "interesting" we can train component classifers on randomly selected subsets of attributes.

#Boosting

#Main principle: Each training object has an associated weight. In subsequent iterations we create a component model (e.g. decision tree)
#and update weights in such a way that wrongly classified objects are made more "important" (higher weight) 
# Final prediction is calculated as a weighted sum of component predictions.


#Example of AdaBoost implementation for ANN. AdaBoost uses exponential weight update.
boostedNNet = function(dataS, clsVec, N = 100, infinityThreshold = 100) {
  weightsVec = rep(1/nrow(dataS), nrow(dataS))
  boostingModelList = list()
  boostingModelWeights = numeric()
  
  for(i in 1:N) {
    boostingModelList[[i]] = nnet(dataS, clsVec, weights = weightsVec, size = 2, entropy = T, trace = F)
    tmpPreds = predict(boostingModelList[[i]], dataS, type="class")
    expErr = weightsVec %*% as.integer(tmpPreds != clsVec)
    if(expErr > 0)  {
      boostingModelWeights[i] = 0.5*log((1-expErr)/expErr)
      updateExpVec = weightsVec * exp(-boostingModelWeights[i]*((2*clsVec-1) * (2*as.integer(tmpPreds)-1)))
      weightsVec = updateExpVec/sum(updateExpVec)
    } else break
  }
  if(length(boostingModelList) > length(boostingModelWeights))
    boostingModelWeights[length(boostingModelWeights)+1] = infinityThreshold
  return(list(models = boostingModelList, weights = boostingModelWeights))
}

predict.boostedNNet = function(modelList, newData)  {
  predsList = lapply(modelList$models, predict, newData, type="class")
  ensemblePreds = numeric(nrow(newData))
  for(i in 1:length(modelList$models))
    ensemblePreds = ensemblePreds + ((2*as.numeric(predsList[[i]])-1) * modelList$weights[i])
  return(as.integer(ensemblePreds > 0))
}

set.seed(1)
boostingModelList = boostedNNet(trainingSet, trainingCls, N = 5)

boostedPreds = predict.boostedNNet(boostingModelList, dataSet[-trainingIdx,])
mean(trueClass[-trainingIdx] == boostedPreds)

trainingPreds = predict.boostedNNet(boostingModelList, dataSet[trainingIdx,])
mean(trueClass[trainingIdx] == trainingPreds)






