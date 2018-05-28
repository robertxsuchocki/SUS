#lab8:rough set theory, rule-based classification.
#author: Andrzej Janusz translated by Marcin Szczuka
#email: janusza@mimuw.edu.pl

#1. Discernibility.
#2. Upper and lower approximation.
#3. Discernibility matrix.
#4. Decision reducts.
#5. Rule generation algorithms.

#install.packages("RoughSets")
#(or even better: install_git('https://github.com/janusza/RoughSets'))

install.packages('mlbench')
library(mlbench)

install.packages('devtools')
library(devtools)
install_git('https://github.com/janusza/RoughSets')
library(RoughSets)

#load symbolic data set:
dataSet = read.table(file = "lymphography_processed.data", header = T, sep='\t', row.names=NULL)
dim(dataSet)
colnames(dataSet)

dataSet = cbind(dataSet[-1], class = dataSet[[1]])

#class distribution:
table(dataSet$class)

#let's take look at first two objects (rows):
as.character(dataSet$class[1:2])

dataSet[1:2,]

#The following attributes discern object  1 from 2: 
colnames(dataSet)[which(dataSet[1,] != dataSet[2,])]
#In rough set theory indiscernibility class for an object is a set of all objects that have the same attribute values
#for the selected subset of attributes. We want to describe concepts (by their extensions), i.e., sets of objects with   
#different decisions. To do that we want to use "building blocks" that are discernibility classes. 

anyDuplicated(dataSet[-ncol(dataSet)])

#In case of our data all objects are discernible, their discernibility classes contain exactly one element.
#In this case the description with use of discernibility classes is not helpful. 
#We need to select attribute set in a clever way to remedy that. 

#For starters, we convert our data set to format accepted by RoughSets package.
dataSet = SF.asDecisionTable(dataSet, 
                             decision.attr = ncol(dataSet), 
                             indx.nominal = 1:ncol(dataSet))

attributes(dataSet)

#For the moment we assume that we are only givet the firts 5 attributes.
sum(duplicated(dataSet[1:5]))                     # - now the data has duplicates

sum(duplicated(dataSet[c(1:5, ncol(dataSet))]))   # - there are inconsistent (contradictory) objects

#If there are contadictions in data then we are not able to provide exact decision clss description using discernibility.
#To deal with that rough set theory provides two essential concepts of upper and lower approximation: 
# - lower approximation is a sum of all discernibility classes compeltely contained in a given concept 
# - upper approaximation is a sum of all discernibility classes having non-empty intersection with a given concept

?BC.IND.relation.RST

IND = BC.IND.relation.RST(dataSet, feature.set = 1:5)
IND

#Task 1:
#Find the upper and lower approximation of the decision class "malign lymph" 
#using relation IND (do not use dedicated function from RoughSets package yet).

relation = IND[["IND.relation"]]
class = "malign lymph"
upper_nymph = list()
lower_nymph = list()

for (name in names(relation)) {
  if (class %in% dataSet[relation[[name]],]$class) {
    upper_nymph = c(upper_nymph, name, relation[[name]])
  }
  if (all(dataSet[relation[[name]],]$class == class)) {
    lower_nymph = c(lower_nymph, name, relation[[name]])
  }
}

#Upper and lower approximation can be easile obtained
#using dedicated function from RoughSets package

RSet = BC.LU.approximation.RST(dataSet, IND)

RSet[["lower.approximation"]][["malign lymph"]]


#For any pair of objects we can identify (list) attributes that discern them.
#This makes it possible to construct a square (and usually symmetric) discernibility matrix.

discMatrix = BC.discernibility.mat.RST(dataSet, return.matrix = TRUE)

names(discMatrix)

discMatrix$disc.mat[1:2,1:2]
dim(discMatrix$disc.mat)

head(discMatrix$disc.list)
length(discMatrix$disc.list)

#Why we want to have this matrix?
#Discernibility matrix allows straightforward derivation of all decision reducts for a given decision table.
#Decision reduct is a minimal (w.r.t. containment) set of attributes that is sufficient for achieving the same discernibility
#of objects from different decision classes as the whole attribute set. 
#Decision reducts is the smallest subset of attributes that preserves all neccessary decision-related information.

#The discernibility matrix can be encoded as a boolean function in conjunctive normal form (CNF).
#This is called duiscernibility (information) function. Each CNF formula can be recoded as a  formula in 
#disjunctive normal form (DNF). If we do such recoding, we are able to direcly name all decision reducts, each reduct being a clause in DNF.
#Unfortunately, the cost of recoding CNF to DNF may be prohibitive.


reductList = FS.all.reducts.computation(discMatrix)

names(reductList)
length(reductList$decision.reduct)

reductList$decision.reduct[1:3]
summary(sapply(reductList$decision.reduct, function(x) length(x$reduct)))

#each DNFclause corresponds to decision reducts - typically we are interested in shortest reducts
#In principle, searching for one, shortest reduct for a given decision table is NP-hard.

#The frequency with which attributes appear in reduct may be a guideline regarding reduct's usefulness:
table(unlist(lapply(reductList$decision.reduct, function(x) names(x$reduct))))

#We usually don't need all reducts - one "reasonably good" is often enough.

reduct = FS.one.reduct.computation(discMatrix, greedy = TRUE)
reduct$decision.reduct

#Reducts can be found without discernibility matrix.

#Permutation-based reducts:
FS.reduct.computation(dataSet, method = "permutation.heuristic")

orderingReductList = replicate(500, 
                               FS.reduct.computation(dataSet, method = "permutation.heuristic"),
                               simplify = FALSE)
length(unique(orderingReductList))

orderingReductList = unique(orderingReductList)
orderingReductList = orderingReductList[order(sapply(orderingReductList, function(x) length(x$reduct)))]
orderingReductList[[1]]

#discernibility-based (greedy) reduct 
greedyReduct1 = FS.reduct.computation(dataSet, method = "greedy.heuristic", 
                                      qualityF = X.nOfConflicts, nAttrs = NULL, epsilon = 0)
greedyReduct1

#"probabilistic" reduct
greedyReduct2 = FS.reduct.computation(dataSet, method = "greedy.heuristic", 
                                      qualityF = X.gini, nAttrs = NULL, epsilon = 0)
greedyReduct2

#"entropy" reduct
greedyReduct3 = FS.reduct.computation(dataSet, method = "greedy.heuristic", 
                                      qualityF = X.entropy, nAttrs = NULL, epsilon = 0)
greedyReduct3


#Rule induction algorithms

# RoughSets package contains implementation of several popular rule induction algorithms
#In particular - indiscernibilityBasedRules, CN2, AQ and LEM2.

#Example:  CN2
#CN2 is a greedy, covering-based algorithm. Oznacza to, ze w kazdej iteracji probuje znalezc regule, ktora ma 
#najlepsza jakosc na zbiorze niepokrytych obiektow, a nastepnie usuwa pokryte przez nia obiekty z danych.
#Do oceniania jakosci najczesciej uzywa estymaty Laplace'a.
#Aby ograniczyc przestrzen przeszukiwan przy wybieraniu najlepszej reguly, algorytm CN2 w kazdym kroku (czyli
#przy sprawdzaniu regul o ustalonej dlugosci) "pamieta" jedynie K najwyzej ocenionych regul z kroku poprzedniego.

CN2rules = RI.CN2Rules.RST(dataSet[1:100,], K = 3)
CN2rules

#With rule set generated by CN2 we predict the decision using the "best wins" principle.
#Rules are applied to a new case in the order they were discovered by CN2. Once a rule covers the case its decision stands.

predictions = predict(CN2rules, dataSet[101:nrow(dataSet),])

mean(predictions == as.character(dataSet[101:nrow(dataSet), 'class']))

rm(list = ls()); gc()
