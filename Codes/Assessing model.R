#### Preamble ####
# Purpose: Create a function for aggregate score
# Author: Yanlin Li
# Data: 20 December 2020
# Contact: tristal.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: none

############## Setup workspace #############

library(knitr)
library(tidyverse)
library(rlist)
library(pROC)
library(vcd)
library(glmnet)
library(Matrix)
library(e1071)
library(scales)
library(caret)

################### Aggregate score function ####################

AggregatedScore <- function(predict, test){
  auc = auc(predict, test)
  if (auc < 0.5){
    adj.auc = 0
  }
  else{
    adj.auc = 2*(auc - 0.5)
  }
  confusion = confusionMatrix(as.factor(predict), as.factor(test), positive = '1', dnn = c("Prediction", "Test"))
  kappa = confusion$overall[["Kappa"]]
  if (kappa < 0){
    adj.kappa = 0
  }
  else{
    adj.kappa = kappa
  }
  aggscore = adj.auc + adj.kappa
  r = list(adjAUC = adj.auc, adjKappa = adj.kappa, AggScore = aggscore)
  r
}
