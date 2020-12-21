#### Preamble ####
# Purpose: Clean the data and create features
# Author: Yanlin Li
# Data: 20 December 2020
# Contact: tristal.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have the data

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


########################### Initialize data #################################

InitiateData <- function(problems, data){
  data.new = data.frame(matrix(0, nrow = length(data), ncol = (length(problems)+1)))
  rownames(data.new) <- names(data)
  colnames(data.new) <- c('numprob', problems)
  
  #Extracting data
  for (i in 1:length(data)){
    student = data[[i]]
    done = c()
    for (j in 1:(length(student[[1]])-1)){
      prob = student[[3]][j]
      if (prob %in% problems){
        ind = which(prob == problems)
        if (isTRUE((student[[8]][j] < 1800) && (student[[8]][j+1] < 1800))){
          data.new[i,(ind+1)] = data.new[i,(ind+1)] + (student[[8]][j+1] - student[[8]][j])
        }
        else if (isTRUE((student[[8]][j] < 1800) && (student[[8]][j+1] >= 1800))){
          data.new[i,(ind+1)] = data.new[i,(ind+1)] + (1800 - student[[8]][j])
        }
        if (isTRUE((!(prob %in% done)) && (student[[8]][j] < 1800))){
          data.new[i,1] = data.new[i,1] + 1
          done = c(prob, done)
        }
      }
    }
    last = student[[3]][length(student[[1]])]
    if (isTRUE((student[[8]][length(student[[1]])] < 1800) && (last %in% problems))){
      ind.l = which(last == problems)
      data.new[i,(ind+1)] = data.new[i,(ind+1)] + (1800 - student[[8]][length(student[[1]])])
      if (!(last %in% done)){
        data.new[i,1] = data.new[i,1] + 1
        done = c(last, done)
      }
    }
  }
  return(data.new)
}

########################## Normalization #################################

Normalize <- function(data){
  data.new = data
  for (i in 1:length(data)){
    x = data[[i]]
    mean = mean(x)
    sd = sd(x)
    x.new = c()
    for (j in 1:length(x)){
      perc = pnorm(x[j], mean,sd)
      x.new = append(x.new, perc, after = length(x.new))
    }
    data.new[[i]] = x.new
  }
  return(data.new)
}


###################### Apply the functions #####################

load("data/train_log_by_id.RData")
data_train_label=read.csv('data/data_train_label.csv')
x.train.orig=train_log_by_id[1:1000]
x.test.orig=train_log_by_id[1001:1232]
y.train.orig=data_train_label[1:1000,]
y.test.orig=data_train_label[1001:1232,]
y.train = y.train.orig[,2]
y.test = y.test.orig[,2]
y.train = ifelse(y.train == 'True', 1,0)
y.test = ifelse(y.test == 'True', 1,0)

items= c()
for (i in 1:1000){
  student=x.train.orig[[i]]
  for (j in 1:length(student[[1]])){
    elem = paste(student[[3]][j], student[[4]][j])
    if (!(elem %in% items)) {
      items = append(items, elem, after = length(items))
    }
  }
}

x.train.r = InitiateData(items, x.train.orig)

x.test.r = InitiateData(items, x.test.orig)

x.train = data.frame(Normalize(x.train.r))

x.test = data.frame(Normalize(x.test.r))

train = data.frame(cbind(x.train, y.train))






