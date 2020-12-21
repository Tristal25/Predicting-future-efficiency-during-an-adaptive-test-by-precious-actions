#### Preamble ####
# Purpose: Do modeling and analysis
# Author: Yanlin Li
# Data: 20 December 2020
# Contact: tristal.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Run the script Cleaning data.R

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


######################## Model selection ############################

# Logistic
mod.logi = glm(y.train~., data = train, family = 'binomial')
pred.logi = predict(mod.logi, x.test)
pred.logi = ifelse(pred.logi >= 0.5, 1,0)
score.logi = AggregatedScore(pred.logi, y.test)

# Ridge
mod.ridge = glmnet(x = as.matrix(x.train), y = y.train, alpha = 0, family = 'binomial')
mod.cvridge = cv.glmnet(x = as.matrix(x.train), y = y.train, alpha = 0, family = 'binomial')
bestlambda.ridge <- mod.cvridge$lambda.min
pred.ridge = predict(mod.ridge, as.matrix(x.test), type = 'response', s = bestlambda.ridge)
pred.ridge = ifelse(pred.ridge >= 0.5, 1, 0)
score.ridge = AggregatedScore(pred.ridge, y.test)

# Lasso
mod.lasso = glmnet(x = as.matrix(x.train), y = y.train, alpha = 1, family = 'binomial')
mod.cvlasso = cv.glmnet(x = as.matrix(x.train), y = y.train, alpha = 1, family = 'binomial')
bestlambda.lasso <- mod.cvlasso$lambda.min
pred.lasso = predict(mod.lasso, as.matrix(x.test), type = 'response', s = bestlambda.lasso)
pred.lasso = ifelse(pred.lasso >= 0.5, 1, 0)
score.lasso = AggregatedScore(pred.lasso, y.test)

# Assessment
table.score = tibble(
  AggregateScore = round(c(score.logi$AggScore, score.ridge$AggScore, score.lasso$AggScore),4),
  AdjustedKappa = round(c(score.logi$adjKappa, score.ridge$adjKappa, score.lasso$adjKappa),4),
  AdjustedAUC = round(c(score.logi$adjAUC, score.ridge$adjAUC, score.lasso$adjAUC),4),
  Accuracy = round(c(mean(pred.logi == y.test), mean(pred.ridge == y.test), mean(pred.lasso == y.test)),4)
)
rownames(table.score) = c('Logistic Regression', 'Ridge', 'LASSO')
colnames(table.score) = c("Aggregate Score", "Adjusted Kappa", "Adjusted AUC", "Accuracy")
kable(table.score, caption = "Performances of the models")

################### Visualize Chosen model ####################

actions = strsplit(names(x.train), "\\.")
access = c("numact")
type = c("numact")
for (i in 2:25){
  access = c(access, actions[[i]][1])
  type = c(type, actions[[i]][2])
}
coef = tibble(actions = c("(Intercept)", access), 
              itemtypes = c("(Intercept)", type), 
              coefficients = as.numeric(coef(mod.ridge, bestlambda.ridge)))
kable(coef, 
      caption = "Coefficients of logistic ridge regression model", 
      digits = 2)

class.coef =tibble(
  range = c("coef < -1", "-1 < coef < 0", "0 < coef < 0.1", "0.1 < coef < 0.3", "0.3 < coef < 0.5", "0.5 < coef < 1", "coef > 1"), 
  actions = c(str_c(as.character(unlist(select(filter(coef, coefficients < -1), actions))),collapse = ","),
              str_c(as.character(unlist(select(filter(coef, coefficients > -1 & coefficients < 0), actions))),collapse = ","), 
              str_c(as.character(unlist(select(filter(coef, coefficients > 0 & coefficients < 0.1), actions))),collapse = ","), 
              str_c(as.character(unlist(select(filter(coef, coefficients > 0.1 & coefficients < 0.3), actions))),collapse = ","), 
              str_c(as.character(unlist(select(filter(coef, coefficients > 0.3 & coefficients < 0.5), actions))),collapse = ","), 
              str_c(as.character(unlist(select(filter(coef, coefficients > 0.5 & coefficients < 1), actions))),collapse = ","), 
              str_c(as.character(unlist(select(filter(coef, coefficients > 1), actions))),collapse = ",")), 
  itemtypes = c(str_c(unique(as.character(unlist(select(filter(coef, coefficients < -1), itemtypes)))),collapse = ","),
                str_c(unique(as.character(unlist(select(filter(coef, coefficients > -1 & coefficients < 0), itemtypes)))),collapse = ","), 
                str_c(unique(as.character(unlist(select(filter(coef, coefficients > 0 & coefficients < 0.1), itemtypes)))),collapse = ","), 
                str_c(unique(as.character(unlist(select(filter(coef, coefficients > 0.1 & coefficients < 0.3), itemtypes)))),collapse = ","), 
                str_c(unique(as.character(unlist(select(filter(coef, coefficients > 0.3 & coefficients < 0.5), itemtypes)))),collapse = ","), 
                str_c(unique(as.character(unlist(select(filter(coef, coefficients > 0.5 & coefficients < 1), itemtypes)))),collapse = ","), 
                str_c(unique(as.character(unlist(select(filter(coef, coefficients > 1), itemtypes)))),collapse = ","))
  
)

kable(select(class.coef, c(range, actions)), caption = "Actions with coefficient ranges")

kable(select(class.coef, c(range, itemtypes)), caption = "Itemtypes with coefficient ranges")


table.type = coef %>% 
  group_by(itemtypes) %>% 
  summarize(num = length(coefficients), 
            min_coef = min(coefficients), 
            max_coef = max(coefficients), 
            sd_coef = sd(coefficients), 
            mean_coef = mean(coefficients))


kable(table.type, col.names = c("item types",  "number of actions", "minimum", "maximum", "sd", "mean"), caption = "Distribution of coefficients for different item types", digits = 2)








