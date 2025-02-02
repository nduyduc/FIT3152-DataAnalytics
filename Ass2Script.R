install.packages("tree")
install.packages("ROCR")
install.packages("e1071")
install.packages("adabag")
install.packages("randomForest")

# setwd("~/Desktop/FIT3152/Assignment2")
rm(list = ls())
WAUS <- read.csv("WAUS2019.csv")
L <- as.data.frame(c(1:49))
set.seed(28378210) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows
head(WAUS)

# question 1 - preprocessing data

WAUS <- WAUS[(!is.na(WAUS$RainTomorrow)),]

str(WAUS)
attach(WAUS)
contrasts(WindGustDir) = contr.treatment(16)
contrasts(WindDir9am) = contr.treatment(16)
contrasts(WindDir3pm) = contr.treatment(16)
contrasts(RainToday) = contr.treatment(2)

summary(WAUS)

WAUS$Evaporation <- NULL
WAUS$Sunshine <- NULL

summary(WAUS)

WAUS.rain = WAUS[(WAUS$RainTomorrow == "Yes"),]
WAUS.norain = WAUS[(WAUS$RainTomorrow == "No"),]

mean.loc = aggregate(
  cbind(MinTemp, MaxTemp, Rainfall, WindGustSpeed, WindSpeed9am, WindSpeed3pm, Humidity9am, 
        Humidity3pm, Pressure9am, Pressure3pm, Temp9am, Temp3pm) 
        ~ Location, WAUS, FUN = mean, na.action = na.omit)

print(mean.loc)

mean.loc.cloud = aggregate(cbind(Cloud9am, Cloud3pm) ~ Location, WAUS, FUN = mean)

print(mean.loc.cloud)

# question 2 - split train test data

train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]

# question 3, 4, 5, 6 - classification models

######## TREE ########
library(tree)
library(ROCR)
WAUS.tree = tree(RainTomorrow ~ ., data = WAUS.train)
print(summary(WAUS.tree))
plot(WAUS.tree)
text(WAUS.tree, pretty = 0)

# confusion matrix
WAUS.pred.tree = predict(WAUS.tree, WAUS.test, type = "class")
conf.tree = table(Predicted_Class = WAUS.pred.tree, Observed_Class = WAUS.test$RainTomorrow)
print(conf.tree)

# ROC curve
WAUS.pred.tree = predict(WAUS.tree, WAUS.test, type = "vector")
WAUS.tree.prob = prediction(WAUS.pred.tree[,2], WAUS.test$RainTomorrow)
WAUS.tree.perf = performance(WAUS.tree.prob, "tpr", "fpr")
plot(WAUS.tree.perf)
abline(0, 1)

# calculate AUC
WAUS.tree.auc = performance(WAUS.tree.prob, "auc")
print(as.numeric(WAUS.tree.auc@y.values))

######## NAIVE BAYES ########
library(e1071)
WAUS.naive = naiveBayes(RainTomorrow ~ ., data = WAUS.train)
WAUS.pred.naive = predict(WAUS.naive, WAUS.test)

# confusion matrix
conf.naive = table(Predicted_Class = WAUS.pred.naive, Observed_Class = WAUS.test$RainTomorrow)
print(conf.naive)

# ROC curve
WAUS.pred.naive = predict(WAUS.naive, WAUS.test, type = "raw")
WAUS.naive.prob = prediction(WAUS.pred.naive[,2], WAUS.test$RainTomorrow)
WAUS.naive.perf = performance(WAUS.naive.prob, "tpr", "fpr")
plot(WAUS.naive.perf, col = "red", add = TRUE)

# calculate AUC
WAUS.naive.auc = performance(WAUS.naive.prob, "auc")
print(as.numeric(WAUS.naive.auc@y.values))

######## BAGGING ########
library(adabag)
WAUS.bag = bagging(RainTomorrow ~ ., data = WAUS.train)
WAUS.pred.bag = predict.bagging(WAUS.bag, WAUS.test)

# confusion matrix
print(WAUS.pred.bag$confusion)

# ROC curve
WAUS.bag.prob = prediction(WAUS.pred.bag$prob[,2], WAUS.test$RainTomorrow)
WAUS.bag.perf = performance(WAUS.bag.prob, "tpr", "fpr")
plot(WAUS.bag.perf, col = "green", add = TRUE)

# calculate AUC
WAUS.bag.auc = performance(WAUS.bag.prob, "auc")
print(as.numeric(WAUS.bag.auc@y.values))

######## BOOSTING ########
WAUS.boost = boosting(RainTomorrow ~ ., data = WAUS.train)
WAUS.pred.boost = predict.boosting(WAUS.boost, WAUS.test)

# confusion matrix
print(WAUS.pred.boost$confusion)

# ROC curve
WAUS.boost.prob = prediction(WAUS.pred.boost$prob[,2], WAUS.test$RainTomorrow)
WAUS.boost.perf = performance(WAUS.boost.prob, "tpr", "fpr")
plot(WAUS.boost.perf, col = "blue", add = TRUE)

# calculate AUC
WAUS.boost.auc = performance(WAUS.boost.prob, "auc")
print(as.numeric(WAUS.boost.auc@y.values))

######## RANDOM FOREST ########
library(randomForest)
WAUS.rforest = randomForest(RainTomorrow ~ ., data = WAUS.train, na.action = na.exclude)
WAUS.pred.rforest = predict(WAUS.rforest, WAUS.test)

# confusion matrix
conf.rforest = table(Predicted_Class = WAUS.pred.rforest, Observed_Class = WAUS.test$RainTomorrow)
print(conf.rforest)

# ROC curve
WAUS.pred.rforest = predict(WAUS.rforest, WAUS.test, type = "prob")
WAUS.rforest.prob = prediction(WAUS.pred.rforest[,2], WAUS.test$RainTomorrow)
WAUS.rforest.perf = performance(WAUS.rforest.prob, "tpr", "fpr")
plot(WAUS.rforest.perf, col = "yellow", add = TRUE)

# calculate AUC
WAUS.rforest.auc = performance(WAUS.rforest.prob, "auc")
print(as.numeric(WAUS.rforest.auc@y.values))


# question 7 - attribute importance

# decision tree
print(summary(WAUS.tree))

# naive bayes


# bagging
print(WAUS.bag$importance)

# boosting
print(WAUS.boost$importance)

# random forest
print(WAUS.rforest$importance)
