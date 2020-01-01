# This file explores methods of classification using:
## data: the original data 
## data: first 20 principal components
## method: SVM (kernlab::ksvm)
## apply k-fold cross-validation to estimate out-of-sample error; where k = 10
## create confusion matrix
## compute percent accuracy of model

# step 1: fit using both PC 1-20 and original training set
## predict the test data set, measure error, evaluate accuracy and compute F1 score
# step 2: determine best model based on results from step 1.

rm(list = ls())
library(kernlab)
library(Metrics)
library(magrittr)


# load principal components and training data
load('data/4Model_pca.rda')

# function to compute "flat" 0-1 error rate
flatErr<-function(x, ref) mean(x != ref)
# function to compute class-specific "flat" prediction errors
classErrs<-function(x, ref) tapply(x != ref, ref, mean)

# transform column names in training data and test data
df.train <- training_data
colnames(df.train)[3:dim(df.train)[2]] <- paste0("V", 3:dim(df.train)[2])
df.train <- df.train[, -2]
df.test <- test_data
colnames(df.test)[3:dim(df.test)[2]] <- paste0("V", 3:dim(df.test)[2])
df.test <- df.test[, -2]

# step 1: fit using both PC 1-20 and original training set
## predict the test data set, measure error, evaluate accuracy and compute F1 score
##############################################
# Fit: original training data
##############################################
system.time(
  mod.o <- ksvm(activity ~., data = df.train, kernel = 'rbfdot',
                   kpar = 'automatic', C = 5, prob.model = TRUE, cross = 10)
)
pred.o <- predict(mod.o, newdata = df.test, type = 'response')
##############################################
# out-of-sample error: original training data
##############################################
# flat error
flatErr.o <- flatErr(x = pred.o, 
                         ref = df.test$activity)
# class-specific error
classErrCV.o <- classErrs(x = pred.o, ref = df.test$activity)
##############################################
# accuracy: original training data
##############################################
cmatrix.o <- table(pred.o, df.test[,'activity'])
totalCorrect.o <- sapply(1:nlevels(df.test$activity),
                         function(x) cmatrix.o[x,x]) %>% sum
accuracy.o <- totalCorrect.o / nrow(df.test)
cmatrix.o
accuracy.o
##############################################
# Fit: Principal Components 1-20
##############################################
my_pca <- prcomp(df.train[,-1], center = TRUE, scale. = TRUE)
df.train.pca <- my_pca$x[,1:20] %>% as.data.frame
df.train.pca$activity <- df.train$activity
df.test.pca <- predict(my_pca, df.test[,-1])[,1:20] %>%
  as.data.frame
df.test.pca$activity <- df.test$activity
system.time(
  mod.pca <- ksvm(activity ~., data = df.train.pca, kernel = 'rbfdot',
                  kpar = 'automatic', C = 5, prob.model = TRUE, cross = 10)
)
pred.pca <- predict(mod.pca, newdata = df.test.pca, type = 'response')
##############################################
# out-of-sample error: Principal Components 1-20
##############################################
# flat error
flatErr.pca <- flatErr(x = pred.pca, 
                       ref = df.test.pca$activity)
# class-specific error
classErrCV.pca <- classErrs(x = pred.pca, ref = df.test.pca$activity)
##############################################
# accuracy: Principal Components 1-20
##############################################
cmatrix.pca <- table(pred.pca, df.test.pca[,'activity'])
totalCorrect.pca <- sapply(1:nlevels(df.test$activity), 
                           function(x) cmatrix.pca[x,x]) %>% sum
accuracy.pca <- totalCorrect.pca / nrow(df.test.pca)
cmatrix.pca
accuracy.pca

# print model summaries
mod.o
mod.pca

##############################################
# Results
##############################################
# By comparing the test set error and accuracy of two SVM models, 
# it is clear that the model fit on the original training data performed the best.
# If a decision to deploy a classification model for ADL using cell-phone data is made,
# we have our preferred model. 
# However, let us further investigate how well the SVM algorithm performs when instead we
# choose to have a binary classifier rather than a multiclass classifier.
# As we saw in the confusion matrix and class errors, there are quite a few observations
# that were "hard to classify" given the class-specific label.
# SVMs are generally described to be binary classifiers. 
# If the business decision is to deploy a model with the least amount of error yet still be
# effective in classifying human activity to some extent, then perhaps a binary classification
# model might be more appropriate.
df.train.b <- df.train
df.train.b$activity <- ifelse(df.train.b$activity %in% c('LAYING','SITTING','STANDING'),
                              'SEDENTARY', 'INMOTION')
df.test.b <- df.test
df.test.b$activity <- ifelse(df.test.b$activity %in% c('LAYING','SITTING','STANDING'),
                             'SEDENTARY', 'INMOTION')
mod.b <- ksvm(activity ~., data = df.train.b, kpar = 'automatic', kernel = 'rbfdot',
              C = 5, cross = 10, prob.model = TRUE)
mod.b
mod.o
# This new binary classifier performs exceedingly better than the multiclass classifier
# from the training error and CV error standpoint. It is also much less complex 
# requiring fewer support vectors.
# predict test set
pred.b <- predict(mod.b, df.test.b[,-1], type = 'response')
# confusion matrix and accuracy of model
cmatrix.b <- table(pred.b, df.test.b[,'activity'])
totalCorrect.b <- sapply(1:nlevels(as.factor(df.test.b$activity)), 
                           function(x) cmatrix.b[x,x]) %>% sum
accuracy.b <- totalCorrect.b / nrow(df.test.b)
# Flat prediction errors of the test set
flatErr.b <- flatErr(x = pred.b, ref = df.test.b$activity)
# Class-specific prediction errors of the test set
classErrCV.b <- classErrs(x = pred.b, ref = df.test.b$activity)
# distribution of support vectors
alpha(mod.b)[[1]] %>% quantile
# none of the support vectors came very close to our maximum cost of constraints value of 5.
# this means that none of the observations violated the maximum cost of constraints and 
# the model did not have difficulty classifying these observations.
## Decide if we should consider tuning the cost parameter
# Arrive at the final model.

save(
  mod.b,
  mod.o,
  mod.pca,
  df.test,
  df.test.b,
  df.test.pca,
  df.train,
  df.train.b,
  df.train.pca,
  accuracy.b,
  accuracy.o,
  accuracy.pca,
  classErrCV.b,
  classErrCV.o,
  classErrCV.pca,
  cmatrix.b,
  cmatrix.o,
  cmatrix.pca,
  flatErr.b,
  flatErr.o,
  flatErr.pca,
  my_pca,
  imp_features_pc1,
  imp_features_pc2,
  pc_eval_df,
  file = 'data/4Model_classification.rda'
)