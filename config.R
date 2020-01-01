# This file specifies the necessary configurations for this project including: 
# - paths to original data 
# - sourcing data tools
# - loading libraries
rm(list = ls())
library(magrittr)
library(ggplot2)
library(reshape2)
library(knitr)
library(gridExtra)
library(caret)
library(ggfortify)
library(tm)
library(wordcloud)
library(stringr)
library(kernlab)
source('code/utils.r')

# file path to original data
file_path_data <- 'data/original_data/uci har dataset'
# file path to original training data (response variable)
file_path_y_train <- 'data/original_data/uci har dataset/train/y_train.txt'
# file path to original training data (predictor variables)
file_path_x_train <- 'data/original_data/uci har dataset/train/x_train.txt'
# file path to original training data (subject IDs)
file_path_id_train <- 'data/original_data/uci har dataset/train/subject_train.txt'
# file path to original test data (response variable)
file_path_y_test <- 'data/original_data/uci har dataset/test/y_test.txt'
# file path to original test data (predictor variables)
file_path_x_test <- 'data/original_data/uci har dataset/test/x_test.txt'
# file path to original test data (subject IDs)
file_path_id_test <- 'data/original_data/uci har dataset/test/subject_test.txt'
# file path to activity labels respective to response variable
file_path_labels <- 'data/original_data/uci har dataset/activity_labels.txt'
# file path to feature vector names respective to predictor variables
file_path_features <- 'data/original_data/uci har dataset/features.txt'

