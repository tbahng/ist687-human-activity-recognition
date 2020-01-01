# This file performs principal component analysis on the training data to:
## create principal components
## examine feature importance
## evaluate the effectiveness of principal components
source('config.r')
# load extracted data
load('data/2Transform.rda')

# Principal Components Analysis
# Objective1: Identify dimensions that are more important than others.
# Objective2: Evaluate the effectiveness of principal components
# Create principal components analysis (feature extraction / feature reduction)
# creation of new variables made up of many of my variables
# ref: https://www.datacamp.com/community/tutorials/pca-analysis-r
# ref: https://www.youtube.com/watch?v=qhvkVxuwvLk
# ref: https://stackoverflow.com/questions/42325276/how-to-use-pca-on-test-set-code
###########################################################
# Pre-Processing for PCA
###########################################################
df <- training_data[,3:dim(training_data)[2]]
# Are any variables integer values? Use modular division
any(sapply(df, function(x) all(x%%1==0)))
# identify near zero variance predictors; these nzv features can be problematic for pca
nzv <- nearZeroVar(df, saveMetrics = TRUE)
print(paste('Range:', range(nzv$percentUnique)))
# any features with 0 or close to 0 should be removed; use 0.1 as threshold
dim(nzv)
dim(nzv[nzv$percentUnique > 0.1,])
head(nzv)

# It appears that no variables that zero or near-zero variance that exceed tolerance.
df_nzv <- df[c(rownames(nzv[nzv$percentUnique > 0.1, ]))]
###########################################################
# Perform PCA
###########################################################
my_pca <- prcomp(df_nzv, center = TRUE, scale. = TRUE)
# each principal component is a normalized linear combination of the original 561 features
###########################################################
# Identify dimensions that are more important than others.
###########################################################
# which features are most important for Principal Component 1? Top 6
imp_features_pc1 <- sort(abs(my_pca$rotation[,"PC1"])) %>% 
  names %>% tail
imp_features_pc1
# which features are most important for Principal Component 2? Top 6
imp_features_pc2 <- sort(abs(my_pca$rotation[,"PC2"])) %>% 
  names %>% tail
imp_features_pc2
# based on results, it doesn't appear that any of the features load very high on the components.
# biplot of PC1 and PC2
pc_biplot <- autoplot(my_pca, label = FALSE, loadings.label = TRUE) +
  theme_bw() +
  ggtitle("Biplot of First Two Principal Components")
pc_biplot
# Screeplot of variance explained by principal components
pc_screeplot <- screeplot(my_pca, col = 'blue', npcs = 5,
                          main = 'Screeplot of First 5 Principal Components vs. Explained Variance')
pc_screeplot
# Summary of PCA Showing First 5 Principal Components
pcaPrint(summary(my_pca), n = 10)

###########################################################
# Evaluate the effectiveness of principal components - Label = 'WALKING'
###########################################################
Label = 'WALKING'
# Create a separate vector of labels associated with observations
myLabels <- training_data$activity
# Because we'll be using a logistic regression model to evaluate the effectiveness of PCs
# I will convert my labels into a more suitable binary classification
myLabels <- ifelse(myLabels == Label, 1, 0)
# bind newly cleaned dataset with labels
dfEvaluate <- cbind(as.data.frame(sapply(df_nzv, as.numeric)),
                    cluster = myLabels)

# in the video he evaluated the AUC using custom function EvaluateAUC
# The AUC is the area under the ROC (receiver-operator characteristic curve)
# This is the probability that a randomly chosen positive observation has a 
# higher predicted value than a randomly chosen negative value.
# my data has multiple level factor; his code has binary label.
# Therefore I can only test with one level at a time.
orig_results <- EvaluateAUC(dfEvaluate)
orig_results$dataset <- 'orig'
orig_results$activity <- Label
# ultimately you want to get a mean AUC of as close to 1 as possible. 0.5 is garbage.
# The evaluation of AUC gave us mean AUC of 0.94, then PCA should
# give us similar results.

# Evaluate all PC combinations from 1 PC to 20 PCs
my_vec <- 1:20; names(my_vec) <- paste0('pc',my_vec)
pc_results <- lapply(
  my_vec,
  function(x) {
    dfComponents <- predict(my_pca, newdata = df_nzv)[,1:x]
    dfEvaluate <- cbind(as.data.frame(dfComponents), cluster = myLabels)
    EvaluateAUC(dfEvaluate)
  }
) %>% do.call('rbind',.)
pc_results$dataset <- names(my_vec) %>% factor(., levels = names(my_vec))
pc_results$activity <- Label
results <- rbind(pc_results, orig_results)
results_l <- melt(results, id = c('dataset','activity'))
ggplot(results_l, aes(x = dataset, y = value, col = variable)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste("Mean AUC and Mean Error by Dataset:", Label))
pc_results_walking <- results_l
###########################################################
# Evaluate the effectiveness of principal components - Label = 'STANDING'
###########################################################
Label = 'STANDING'
# Create a separate vector of labels associated with observations
myLabels <- training_data$activity
myLabels <- ifelse(myLabels == Label, 1, 0)
# bind newly cleaned dataset with labels
dfEvaluate <- cbind(as.data.frame(sapply(df_nzv, as.numeric)),
                    cluster = myLabels)
orig_results <- EvaluateAUC(dfEvaluate)
orig_results$dataset <- 'orig'
orig_results$activity <- Label
my_vec <- 1:20; names(my_vec) <- paste0('pc',my_vec)
pc_results <- lapply(
  my_vec,
  function(x) {
    dfComponents <- predict(my_pca, newdata = df_nzv)[,1:x]
    dfEvaluate <- cbind(as.data.frame(dfComponents), cluster = myLabels)
    EvaluateAUC(dfEvaluate)
  }
) %>% do.call('rbind',.)
pc_results$dataset <- names(my_vec) %>% factor(., levels = names(my_vec))
pc_results$activity <- Label
results <- rbind(pc_results, orig_results)
results_l <- melt(results, id = c('dataset','activity'))
ggplot(results_l, aes(x = dataset, y = value, col = variable)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste("Mean AUC and Mean Error by Dataset:", Label))
pc_results_standing <- results_l
###########################################################
# Evaluate the effectiveness of principal components - Label = 'LAYING'
###########################################################
Label = 'LAYING'
# Create a separate vector of labels associated with observations
myLabels <- training_data$activity
myLabels <- ifelse(myLabels == Label, 1, 0)
# bind newly cleaned dataset with labels
dfEvaluate <- cbind(as.data.frame(sapply(df_nzv, as.numeric)),
                    cluster = myLabels)
orig_results <- EvaluateAUC(dfEvaluate)
orig_results$dataset <- 'orig'
orig_results$activity <- Label
my_vec <- 1:20; names(my_vec) <- paste0('pc',my_vec)
pc_results <- lapply(
  my_vec,
  function(x) {
    dfComponents <- predict(my_pca, newdata = df_nzv)[,1:x]
    dfEvaluate <- cbind(as.data.frame(dfComponents), cluster = myLabels)
    EvaluateAUC(dfEvaluate)
  }
) %>% do.call('rbind',.)
pc_results$dataset <- names(my_vec) %>% factor(., levels = names(my_vec))
pc_results$activity <- Label
results <- rbind(pc_results, orig_results)
results_l <- melt(results, id = c('dataset','activity'))
ggplot(results_l, aes(x = dataset, y = value, col = variable)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste("Mean AUC and Mean Error by Dataset:", Label))
pc_results_laying <- results_l
###########################################################
# Evaluate the effectiveness of principal components - Label = 'SITTING'
###########################################################
Label = 'SITTING'
# Create a separate vector of labels associated with observations
myLabels <- training_data$activity
myLabels <- ifelse(myLabels == Label, 1, 0)
# bind newly cleaned dataset with labels
dfEvaluate <- cbind(as.data.frame(sapply(df_nzv, as.numeric)),
                    cluster = myLabels)
orig_results <- EvaluateAUC(dfEvaluate)
orig_results$dataset <- 'orig'
orig_results$activity <- Label
my_vec <- 1:20; names(my_vec) <- paste0('pc',my_vec)
pc_results <- lapply(
  my_vec,
  function(x) {
    dfComponents <- predict(my_pca, newdata = df_nzv)[,1:x]
    dfEvaluate <- cbind(as.data.frame(dfComponents), cluster = myLabels)
    EvaluateAUC(dfEvaluate)
  }
) %>% do.call('rbind',.)
pc_results$dataset <- names(my_vec) %>% factor(., levels = names(my_vec))
pc_results$activity <- Label
results <- rbind(pc_results, orig_results)
results_l <- melt(results, id = c('dataset','activity'))
ggplot(results_l, aes(x = dataset, y = value, col = variable)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste("Mean AUC and Mean Error by Dataset:", Label))
pc_results_sitting <- results_l
###########################################################
# Evaluate the effectiveness of principal components - Label = 'WALKING_DOWNSTAIRS'
###########################################################
Label = 'WALKING_DOWNSTAIRS'
# Create a separate vector of labels associated with observations
myLabels <- training_data$activity
myLabels <- ifelse(myLabels == Label, 1, 0)
# bind newly cleaned dataset with labels
dfEvaluate <- cbind(as.data.frame(sapply(df_nzv, as.numeric)),
                    cluster = myLabels)
orig_results <- EvaluateAUC(dfEvaluate)
orig_results$dataset <- 'orig'
orig_results$activity <- Label
my_vec <- 1:20; names(my_vec) <- paste0('pc',my_vec)
pc_results <- lapply(
  my_vec,
  function(x) {
    dfComponents <- predict(my_pca, newdata = df_nzv)[,1:x]
    dfEvaluate <- cbind(as.data.frame(dfComponents), cluster = myLabels)
    EvaluateAUC(dfEvaluate)
  }
) %>% do.call('rbind',.)
pc_results$dataset <- names(my_vec) %>% factor(., levels = names(my_vec))
pc_results$activity <- Label
results <- rbind(pc_results, orig_results)
results_l <- melt(results, id = c('dataset','activity'))
ggplot(results_l, aes(x = dataset, y = value, col = variable)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste("Mean AUC and Mean Error by Dataset:", Label))
pc_results_walking_down <- results_l
###########################################################
# Evaluate the effectiveness of principal components - Label = 'WALKING_UPSTAIRS'
###########################################################
Label = 'WALKING_UPSTAIRS'
# Create a separate vector of labels associated with observations
myLabels <- training_data$activity
myLabels <- ifelse(myLabels == Label, 1, 0)
# bind newly cleaned dataset with labels
dfEvaluate <- cbind(as.data.frame(sapply(df_nzv, as.numeric)),
                    cluster = myLabels)
orig_results <- EvaluateAUC(dfEvaluate)
orig_results$dataset <- 'orig'
orig_results$activity <- Label
my_vec <- 1:20; names(my_vec) <- paste0('pc',my_vec)
pc_results <- lapply(
  my_vec,
  function(x) {
    dfComponents <- predict(my_pca, newdata = df_nzv)[,1:x]
    dfEvaluate <- cbind(as.data.frame(dfComponents), cluster = myLabels)
    EvaluateAUC(dfEvaluate)
  }
) %>% do.call('rbind',.)
pc_results$dataset <- names(my_vec) %>% factor(., levels = names(my_vec))
pc_results$activity <- Label
results <- rbind(pc_results, orig_results)
results_l <- melt(results, id = c('dataset','activity'))
pc_results_walking_up <- results_l

# Results for PCA Evaluation
pc_eval_df <- list(
  pc_results_laying,
  pc_results_sitting,
  pc_results_standing,
  pc_results_walking,
  pc_results_walking_down,
  pc_results_walking_up
) %>% do.call('rbind', .)
# Plot results for PCA Evaluation
pc_eval_plot <- ggplot(pc_eval_df, aes(x = dataset, y = value, col = variable)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste("Mean AUC and Mean Error by Dataset and Activities")) +
  facet_wrap(~activity)
pc_eval_plot

save(
  my_pca,
  pc_eval_df,
  #pc_eval_plot,
  training_data,
  test_data,
  imp_features_pc1,
  imp_features_pc2,
  #pc_biplot,
  #pc_screeplot,
  file = 'data/4Model_pca.rda'
)