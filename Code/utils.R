# This files defines relevant functions for this project

# function to convert a response variable value from integer to y names
convert_y <- function(x) {
  switch(
    x, 
    'WALKING',
    'WALKING_UPSTAIRS',
    'WALKING_DOWNSTAIRS',
    'SITTING',
    'STANDING',
    'LAYING'
  )
}

# function to return a vector of means from a vector of features
get_stat <- function(vec, dat = training_data, fun = 'mean') {
  if (fun == 'sd') {
    sapply(
      vec, 
      function(x) sd(dat[,x])
    )
  } else {
    sapply(
      vec, 
      function(x) mean(dat[,x])
    )
  }
}

# function to generate a normal qqplot given a column index of a data.frame
get_qq <- function(x, dat = training_data) {
  qqnorm(training_data[,x], 
         main = paste('Normal qqplot of', names(training_data)[x],paste0('[',x,']')))
  qqline(training_data[,x])
}

# function to generate a histogram given a column index of a data.frame
get_hist <- function(x, dat = training_data) {
  hist(training_data[,x], 
         main = paste('Histogram of', names(training_data)[x],paste0('[',x,']')))
}

# function to do cross-validation of data
EvaluateAUC <- function(dfEvaluate) {
  require(xgboost)
  require(Metrics)
  #require(ModelMetrics)
  CVs <- 5
  cvDivider <- floor(nrow(dfEvaluate) / (CVs+1))
  indexCount <- 1
  outcomeName <- c('cluster')
  predictors <- names(dfEvaluate)[!names(dfEvaluate) %in% outcomeName]
  lsErr <- c()
  lsAUC <- c()
  for (cv in seq(1:CVs)) {
    print(paste('cv', cv))
    dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
    dataTest <- dfEvaluate[dataTestIndex,]
    dataTrain <- dfEvaluate[-dataTestIndex,]
    
    bst <- xgboost::xgboost(data = as.matrix(dataTrain[, predictors]),
                            label = dataTrain[,outcomeName],
                            max.depth = 6, eta = 1, verbose = 0,
                            nround = 5, nthread = 4,
                            objective = 'reg:linear')
    predictions <- predict(bst, as.matrix(dataTest[, predictors]),
                           outputmargin = TRUE)
    err <- rmse(dataTest[, outcomeName], predictions)
    auc <- auc(dataTest[,outcomeName], predictions)
    
    lsErr <- c(lsErr, err)
    lsAUC <- c(lsAUC, auc)
    gc()
  }
  # print(paste('Mean Error:', mean(lsErr)))
  # print(paste('Mean AUC:', mean(lsAUC)))
  data.frame(mean_error = mean(lsErr),
             mean_auc = mean(lsAUC))
}

# Function to print PCA summary robust for addressing messiness of large feature vectors
pcaPrint <- function(x, n = 5) {
  cat("Importance of components:\n")
  print(x$importance[, 1:n])
  invisible(x)
}
#example
# pcaPrint(summary(my_pca))

# function to compute "flat" 0-1 error rate
flatErr<-function(x, ref) mean(x != ref)
# function to compute class-specific "flat" prediction errors
classErrs<-function(x, ref) tapply(x != ref, ref, mean)