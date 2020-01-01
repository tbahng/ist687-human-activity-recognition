## ---- echo = FALSE, warning=FALSE, message=FALSE-------------------------
source('config.r')
load('data/2Transform.rda')
load('data/3Explore.rda')
load('data/4Model_pca.rda')

## ---- echo = FALSE-------------------------------------------------------
paste(
  readLines(paste0(file_path_data, '/activity_labels.txt')),
  collapse = ' | '
)

## ---- echo=FALSE, warning=FALSE------------------------------------------
ggplot(training_data, aes(x = subject_id)) +
  geom_histogram(aes(fill = as.factor(subject_id)), color = 'white', stat = 'count') +
  ggtitle("Distribution of Subjects for Training Data")

ggplot(test_data, aes(x = subject_id)) +
  geom_histogram(aes(fill = as.factor(subject_id)), color = 'white', stat = 'count') +
  ggtitle("Distribution of Subjects for Test Data")

## ---- echo=FALSE---------------------------------------------------------
paste(
  readLines(paste0(file_path_data, '/features_info.txt'))[13:29],
  collapse = ' | '
)

## ---- echo = FALSE-------------------------------------------------------
df.vars <- readLines(paste0(file_path_data, '/features_info.txt'))[33:49] %>%
  str_split(., pattern = ': ') %>% do.call('rbind',.) %>%
  data.frame()
colnames(df.vars) <- c('Variable','Description')
kable(df.vars)

## ---- echo = FALSE-------------------------------------------------------
df.tree <- rbind(dim(training_data), dim(test_data)) %>% 
  as.data.frame() %>%
  cbind(c('training','test'))
colnames(df.tree) <- c('rows','columns','name')
treemap::treemap(
  df.tree,
  index = "name",
  vSize = "rows",
  vColor = "name",
  type = "index",
  title = "Training / Test (70:30) Split"
)

## ---- echo = FALSE-------------------------------------------------------
m.fv <- rbind(
  test_data[, 3:563] %>% as.matrix,
  training_data[, 3:563] %>% as.matrix
)
hist(m.fv, main = "Histogram of Normalized Feature Values", xlab = '')

## ---- echo=FALSE---------------------------------------------------------
quantile(m.fv, probs = c(c(0, .25, .5), seq(.75, 1, 0.05)))

## ---- echo = FALSE, warning=FALSE----------------------------------------
ggplot(training_data, aes(x = activity)) +
  geom_histogram(color = 'white', fill = 'blue', stat = 'count') +
  ggtitle("Distribution of Activity Labels (Y) for Training Data") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ---- echo = FALSE-------------------------------------------------------
loadings.pc1 <- sort(abs(my_pca$rotation[,"PC1"]))
loadings.pc2 <- sort(abs(my_pca$rotation[,"PC2"]))
hist(loadings.pc1, col = 'blue',
     main = 'Histogram of Loadings for PC1')
hist(loadings.pc2, col = 'blue',
     main = 'Histogram of Loadings for PC2')

## ---- echo = FALSE-------------------------------------------------------
sort(tail(loadings.pc1, 10), decreasing = TRUE)

## ---- echo =FALSE--------------------------------------------------------
sort(tail(loadings.pc2, 10), decreasing = TRUE)

## ---- echo = FALSE-------------------------------------------------------
load('data/4Model_classification.rda')
autoplot(my_pca, data = training_data ,label = FALSE, 
         loadings.label = FALSE, colour = 'activity') +
  theme_bw() +
  ggtitle("Biplot of First Two Principal Components")

## ---- echo = FALSE-------------------------------------------------------
autoplot(my_pca, label = FALSE, loadings.label = TRUE) +
  theme_bw() +
  ggtitle("Biplot of First Two Principal Components",
          subtitle = "Actual feature names masked for ease of view")

## ---- echo=FALSE, fig.width=10-------------------------------------------
screeplot(my_pca, 
          col = 'blue', 
          npcs = 5, 
          main = 'Screeplot of First 5 Principal Components vs. Explained Variance',
          xlab = 'Principal Components')

## ---- echo=FALSE---------------------------------------------------------
pc_eval_df.orig <- pc_eval_df[pc_eval_df$dataset == 'orig',]
orig.mean_auc <- mean(pc_eval_df.orig$value[pc_eval_df.orig$variable == 'mean_auc'])
orig.mean_err <- mean(pc_eval_df.orig$value[pc_eval_df.orig$variable == 'mean_error'])

## ---- echo = FALSE-------------------------------------------------------
mod.o

## ---- echo = FALSE-------------------------------------------------------
kable(
  data.frame(
    class = names(classErrCV.o),
    error = paste0(as.character(round(classErrCV.o*100,2)),'%')
  )
)


## ---- echo = FALSE-------------------------------------------------------
kable(
  cmatrix.o %>% as.data.frame.matrix
)

## ---- echo = FALSE-------------------------------------------------------
mod.b

## ---- echo = FALSE-------------------------------------------------------
kable(
  data.frame(
    class = names(classErrCV.b),
    error = paste0(as.character(round(classErrCV.b*100,2)),'%')
  )
)

## ---- echo = FALSE-------------------------------------------------------
kable(
  cmatrix.b %>% as.data.frame.matrix
)

