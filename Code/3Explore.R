# This file explores the data through descriptive and inferential statistics
source('config.r')
# load extracted data
load('data/2Transform.rda')

# wordcloud of feature names
features <- readLines('data/original_data/uci har dataset/features.txt') %>%
  # split the string by hyphen
  stringr::str_split(., pattern = '-') %>% unlist
# create an object inheriting from VectorSource; required for Corpus
words.vec <- VectorSource(features)
# define a corpora; This will be used as we go further along text mining
words.corpus <- Corpus(words.vec)
############################################
# transform the corpus
############################################
# convert text to lower case
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
# remove punctuation
words.corpus <- tm_map(words.corpus, removePunctuation)
# remove numbers
words.corpus <- tm_map(words.corpus, removeNumbers)
# remove stop words
words.corpus <- tm_map(words.corpus, removeWords, stopwords('english'))
# create term document matrix
tdm <- TermDocumentMatrix(words.corpus)
m <- as.matrix(tdm)
# get term frequencies
wordCounts <- rowSums(m)
# sort term frequencies descending
wordCounts <- sort(wordCounts, decreasing = TRUE)
# word cloud
wordcloud(
  names(wordCounts), wordCounts,
  min.freq = 2, # only show words if it exists at least 2 times
  max.words = 50, # only show at most 50 words in wordcloud
  rot.per = 0.35, 
  colors = brewer.pal(8, "Dark2") # number of different colors in palette; palette name
)

# validate training and test data have been normalized
fv_test <- test_data[, 3:563] %>% as.matrix
fv_train <- training_data[, 3:563] %>% as.matrix
min(fv_test)
max(fv_test)
min(fv_train)
max(fv_train)
# look at the quantiles and distribution of the feature vector
quantile(fv_test, probs = c(c(0, .25, .5), seq(.75, 1, 0.05)))
hist(fv_test)
quantile(fv_train, probs = c(c(0, .25, .5), seq(.75, 1, 0.05)))
hist(fv_train)

# Are the features normalized between -1 and 1?
sapply(training_data[,3:dim(training_data)[2]],
       function(x) all(x <= 1 & x >= -1)) %>% table
sapply(test_data[,3:dim(test_data)[2]],
       function(x) all(x <= 1 & x >= -1)) %>% table
# Variables in training data
feature_split_list <- strsplit(names(training_data), split = '-')
names(feature_split_list) <- names(training_data)
# dimensions of training data
dim(training_data)
# mode of training data
sapply(training_data, mode) %>% table
# class of training data
sapply(training_data, class) %>% table
# check for missing values
sapply(training_data, function(x) any(is.na(x))) %>% table
# feature sampling
# matrix of columns to randomly sample from the feature vector
set.seed(3)
m_col_sample <- replicate(10, sample(3:length(names(training_data)), size = 100, replace = TRUE), simplify = TRUE)
# explore the means: for each column sample vector of 100 features take the mean of each feature
# for each vector of 100 column samples, get the mean of each column to create a distribution of means
# distribution of feature means
col_means_list <- lapply(
  as.data.frame(m_col_sample),
  get_stat, fun = 'mean'
)
sapply(col_means_list, summary)
# explore the standard deviations: for each column sample vector of 100 features take the std. dev. of each feature
# for each vector of 100 column samples, get the std. dev. of each column to create a distribution of st. dev.
# distribution of feature standard deviations
col_sd_list <- lapply(
  as.data.frame(m_col_sample),
  get_stat, fun = 'sd'
)
sapply(col_sd_list, summary)
# normality of a random sample of 4 predictor variables
par(mfrow = c(2,2))
set.seed(3)
sapply(sample(m_col_sample, 4), get_qq)
set.seed(4)
sapply(sample(m_col_sample, 4), get_qq)

# Histograms of a random sample of 4 predictor variables
par(mfrow = c(2,2))
set.seed(3)
sapply(sample(m_col_sample, 4), get_hist)
set.seed(4)
sapply(sample(m_col_sample, 4), get_hist)
par(mfrow = c(1,1))
# How does the feature vector compare by subject ID? (heatmap)
training.long <- training_data
colnames(training.long)[3:dim(training.long)[2]] <- 1:561
training.long <- melt(training.long, 
                      id = c('subject_id','activity'))
training.long$subject_id <- as.factor(training.long$subject_id)
training.long$variable <- as.numeric(training.long$variable)
# create heatmap of features by subject_id
heat_map_subject <- ggplot(training.long, aes(x = variable, y = subject_id)) +
  geom_tile(aes(fill = value)) +
  theme_bw() +
  ggtitle('Heatmap of Features by Subject ID')
# create heatmap of features by activity
heat_map_activity <- ggplot(training.long, aes(x = variable, y = activity)) +
  geom_tile(aes(fill = value)) +
  theme_bw() +
  ggtitle('Heatmap of Features by Activity')

# Create histogram of activity labels in training data
hist_activity_train <- ggplot(training_data, aes(x = activity)) +
  geom_histogram(color = 'white', fill = 'blue', stat = 'count') +
  ggtitle("Distribution of Activity Labels (Y) for Training Data") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create histogram of activity labels in test data
hist_activity_test <- ggplot(test_data, aes(x = activity)) +
  geom_histogram(color = 'white', fill = 'blue', stat = 'count') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Distribution of Activity Labels (Y) for Test Data")

# Create histogram of subject id in training data
hist_subject_train <- ggplot(training_data, aes(x = subject_id)) +
  geom_histogram(aes(fill = as.factor(subject_id)), color = 'white', stat = 'count') +
  ggtitle("Distribution of Subjects for Training Data")

# Create histogram of subject id in test data
hist_subject_test <- ggplot(test_data, aes(x = subject_id)) +
  geom_histogram(aes(fill = as.factor(subject_id)), color = 'white', stat = 'count') +
  ggtitle("Distribution of Subjects for Test Data")


# save
save(
  feature_split_list,
  m_col_sample,
  col_means_list,
  col_sd_list,
  heat_map_subject,
  heat_map_activity,
  wordCounts,
  file = 'data/3Explore.rda'
)