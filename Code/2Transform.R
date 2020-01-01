# This file transforms original data into R data suitable for further exploration and modeling
# Transformed training_data and test_data is saved to 2Transform.rda
source('config.r')
# time start
ptm <- proc.time()
# load extracted data
load('data/1Extract.rda')

# specify column names of training data and test data
dat_names <- c('activity', 'subject_id', as.character(x_names[,2]))
# transform training data
training_data <- cbind(train_y, train_id, train_x)
training_data[,1] <- sapply(training_data[,1], convert_y) %>% as.factor
colnames(training_data) <- dat_names

# transform test data
test_data <- cbind(test_y, test_id, test_x)
test_data[,1] <- sapply(test_data[,1], convert_y) %>% as.factor
colnames(test_data) <- dat_names

# save
save(
  training_data,
  test_data,
  file = 'data/2Transform.rda'
)

# time elapsed
time_elapsed <- proc.time() - ptm %>% data.matrix %>% t
write.csv(time_elapsed, 'data/log/log_2Transform.csv')