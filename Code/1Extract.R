# This file extracts original data and saves data to 1Extract.rda
source('config.r')
# time start
ptm <- proc.time()
# read original data
file_paths <- c(file_path_x_train, file_path_y_train, file_path_id_train, 
                file_path_x_test, file_path_y_test, file_path_id_test,
                file_path_features, file_path_labels)
read_list <- lapply(
  file_paths,
  function(x) read.table(x)
)
names(read_list) <- c('train_x', 'train_y', 'train_id', 'test_x', 'test_y', 'test_id', 'x_names', 'y_names')

train_x <- read_list[['train_x']]
train_y <- read_list[['train_y']]
test_x <- read_list[['test_x']]
test_y <- read_list[['test_y']]
x_names <- read_list[['x_names']]
y_names <- read_list[['y_names']]
train_id <- read_list[['train_id']]
test_id <- read_list[['test_id']]

# save
save(
  train_x,
  train_y,
  test_x,
  test_y,
  x_names,
  y_names,
  train_id,
  test_id,
  file = 'data/1Extract.rda'
)

# time elapsed
time_elapsed <- proc.time() - ptm %>% data.matrix %>% t
write.csv(time_elapsed, 'data/log/log_1Extract.csv')