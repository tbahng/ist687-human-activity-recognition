rm(list = ls())
library(dplyr)
library(tibble)
data(USArrests)
train <- USArrests %>% rownames_to_column(var = "rowname")
test <- USArrests %>% rownames_to_column(var = "rowname")

# Run Principal Components Analysis
pc <- prcomp(train %>% select(-rowname), scale = TRUE)
# Extract PCs  (e.g. 1st 3 PCs)
train <- as_tibble(pc$x) %>% select(PC1:PC3)
test <- as_tibble(
  predict(pc, newdata = test %>% select(-rowname))
) %>% select(PC1:PC3)