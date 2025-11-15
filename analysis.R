# look at stores we have to predict for and see what stores we have data for
# take dataset and mutate
# combine tables
# lag sales
# find which departments are missing, figure out how to predict something you dont have data for
# knn
# what do we do with missing data?
# joining issue
# there are places we need predcictions for that we have no dtaa on
# some stores and departments we only have 5 datapoints, what do we do?

# setwd("C:\\Users\\madel\\OneDrive\\Documents\\Stat 348\\Walmart")

library(tidymodels)

train_wal <- read_csv("train.csv")
test_wal <- read_csv("test.csv")
features_wal <- read_csv("features.csv")


# --- Join all 2021 files ---
full_wal <- train_wal %>%
  full_join(features_wal, by = "Store")


# --- Write to CSV ---
write_csv(combined, "walmart.csv")
