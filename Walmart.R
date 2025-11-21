library(dplyr)
library(tidyr)
library(ggplot2)
library(tidymodels)
library(vroom)
library(lubridate)

# setwd("C:\\Users\\madel\\OneDrive\\Documents\\Stat 348\\Walmart\\Walmart")
######################################################################
# read in data
train_wal <- vroom("./train.csv")
test_wal <- vroom("./test.csv")
features_wal <- vroom("./features.csv")

#####################################################################
# replace NAs in the markdown variables with a 0

features_wal$MarkDown1[is.na(features_wal$MarkDown1)] <- 0
features_wal$MarkDown2[is.na(features_wal$MarkDown2)] <- 0
features_wal$MarkDown3[is.na(features_wal$MarkDown3)] <- 0
features_wal$MarkDown4[is.na(features_wal$MarkDown4)] <- 0
features_wal$MarkDown5[is.na(features_wal$MarkDown5)] <- 0

train_wal$Weekly_Sales <- as.numeric(train_wal$Weekly_Sales)


# create TotalMarkdown variable column that is the sum of all markdowns
features_wal$TotalMarkdown <- rowSums(features_wal[, c("MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5")])
features_wal$MarkdownFlag <- ifelse(features_wal$TotalMarkdown > 0, 1, 0)
features_wal[, c("MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5")] <- NULL

train_wal <- train_wal %>%
  left_join(features_wal, by = c("Date", "Store"))
test_wal <- test_wal %>%
  left_join(features_wal, by = c("Date", "Store"))

## Impute Missing CPI and Unemployment
feature_recipe <- recipe(~., data = features_wal) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))

prep_rec <- prep(feature_recipe)
features_imputed <- bake(prep_rec, new_data = features_wal)

######################### FACEBOOK PROPHETS ##############################

##########################################
## Fit Prophet Model to see how it does ##
##########################################

## Choose Store and Dept
store <- 1
dept <- 1

## Filter and Rename to match prophet syntax
sd_train <- train_wal %>%
  filter(Store==store, Dept==dept) %>%
  rename(y=Weekly_Sales, ds=Date)

sd_test <- test_wal %>%
  filter(Store==store, Dept==dept) %>%
  rename(ds=Date)

library(prophet)

## Fit a prophet model
prophet_model <- prophet() %>%
  add_regressor("CPI") %>%
  add_regressor("Unemployment") %>%
  add_regressor("TotalMarkdown") %>%
  fit.prophet(df=sd_train)

## Predict Using Fitted prophet Model
fitted_vals <- predict(prophet_model, df=sd_train) #For Plotting Fitted Values
sd_test_clean <- na.omit(sd_test)
test_preds <- predict(prophet_model, sd_test_clean)

## Plot Fitted and Forecast on Same Plot
p1 <- ggplot() +
  geom_line(data = sd_train, mapping = aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
  labs(color="Store 1 Dept 1")

## Choose Store and Dept
store <- 1
dept <- 4

## Filter and Rename to match prophet syntax
sd_train <- train_wal %>%
  filter(Store==store, Dept==dept) %>%
  rename(y=Weekly_Sales, ds=Date)

sd_test <- test_wal %>%
  filter(Store==store, Dept==dept) %>%
  rename(ds=Date)

library(prophet)

## Fit a prophet model
prophet_model <- prophet() %>%
  add_regressor("CPI") %>%
  add_regressor("Unemployment") %>%
  add_regressor("TotalMarkdown") %>%
  fit.prophet(df=sd_train)

## Predict Using Fitted prophet Model
fitted_vals <- predict(prophet_model, df=sd_train) #For Plotting Fitted Values
sd_test_clean <- na.omit(sd_test)
test_preds <- predict(prophet_model, sd_test_clean)


p2 <- ggplot() +
  geom_line(data = sd_train, mapping = aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
  labs(color="Store 1 Dept 4")

library(patchwork)
p1 + p2


###################### PENALIZED REGRESSION DEPT 5##########################

train_wal2 <- train_wal %>%
  left_join(features_imputed, by = c("Date", "Store"))

test_wal2 <- test_wal %>%
  left_join(features_imputed, by = c("Date", "Store"))

train_dept5 <- train_wal2 %>% filter(Dept == 5)

my_recipe <- recipe(Weekly_Sales ~ ., data = train_dept5) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_rm(Date) %>%
  step_rm(contains("IsHoliday") ) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())


my_mod <- linear_reg(
  mixture = tune(),
  penalty = tune()
) %>% 
  set_engine("glmnet")

wal_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)

set.seed(123)
wal_folds <- vfold_cv(train_dept5, v = 5)

grid_vals <- grid_regular(
  penalty(range = c(-6, 1)), 
  mixture(),
  levels = 5
)


tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5) 

start_time <- Sys.time()

tune_res <- tune_grid(
  wal_workflow,
  resamples = wal_folds,
  grid = grid_vals,
  metrics = metric_set(rmse, rsq),
  control = control_grid(save_pred = TRUE, verbose = TRUE)
)

elapsed <- Sys.time() - start_time
elapsed



# get best tuned model

best_params <- select_best(tune_res, metric = "rmse")
best_params

# get cross-validated error for your report

cv_rmse <- show_best(tune_res, metric = "rmse", n = 1)
cv_rmse

# random forest, 1050

######################### EDA ########################################


missing_dat <- features_wal %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "missing") 

ggplot(missing_dat, aes(x = reorder(variable, -missing), y = missing)) +
  geom_bar(stat = "identity") +
  labs(title = "Missing Values per Variable in Features Table",
       x = "Variable",
       y = "Number of Missing Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# check for distribution of stores in each table

library(ggplot2)

ggplot() +
  geom_density(data = train_wal, aes(x = Store, color = "Train"), linewidth = 1) +
  geom_density(data = test_wal, aes(x = Store, color = "Test"), linewidth = 1) +
  scale_color_manual(values = c("Train" = "steelblue", 
                                "Test"  = "firebrick")) +
  labs(title = "Train vs Test Distribution",
       color = "Dataset") +
  theme_minimal()

# check the holidays

library(patchwork)

p1 <- ggplot(train_wal, aes(x = IsHoliday)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Training Data Count of Holiday vs Non-Holiday Days",
       x = "Is Holiday",
       y = "Count") +
  theme_minimal()

p2 <- ggplot(test_wal, aes(x = IsHoliday)) +
  geom_bar(fill = "firebrick") +
  labs(title = "Test Data Count of Holiday vs Non-Holiday Days",
       x = "Is Holiday",
       y = "Count") +
  theme_minimal()

p1 + p2



######################### TRY TO JOIN DATA #################
train_wal <- read_csv("train.csv")
test_wal <- read_csv("test.csv")
features_wal <- read_csv("features.csv")


# --- Join all 2021 files ---
full_wal <- train_wal %>%
  full_join(features_wal, by = "Store")


# --- Write to CSV ---
write_csv(full_wal, "walmart.csv")
