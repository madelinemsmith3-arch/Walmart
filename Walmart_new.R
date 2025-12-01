## Libraries I need
library(tidyverse)
library(vroom)
library(tidymodels)
library(DataExplorer)

## Read in the Data
train <- vroom("./train.csv")
test <- vroom("./test.csv")
features <- vroom("./features.csv")

#########
## EDA ##
#########
plot_missing(features)
plot_missing(test)

### Impute Missing Markdowns
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log   = log1p(MarkDown_Total)
  ) %>%
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

## Impute Missing CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))
imputed_features <- juice(prep(feature_recipe))

########################
## Merge the Datasets ##
########################

fullTrain <- left_join(train, imputed_features, by=c("Store", "Date")) %>%
  select(-IsHoliday.y) %>%
  rename(IsHoliday=IsHoliday.x) %>%
  select(-MarkDown_Total)
fullTest <- left_join(test, imputed_features, by=c("Store", "Date")) %>%
  select(-IsHoliday.y) %>%
  rename(IsHoliday=IsHoliday.x) %>%
  select(-MarkDown_Total)
plot_missing(fullTrain)
plot_missing(fullTest)

### change IsHoliday to the actual holiday it refers to
holiday_lookup <- tibble(
  Date = as.Date(c(
    # Super Bowl
    "2010-02-12","2011-02-11","2012-02-10","2013-02-08",
    # Labor Day
    "2010-09-10","2011-09-09","2012-09-07","2013-09-06",
    # Thanksgiving
    "2010-11-26","2011-11-25","2012-11-23","2013-11-29",
    # Christmas
    "2010-12-31","2011-12-30","2012-12-28","2013-12-27"
  )),
  HolidayName = c(
    rep("SuperBowl",4),
    rep("LaborDay",4),
    rep("Thanksgiving",4),
    rep("Christmas",4)
  )
)

##################################
## Loop Through the Store-depts ## 
## and generate predictions.    ##
##################################
all_preds <- tibble(Id = character(), Weekly_Sales = numeric())
n_storeDepts <- fullTest %>% distinct(Store, Dept) %>% nrow()
cntr <- 0
for(store in unique(fullTest$Store)){
  
  store_train <- fullTrain %>%
    filter(Store==store)
  store_test <- fullTest %>%
    filter(Store==store)
  
  for(dept in unique(store_test$Dept)){
    
    ## Filter Test and Training Data
    dept_train <- store_train %>%
      filter(Dept==dept)
    dept_test <- store_test %>%
      filter(Dept==dept)
    
    dept_train <- dept_train %>%
      left_join(holiday_lookup, by="Date") %>%
      mutate(HolidayName = ifelse(IsHoliday, HolidayName, "None"))
    
    dept_test <- dept_test %>%
      left_join(holiday_lookup, by="Date") %>%
      mutate(HolidayName = ifelse(IsHoliday, HolidayName, "None"))
    
    ## If Statements for data scenarios
    if(nrow(dept_train)==0){
      
      ## Predict 0
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales=0) %>%
        distinct(Id, .keep_all = TRUE)
      
      
    } else if(nrow(dept_train) < 10 && nrow(dept_train) > 0){
      
      ## Predict the mean
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales=mean(dept_train$Weekly_Sales)) %>%
        distinct(Id, .keep_all = TRUE)
      
      
    } else {
      
      ## Fit a penalized regression model
      holiday_has_var <- dept_train %>% pull(HolidayName) %>% n_distinct() > 1
      
      if (holiday_has_var) {
        my_recipe <- recipe(Weekly_Sales ~ ., data = dept_train) %>%
          step_mutate(Holiday = HolidayName) %>%
          step_novel(Holiday) %>%
          step_dummy(Holiday) %>%     
          step_date(Date, features = c("month", "year")) %>%
          step_rm(Date, Store, Dept, IsHoliday, HolidayName)
      
        
      } else {
        # No dummy variables (all are "None")
        my_recipe <- recipe(Weekly_Sales ~ ., data = dept_train) %>%
          step_mutate(Holiday = HolidayName) %>%
          step_date(Date, features=c("month","year")) %>%
          step_rm(Date, Store, Dept, IsHoliday, HolidayName)
      }
      
      my_model <- rand_forest(mtry=3,
                              trees=100,
                              min_n=5) %>%
        set_engine("ranger") %>%
        set_mode("regression")
      
      my_wf <- workflow() %>%
        add_recipe(my_recipe) %>%
        add_model(my_model) %>%
        fit(dept_train)

      preds <- dept_test %>%
        transmute(Id = paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales = predict(my_wf, new_data = .) %>% pull(.pred)) %>%
        distinct(Id, .keep_all = TRUE)
      
    }
  
    ## Bind predictions together
    all_preds <- all_preds %>% distinct(Id, .keep_all = TRUE)
    
    
    ## Print out Progress
    cntr <- cntr+1
    cat("Store", store, "Department", dept, "Completed.",
        round(100 * cntr / n_storeDepts, 1), "% overall complete.\n")
    
  } ## End Dept Loop
  
} ## End Store Loop

## Write out after each store so I don't have to start over
vroom_write(x=all_preds, 
            file=paste0("./Predictions_change_holiday.csv"), delim=",")



