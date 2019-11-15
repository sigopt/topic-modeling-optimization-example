################################################################################
# This script loads packages and declares functions to be used in the analyses
################################################################################

### load packages ----
library(textmineR)
library(randomForest)
library(magrittr)
library(stringr)
library(SigOptR)

### declare functions ----

# Function to train random forest model
train_classifier <- function(y, x){
  if (class(x) == "matrix")
    x <- as.data.frame(x)
  
  randomForest(x = x, y = y)
}

predict_classifier <- function(object, new_data){
  if (class(new_data) == "matrix")
    new_data <- as.data.frame(new_data)
  
  predict(object = object, newdata = new_data, type = "prob")
}


