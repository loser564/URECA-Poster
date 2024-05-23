library(caret)
library(Boruta)
library(readxl)
library(tidyverse)
library(randomForest) # or library(ranger)
# Read Dataset
gse7390 <- read_excel("GSE7390_Clean.xlsx")
new_genes <- read.csv("p(nooverlap).csv", sep=";", header=TRUE)

# Preproces Data
new_genes_list <- na.omit(c(t(new_genes)))
new_genes_clean <- unique(new_genes_list)
new_genes_clean <- c(new_genes_clean, 'time', 'phenotype')

genes_not_in_dataset <- setdiff(new_genes_clean, colnames(gse7390))
new_genes_clean <- setdiff(new_genes_clean, genes_not_in_dataset)

# ################# ON 582 GENE #################
# gse7390_new <- gse7390[, new_genes_clean]
# # Prepare data for Boruta and subsequent training
# X <- gse7390_new[, !(names(gse7390_new) %in% c('phenotype', 'time'))]
# y <- gse7390_new$phenotype
# y <- as.factor(y) # Ensure the target variable is a factor

# # Placeholder for model accuracy
# accuracy_results <- numeric(1000)

# # Placeholder for storing confirmed and tentative feature information for each iteration
# features_info <- list()

# set.seed(500) # For reproducibility

# for(i in 1:1000) {
#   # Create 80-20 train-test split indices
#   trainIndex <- createDataPartition(y, p = .8, list = FALSE, times = 1)
#   X_train <- X[trainIndex, ]
#   y_train <- y[trainIndex]
#   X_test <- X[-trainIndex, ]
#   y_test <- y[-trainIndex]
  
#   # Run Boruta on the training set
#   boruta_output <- Boruta(X_train, y_train, doTrace = 0, maxRuns = 100)
  
#   # Get confirmed and tentative attributes
#   confirmed <- getSelectedAttributes(boruta_output, withTentative = FALSE)
#   tentative <- getSelectedAttributes(boruta_output, withTentative = TRUE)
  
#   # Store feature information
#   features_info[[i]] <- list(confirmed = confirmed, tentative = tentative)
  
#   # Train Random Forest model on selected features
#   # Note: Adjust the method and parameters as needed
#   model_rf <- randomForest(X_train[, confirmed], y_train)
  
#   # Predict on the test set
#   predictions <- predict(model_rf, X_test[, confirmed])
  
#   # Calculate and store accuracy
#   accuracy_results[i] <- sum(predictions == y_test) / length(y_test)
# }

# # Export or analyze the results
# write.csv(accuracy_results, "RF_accuracy_results.csv", row.names = FALSE)

################### ON ALL GENES ###################
gse7390_all <- gse7390

# Prepare data for Boruta and subsequent training
X <- gse7390_all[, !(names(gse7390_all) %in% c('phenotype', 'time'))]
y <- gse7390_all$phenotype

# Placeholder for model accuracy
accuracy_results <- numeric(1000)

# Placeholder for storing confirmed and tentative feature information for each iteration
features_info <- list()

set.seed(500) # For reproducibility

for(i in 1:1000) {
  # Create 80-20 train-test split indices
  trainIndex <- createDataPartition(y, p = .8, list = FALSE, times = 1)
  X_train <- X[trainIndex, ]
  y_train <- y[trainIndex]
  X_test <- X[-trainIndex, ]
  y_test <- y[-trainIndex]
  
  # Run Boruta on the training set
  boruta_output <- Boruta(X_train, y_train, doTrace = 2, maxRuns = 100)
  
  # Get confirmed and tentative attributes
  confirmed <- getSelectedAttributes(boruta_output, withTentative = FALSE)
  tentative <- getSelectedAttributes(boruta_output, withTentative = TRUE)
  print("Confirmed features: ", confirmed)
    print("Tentative features: ", tentative)
  # Store feature information
  features_info[[i]] <- list(confirmed = confirmed, tentative = tentative)
  
  # Train Random Forest model on selected features
  # Note: Adjust the method and parameters as needed
  model_rf <- randomForest(X_train[, confirmed], y_train)
  
  # Predict on the test set
  predictions <- predict(model_rf, X_test[, confirmed])
  
  # Calculate and store accuracy
  accuracy_results[i] <- sum(predictions == y_test) / length(y_test)
}

# write.csv(accuracy_results, "RF_accuracy_results_all.csv", row.names = FALSE)