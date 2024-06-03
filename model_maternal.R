## Data Extraction 

# Define the URL of the ZIP file
url <- "https://archive.ics.uci.edu/static/public/863/maternal+health+risk.zip"

# Define the destination file path
zip_file <- "data.zip"

# Download the ZIP file
download.file(url, zip_file)

# Unzip the file to a temporary directory
unzip_dir <- tempdir()
unzip(zip_file, exdir = unzip_dir)

# List files in the unzipped directory to find the CSV file
unzipped_files <- list.files(unzip_dir, full.names = TRUE)
csv_file <- unzipped_files[grepl("\\.csv$", unzipped_files)]

# Read the CSV file
maternal_data <- read.csv(csv_file)

## MODEL BUILDING

# Install required packages

#install.packages("lattice") # remove comment to install
#install.packages("randomForest") # remove comment to install
#install.packages("caret") #remove comment to install

# Importing libraries
library(lattice)
library(ggplot2)
library(randomForest)
library(caret)

TrainingIndex <- createDataPartition(maternal_data$RiskLevel, p = 0.8, list = FALSE)
TrainingSet <- maternal_data[TrainingIndex, ] # Training Set
TestingSet <- maternal_data[-TrainingIndex, ] # Test Set

# Save the datasets to CSV files
write.csv(TrainingSet, "training.csv", row.names = FALSE)
write.csv(TestingSet, "testing.csv", row.names = FALSE)

# Read the training set back from the CSV file
TrainSet <- read.csv("training.csv", header = TRUE)

# Remove the first column if it contains row numbers
TrainSet <- TrainSet[, -1]

# Ensure that RiskLevel is a factor
TrainSet$RiskLevel <- as.factor(TrainSet$RiskLevel)

# Build the Random Forest model for classification
model <- randomForest(RiskLevel ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)

# Save the model to an RDS file
saveRDS(model, "model.rds")

# Read the testing set back from the CSV file
TestSet <- read.csv("testing.csv", header = TRUE)

# Remove the first column if it contains row numbers
TestSet <- TestSet[, -1]

# Ensure that RiskLevel is a factor
TestSet$RiskLevel <- as.factor(TestSet$RiskLevel)

# Load the model from the RDS file
loaded_model <- readRDS("model.rds")

# Make predictions on the testing set
predictions <- predict(loaded_model, TestSet)

# Evaluate the model performance
confusion_matrix <- confusionMatrix(predictions, TestSet$RiskLevel)
print(confusion_matrix)

colnames(maternal_data)
