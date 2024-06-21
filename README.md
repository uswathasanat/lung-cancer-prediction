# lung-cancer-prediction

### Loading the Data and Initial Exploration

```r
cancer <- read.csv("C:/Users/saila/Downloads/cancer patient data sets.csv")
View(cancer)
```
- The `read.csv()` function loads the CSV file containing cancer patient data into a data frame named `cancer`.
- `View(cancer)` opens a spreadsheet-like viewer of the data frame in RStudio, allowing you to inspect the data.

### Data Cleaning and Preprocessing

```r
cancer <- cancer[, -which(names(cancer) == "Patient.Id")]
cancer <- cancer[, -which(names(cancer) == "Level")]
```
- The code removes the `Patient.Id` and `Level` columns from the `cancer` data frame using `which()` to identify their positions by name.

### Exploring Unique Values in Each Column

```r
for (col in names(cancer)) {
  cat(col, "\n")
  unique_values <- unique(cancer[[col]])
  cat(unique_values, "\n")
  cat("******\n")
}
```
- This loop iterates over each column in the `cancer` data frame, printing the column name and its unique values, which helps in understanding the distribution and variety of data within each column.

### Calculating Exposure Risk

```r
cancer <- cancer %>%
  mutate(Exposure_Risk = (Air.Pollution) +
           (Occupational.Hazards) +
           (Passive.Smoker) +
           (Alcohol.use) +
           (Obesity) +
           (Dust.Allergy) +
           (chronic.Lung.Disease) +
           (Smoking) +
           (Genetic.Risk) +
           (Balanced.Diet) +
           (Chest.Pain) +
           (Coughing.of.Blood) +
           (Fatigue) +
           (Weight.Loss) +
           (Shortness.of.Breath ) +
           (Wheezing) +
           (Swallowing.Difficulty) +
           (Clubbing.of.Finger.Nails ) +
           (Frequent.Cold) +
           (Snoring) +
           (Dry.Cough))
```
- This code uses the `mutate()` function from the `dplyr` package to create a new column `Exposure_Risk` that is the sum of various risk factor columns. Ensure that `dplyr` is loaded with `library(dplyr)`.

### Calculating and Printing the Exposure Risk Score

```r
cat("Exposure Risk Score:", cancer$Exposure_Risk, "\n")
```
- This prints out the `Exposure_Risk` scores for all patients.

### Calculating Standard Deviation and Mean

```r
standard <- sd(cancer$Exposure_Risk, na.rm = TRUE)
standard
average <- mean(cancer$Exposure_Risk)
average
```
- `sd()` calculates the standard deviation of the `Exposure_Risk` column, ignoring any `NA` values.
- `mean()` calculates the mean of the `Exposure_Risk` column.

### Defining Risk Levels

```r
low_threshold <- average - standard
high_threshold <- average + standard

cancer$Exposure_Risk <- ifelse(
  cancer$Exposure_Risk < low_threshold, "Low",
  ifelse(
    cancer$Exposure_Risk >= low_threshold & cancer$Exposure_Risk <= high_threshold, "Medium",
    ifelse(
      cancer$Exposure_Risk > high_threshold, "High",
      NA
    )
  )
)
```
- This code classifies the `Exposure_Risk` into "Low", "Medium", and "High" categories based on the mean and standard deviation.

### Splitting Data into Training and Testing Sets

```r
set.seed(234)
inTrain <- createDataPartition(y = cancer$Exposure_Risk, p = 0.75, list = FALSE)
Training <- cancer[inTrain, ]
Testing <- cancer[-inTrain, ]

Training$Exposure_Risk <- factor(Training$Exposure_Risk)
Testing$Exposure_Risk <- factor(Testing$Exposure_Risk)
```
- `set.seed(234)` ensures reproducibility of the random partition.
- `createDataPartition()` splits the data into training (75%) and testing (25%) sets.
- Converts `Exposure_Risk` to a factor in both sets for classification purposes.

### Training and Evaluating Models

```r
library(caret)

outcome_var <- "Exposure_Risk"
formula <- as.formula(paste(outcome_var, "~ ."))
data_train <- Training
data_test <- Testing
ctrl <- trainControl(method = "cv", number = 10)
algorithms <- c("rf", "svmRadial", "knn", "nb", "rpart")
results <- data.frame(Model = character(0), Accuracy = numeric(0))

for (algo in algorithms) {
  model <- train(
    formula,
    data = data_train,
    method = algo,
    trControl = ctrl
  )
  
  predictions <- predict(model, newdata = data_test)
  accuracy <- confusionMatrix(predictions, data_test[[outcome_var]])$overall["Accuracy"]
  results <- rbind(results, data.frame(Model = algo, Accuracy = accuracy))
}

best_model <- results[which.max(results$Accuracy), ]
print(results)
print(paste("Best Model:", best_model$Model))
print(paste("Best Accuracy:", best_model$Accuracy))
```
- Loads the `caret` package.
- Defines the outcome variable and formula for model training.
- Uses `trainControl()` to set up 10-fold cross-validation.
- Iterates through a list of algorithms (`rf`, `svmRadial`, `knn`, `nb`, `rpart`), trains models, predicts on the test set, and calculates accuracy.
- Stores the results and identifies the best-performing model.

