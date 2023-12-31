library(dplyr)
library(readr)
library(missForest)
library(mice)
library(e1071)
library(DescTools)
library(ggplot2)
library(gridExtra)


myData <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
#Data Preparation
#Data omission
myData <- na.omit(myData)
#View(myData)
sum(is.na(myData))
#View(myData)
#Creating dummy variables
myData <- myData %>%
  mutate(
    #Generalising
    OnlineSecurity = ifelse(myData$OnlineSecurity == "No internet service", "No", myData$OnlineSecurity),
    OnlineBackup = ifelse(myData$OnlineBackup == "No internet service", "No", myData$OnlineBackup),
    DeviceProtection = ifelse(myData$DeviceProtection == "No internet service", "No", myData$DeviceProtection),
    TechSupport = ifelse(myData$TechSupport == "No internet service", "No", myData$TechSupport),
    StreamingTV = ifelse(myData$StreamingTV == "No internet service", "No", myData$StreamingTV),
    StreamingMovies = ifelse(myData$StreamingMovies == "No internet service", "No", myData$StreamingMovies),
    MultipleLines = ifelse(myData$MultipleLines == "No phone service", "No", myData$MultipleLines),
    #Creating Dummy variables
    genderNumerical = ifelse(gender == "Male", 1, 0),
    partnerNumerical = ifelse(Partner == "Yes", 1, 0),
    dependentsNumerical = ifelse(Dependents == "Yes", 1, 0),
    phoneServiceNumerical = ifelse(PhoneService == "Yes", 1, 0),
    multipleLinesNumerical = ifelse(MultipleLines == "Yes", 1, 0),
    onlineSecurityNumerical = ifelse(OnlineSecurity == "Yes", 1, 0),
    onlineBackupNumerical = ifelse(OnlineBackup == "Yes", 1, 0),
    deviceProtectionNumerical = ifelse(DeviceProtection == "Yes", 1, 0),
    techSupportNumerical = ifelse(TechSupport == "Yes", 1, 0),
    streamingTvNumerical = ifelse(StreamingTV == "Yes", 1, 0),
    streamingMoviesNumerical = ifelse(StreamingMovies == "Yes", 1, 0),
    paperlessBillingNumerical = ifelse(PaperlessBilling == "Yes", 1, 0),
    #if both 0 then it is No service
    dsl = ifelse(InternetService == "DSL", 1,0),
    fiberOptic = ifelse(InternetService == "Fiber optic", 1,0),
    #if both 0 then month to month
    oneYear = ifelse(Contract == "One year",1,0),
    twoYear = ifelse(Contract == "Two year",1,0),
    #if both zero then mailed check
    bankTransfer = ifelse(PaymentMethod == "Bank transfer (automatic)",1,0),
    electronicCheck = ifelse(PaymentMethod == "Electronic check",1,0),
    credicCardCheck = ifelse(PaymentMethod == "Credit card (automatic)",1,0),
    churnNumerical = ifelse(Churn == "Yes",1,0)
  )
#View(myData)
#imputed_data <- mice(myData, m=5, maxit=295806, method="rf")

#Measures of location
mean_value <- mean(myData$TotalCharges, na.rm = TRUE)
median_value <- median(myData$TotalCharges, na.rm = TRUE )
mode_value <- Mode(myData$TotalCharges, na.rm = TRUE)
summary_stats <- summary(myData, na.rm = TRUE)

print(paste("Mean: ", mean_value))
print(paste("Median: ", median_value))
print(paste("Mode: ", mode_value))
print(paste(summary_stats))

# [1] "Mean:  2283.30044084187"
# > print(paste("Median: ", median_value))
# [1] "Median:  1397.475"
# > print(paste("Mode: ", mode_value))
# [1] "Mode:  20.2"

#Measures of Dispersion
range_values <- range(myData$TotalCharges, na.rm = TRUE)
variance_value <- var(myData$TotalCharges, na.rm = TRUE)
sd_value <- sd(myData$TotalCharges, na.rm = TRUE)
iqr_value <- IQR(myData$TotalCharges, na.rm = TRUE)

print(paste("Range: ", range_values))
print(paste("Variance: ", variance_value))
print(paste("Standard Deviation: ", sd_value))
print(paste("IQR: ", iqr_value))

# > print(paste("Range: ", range_values))
# [1] "Range:  18.8"   "Range:  8684.8"
# > print(paste("Variance: ", variance_value))
# [1] "Variance:  5138252.40705357"
# > print(paste("Standard Deviation: ", sd_value))
# [1] "Standard Deviation:  2266.77136188314"
# > print(paste("IQR: ", iqr_value))
# [1] "IQR:  3393.2875"

#Measures of Shape
skewness_value <- skewness(myData$TotalCharges, na.rm = TRUE)
kurtosis_value <- kurtosis(myData$TotalCharges, na.rm = TRUE)

print(paste("Skewness: ", skewness_value))
print(paste("Kurtosis: ", mean_value))

# > print(paste("Skewness: ", skewness_value))
# [1] "Skewness:  0.961232281579416"
# > print(paste("Kurtosis: ", mean_value))
# [1] "Kurtosis:  2283.30044084187"

#Measures of Association
correlation_value <- cor(myData$MonthlyCharges, myData$TotalCharges, use = "complete.obs")
covariance_value <- cov(myData$MonthlyCharges, myData$TotalCharges, use = "complete.obs")

print(paste("Correlation Coeffieient: ", correlation_value))
print(paste("Covariance: ", covariance_value))

# > print(paste("Correlation Coeffieient: ", correlation_value))
# [1] "Correlation Coeffieient:  0.651064803226203"
# > print(paste("Covariance: ", covariance_value))
# [1] "Covariance:  44401.3330725437"

#Data visualization 
#Histogram
histogram <- ggplot(myData, aes(x = tenure, y = TotalCharges)) +
  geom_bar(stat = "identity", position = "stack", fill = "skyblue") +
  labs(title = "Bar Chart of Tenure vs. Total Charges",
       x = "Tenure",
       y = "Total Charges")
#Box plot
boxPlot <- ggplot(myData, aes(x = tenure, y = TotalCharges)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Tenure vs. Total Charges",
       x = "Tenure",
       y = "Total Charges")

#Scatter plot
scatterPlot <- ggplot(myData, aes(x = tenure, y = TotalCharges)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Tenure vs. Total Charges",
       x = "Tenure",
       y = "Total Charges")

grid.arrange(histogram, boxPlot, scatterPlot)

#Logistic regression
library(caTools) 
# Set a random seed for reproducibility
set.seed(123) 

#Training Data
train.rows <- sample(row.names(myData), dim(myData)[1]*0.8) 
train.data <- myData[train.rows,] 

dim(train.data)
#View(train.data)

#Validation Data
valid.rows <- setdiff(rownames(myData), train.rows)
valid.data <- myData[valid.rows,]

dim(valid.data)

train.data80.myData<- train.data
valid.data20.myData <- valid.data

logisticModel <- glm(myData$churnNumerical ~ myData$tenure + myData$MonthlyCharges + myData$TotalCharges
                    + myData$genderNumerical + myData$partnerNumerical + myData$dependentsNumerical 
                    + myData$phoneServiceNumerical + myData$multipleLinesNumerical + myData$onlineSecurityNumerical
                    + myData$onlineBackupNumerical + myData$deviceProtectionNumerical + myData$techSupportNumerical
                    + myData$streamingTvNumerical + myData$streamingMoviesNumerical + myData$paperlessBillingNumerical
                    + myData$dsl + myData$fiberOptic + myData$oneYear + myData$twoYear + myData$bankTransfer
                    + myData$electronicCheck + myData$credicCardCheck, family = binomial, data = myData, weights = ifelse(myData$churnNumerical == "1", 2,1))

summary(logisticModel)

library(forecast)
#accuracy(logisticModel$fitted.values, train.data$churnNumerical)
#pred <- predict(logisticModel, newdata = valid.data)
#accuracy(pred, valid.data$churnNumerical)


pHatLgm <- predict(logisticModel)
#pHatLgm

yHatLgm <- ifelse(pHatLgm >= 0.5, 1,0)
#yHatLgm

#confusion matrix to evaluate performance measures
conf_matrix_lgm <- table(Actual = myData$churnNumerical, Predicted = yHatLgm)

misclassification_rate_lgm <- (conf_matrix_lgm[2, 1] + conf_matrix_lgm[1, 2]) / sum(conf_matrix_lgm)
accuracy_lgm <- 1-misclassification_rate_lgm
specificity_lgm <- conf_matrix_lgm[1, 1] / (conf_matrix_lgm[1, 1] + conf_matrix_lgm[1, 2])
sensitivity_lgm <- conf_matrix_lgm[2, 2] / (conf_matrix_lgm[2, 1] + conf_matrix_lgm[2, 2])
precision_lgm <- conf_matrix_lgm[2, 2] / (conf_matrix_lgm[1, 2] + conf_matrix_lgm[2, 2])

# Print results
print(paste("Misclassification Rate:", round(misclassification_rate_lgm*100, 3), "%"))
print(paste("Accuracy:", round(accuracy_lgm*100,3), "%"))
print(paste("Specificity:", round(specificity_lgm*100,3), "%"))
print(paste("Sensitivity (Recall):", round(sensitivity_lgm*100,3), "%"))
print(paste("Precision:", round(precision_lgm*100,3), "%"))

# > print(paste("Misclassification Rate:", round(misclassification_rate_lgm*100, 3), "%"))
# [1] "Misclassification Rate: 20.193 %"
# > print(paste("Accuracy:", round(accuracy_lgm*100,3), "%"))
# [1] "Accuracy: 79.807 %"
# > print(paste("Specificity:", round(specificity_lgm*100,3), "%"))
# [1] "Specificity: 86.81 %"
# > print(paste("Sensitivity (Recall):", round(sensitivity_lgm*100,3), "%"))
# [1] "Sensitivity (Recall): 60.46 %"
# > print(paste("Precision:", round(precision_lgm*100,3), "%"))
# [1] "Precision: 62.396 %"

############################################################
#Classification Tree

library(caret)
library(gains)
library (rpart)
library(rpart.plot)
library(pROC)
library(ROSE)

myData$Churn <- factor(myData$Churn)
set.seed(1)

#myData <- ROSE(myData$Churn ~ myData$TotalCharges + myData$tenure, data = myData$Churn, seed = 123)$data
#View(myData)
selected_columns <- c("Churn", "tenure","MonthlyCharges", "TotalCharges")
myData <- myData[selected_columns]

myIndex <- createDataPartition(myData$Churn, p=0.7, list = FALSE)
trainSet <- myData[myIndex, ]
#View(trainSet)
validationSet <- myData[-myIndex, ]
#View(validationSet)

#################################################
#default tree
default_tree <- rpart(Churn ~  tenure + TotalCharges + MonthlyCharges, data = trainSet, method = "class", weights = ifelse(Churn == "Yes", 1.75,1))
summary(default_tree)

prp(default_tree, type=1, extra=1, under = TRUE)
predictions <- predict(default_tree, newdata = validationSet, type = "class")

# Confusion matrix to evaluate performance measures default tree
conf_matrix <- table(predictions, validationSet$Churn)

misclassification_rate <- (conf_matrix[2, 1] + conf_matrix[1, 2]) / sum(conf_matrix)
accuracy <- 1-misclassification_rate
specificity <- conf_matrix[1, 1] / (conf_matrix[1, 1] + conf_matrix[1, 2])
sensitivity <- conf_matrix[2, 2] / (conf_matrix[2, 1] + conf_matrix[2, 2])
precision <- conf_matrix[2, 2] / (conf_matrix[1, 2] + conf_matrix[2, 2])

# Print results
print(paste("Misclassification Rate:", round(misclassification_rate*100, 3), "%"))
print(paste("Accuracy:", round(accuracy*100,3), "%"))
print(paste("Specificity:", round(specificity*100,3), "%"))
print(paste("Sensitivity (Recall):", round(sensitivity*100,3), "%"))
print(paste("Precision:", round(precision*100,3), "%"))

# > print(paste("Misclassification Rate:", round(misclassification_rate*100, 3), "%"))
# [1] "Misclassification Rate: 21.205 %"
# > print(paste("Accuracy:", round(accuracy*100,3), "%"))
# [1] "Accuracy: 78.795 %"
# > print(paste("Specificity:", round(specificity*100,3), "%"))
# [1] "Specificity: 85.448 %"
# > print(paste("Sensitivity (Recall):", round(sensitivity*100,3), "%"))
# [1] "Sensitivity (Recall): 60.18 %"
# > print(paste("Precision:", round(precision*100,3), "%"))
# [1] "Precision: 59.643 %"

# ROC Curve
roc_curve <- roc(trainSet$Churn, predict(default_tree, newdata = trainSet, type = "prob")[, "Yes"])
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Decile-wise Lift Chart
trainSet$predicted_probs <- predict(default_tree, newdata = trainSet, type = "prob")[, "Yes"]
lift_data_train <- trainSet %>%
  mutate(decile = ntile(predicted_probs, 10)) %>%
  group_by(decile) %>%
  summarise(cumulative_positives = sum(Churn == "Yes"),
            cumulative_total = n()) %>%
  mutate(lift = cumulative_positives / cumulative_total / mean(myData$Churn == "Yes"))

# Print decile-wise lift chart
print("Decile-wise Lift Chart:")
print(lift_data_train)
plot(lift_data_train$decile, lift_data_train$lift, type = "l", col = "green", lwd = 2,
     xlab = "Decile", ylab = "Lift", main = "Decile-wise Lift Chart (Training)")

# Cumulative Lift Chart
cum_lift_data_train <- trainSet %>%
  arrange(desc(predicted_probs)) %>%
  mutate(cumulative_positives = cumsum(Churn == "Yes"),
         cumulative_total = 1:length(Churn)) %>%
  mutate(cum_lift = cumulative_positives / cumulative_total / mean(Churn == "Yes"))

# Plot cumulative lift chart
plot(cum_lift_data_train$cum_lift, type = "l", col = "red", lwd = 2,
     xlab = "Percentile", ylab = "Cumulative Lift", main = "Cumulative Lift Chart (Training)")

####################################################
#full tree
predictors <- trainSet[, !(names(trainSet) %in% c("predicted_probs"))]
full_tree <- rpart (Churn ~ ., data= predictors, method="class", cp=0, minsplit=2, minbucket=1, weights = ifelse(Churn == "Yes", 1.75,1))
prp(full_tree, type=1, extra=1, under = TRUE)
printcp(full_tree)
predictions_full <- predict(full_tree, newdata = validationSet, type = "class")

 conf_matrix_full <- table(predictions_full, validationSet$Churn)

misclassification_rate_full <- (conf_matrix_full[2, 1] + conf_matrix_full[1, 2]) / sum(conf_matrix_full)
accuracy_full <- 1-misclassification_rate_full
specificity_full <- conf_matrix_full[1, 1] / (conf_matrix_full[1, 1] + conf_matrix_full[1, 2])
sensitivity_full <- conf_matrix_full[2, 2] / (conf_matrix_full[2, 1] + conf_matrix_full[2, 2])
precision_full <- conf_matrix_full[2, 2] / (conf_matrix_full[1, 2] + conf_matrix_full[2, 2])

# Print results
print(paste("Misclassification Rate:", round(misclassification_rate_full*100, 3), "%"))
print(paste("Accuracy:", round(accuracy_full*100,3), "%"))
print(paste("Specificity:", round(specificity_full*100,3), "%"))
print(paste("Sensitivity (Recall):", round(sensitivity_full*100,3), "%"))
print(paste("Precision:", round(precision_full*100,3), "%"))

# > print(paste("Misclassification Rate:", round(misclassification_rate_full*100, 3), "%"))
# [1] "Misclassification Rate: 27.799 %"
# > print(paste("Accuracy:", round(accuracy_full*100,3), "%"))
# [1] "Accuracy: 72.201 %"
# > print(paste("Specificity:", round(specificity_full*100,3), "%"))
# [1] "Specificity: 81.438 %"
# > print(paste("Sensitivity (Recall):", round(sensitivity_full*100,3), "%"))
# [1] "Sensitivity (Recall): 47.751 %"
# > print(paste("Precision:", round(precision_full*100,3), "%"))
# [1] "Precision: 49.286 %"

# ROC Curve
roc_curve <- roc(trainSet$Churn, predict(full_tree, newdata = trainSet, type = "prob")[, "Yes"])
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Decile-wise Lift Chart
trainSet$predicted_probs <- predict(full_tree, newdata = trainSet, type = "prob")[, "Yes"]
lift_data_train <- trainSet %>%
  mutate(decile = ntile(predicted_probs, 10)) %>%
  group_by(decile) %>%
  summarise(cumulative_positives = sum(Churn == "Yes"),
            cumulative_total = n()) %>%
  mutate(lift = cumulative_positives / cumulative_total / mean(myData$Churn == "Yes"))

# Print decile-wise lift chart
print("Decile-wise Lift Chart:")
print(lift_data_train)
plot(lift_data_train$decile, lift_data_train$lift, type = "l", col = "green", lwd = 2,
     xlab = "Decile", ylab = "Lift", main = "Decile-wise Lift Chart (Training)")

# Cumulative Lift Chart
cum_lift_data_train <- trainSet %>%
  arrange(desc(predicted_probs)) %>%
  mutate(cumulative_positives = cumsum(Churn == "Yes"),
         cumulative_total = 1:length(Churn)) %>%
  mutate(cum_lift = cumulative_positives / cumulative_total / mean(Churn == "Yes"))

# Plot cumulative lift chart
plot(cum_lift_data_train$cum_lift, type = "l", col = "red", lwd = 2,
     xlab = "Percentile", ylab = "Cumulative Lift", main = "Cumulative Lift Chart (Training)")

######################################################
# #pruned tree
#Dynamic cp values
xerror_values <- full_tree$cptable[, "xstd"]
min_xerror_index <- which.min(xerror_values)
#min_xerror_index
min_cp_value <- full_tree$cptable[min_xerror_index, "CP"]
#min_cp_value

pruned_tree <- prune(full_tree, cp = min_cp_value, weights = ifelse(Churn == "Yes", 1.5,1))
predictions_pruned <- predict(pruned_tree, newdata = validationSet, type = "class")
prp(pruned_tree, type=1, extra=1, under = TRUE)
conf_matrix_pruned <- table(predictions_pruned, validationSet$Churn)

misclassification_rate_pruned <- (conf_matrix_pruned[2, 1] + conf_matrix_pruned[1, 2]) / sum(conf_matrix_pruned)
accuracy_pruned <- 1-misclassification_rate_pruned
specificity_pruned <- conf_matrix_pruned[1, 1] / (conf_matrix_pruned[1, 1] + conf_matrix_pruned[1, 2])
sensitivity_pruned <- conf_matrix_pruned[2, 2] / (conf_matrix_pruned[2, 1] + conf_matrix_pruned[2, 2])
precision_pruned <- conf_matrix_pruned[2, 2] / (conf_matrix_pruned[1, 2] + conf_matrix_pruned[2, 2])

print(paste("Misclassification Rate:", round(misclassification_rate_pruned*100, 3), "%"))
print(paste("Accuracy:", round(accuracy_pruned*100,3), "%"))
print(paste("Specificity:", round(specificity_pruned*100,3), "%"))
print(paste("Sensitivity (Recall):", round(sensitivity_pruned*100,3), "%"))
print(paste("Precision:", round(precision_pruned*100,3), "%"))

# > print(paste("Misclassification Rate:", round(misclassification_rate_pruned*100, 3), "%"))
# [1] "Misclassification Rate: 23.008 %"
# > print(paste("Accuracy:", round(accuracy_pruned*100,3), "%"))
# [1] "Accuracy: 76.992 %"
# > print(paste("Specificity:", round(specificity_pruned*100,3), "%"))
# [1] "Specificity: 87.668 %"
# > print(paste("Sensitivity (Recall):", round(sensitivity_pruned*100,3), "%"))
# [1] "Sensitivity (Recall): 55.38 %"
# > print(paste("Precision:", round(precision_pruned*100,3), "%"))
# [1] "Precision: 68.929 %"

# ROC Curve
roc_curve <- roc(trainSet$Churn, predict(pruned_tree, newdata = trainSet, type = "prob")[, "Yes"])
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Decile-wise Lift Chart
trainSet$predicted_probs <- predict(pruned_tree, newdata = trainSet, type = "prob")[, "Yes"]
lift_data_train <- trainSet %>%
  mutate(decile = ntile(predicted_probs, 10)) %>%
  group_by(decile) %>%
  summarise(cumulative_positives = sum(Churn == "Yes"),
            cumulative_total = n()) %>%
  mutate(lift = cumulative_positives / cumulative_total / mean(myData$Churn == "Yes"))

# Print decile-wise lift chart
print("Decile-wise Lift Chart:")
print(lift_data_train)
plot(lift_data_train$decile, lift_data_train$lift, type = "l", col = "green", lwd = 2,
     xlab = "Decile", ylab = "Lift", main = "Decile-wise Lift Chart (Training)")

# Cumulative Lift Chart
cum_lift_data_train <- trainSet %>%
  arrange(desc(predicted_probs)) %>%
  mutate(cumulative_positives = cumsum(Churn == "Yes"),
         cumulative_total = 1:length(Churn)) %>%
  mutate(cum_lift = cumulative_positives / cumulative_total / mean(Churn == "Yes"))

# Plot cumulative lift chart
plot(cum_lift_data_train$cum_lift, type = "l", col = "red", lwd = 2,
     xlab = "Percentile", ylab = "Cumulative Lift", main = "Cumulative Lift Chart (Training)")

#Default tree is the best model to use in our case, the model is overfitting if trying to increase precision


