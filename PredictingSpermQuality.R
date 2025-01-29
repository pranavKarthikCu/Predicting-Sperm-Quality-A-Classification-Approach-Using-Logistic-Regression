# Load libraries
library(readxl)
library(caret)
library(dplyr)
library(ggplot2)
library(pROC)

# Load dataset
sq_data <- read_excel("C:/Users/prana/Desktop/UTA/5303-Stats and Computing/Final_project/Fertility_Dataset.xlsx")
summary(sq_data)
str(sq_data)
head(sq_data)

# Clean and reformat column names
colnames(sq_data) <- gsub("\t", "", colnames(sq_data))

# Drop rows with missing values
sq_data <- na.omit(sq_data)

############ visualisation #####################
#Comparison of Alcohol and Smoking Impacts on Sperm Quality

# Prepare data for smoking and alcohol impacts separately

alcohol_summary <- sq_data %>% 
  
  group_by(alcohol) %>% 
  
  count(Diagnosis) %>% 
  
  mutate(Proportion = n / sum(n))

smoking_summary <- sq_data %>% 
  
  group_by(smoking) %>% 
  
  count(Diagnosis) %>% 
  
  mutate(Proportion = n / sum(n))

# Plot side-by-side bar charts for comparison

p1 <- ggplot(alcohol_summary, aes(x = factor(alcohol), y = Proportion, fill = factor(Diagnosis))) +
  
  geom_bar(stat = "identity", position = "fill") +
  
  labs(title = "Impact of Alcohol Consumption on Sperm Quality", x = "Alcohol Consumption (1 = Every day, 4 = Hardly ever or never)", y = "Proportion", fill = "Diagnosis (1 = Normal, 0 = Altered)") +
  
  theme_minimal() +
  
  scale_fill_viridis_d()

p2 <- ggplot(smoking_summary, aes(x = factor(smoking), y = Proportion, fill = factor(Diagnosis))) +
  
  geom_bar(stat = "identity", position = "fill") +
  
  labs(title = "Impact of Smoking Habits on Sperm Quality", x = "Smoking Habits (-1=Never, 0=Occasional, 1=Daily)", y = "Proportion", fill = "Diagnosis (1 = Normal, 0 = Altered)") +
  
  theme_minimal() +
  
  scale_fill_viridis_d()


# Combine the plots

gridExtra::grid.arrange(p1, p2, ncol = 2, top = "Comparison of Alcohol and Smoking Impacts on Sperm Quality")

########################################################
# 7. Surgical Interventions vs. Diagnosis (Stacked Bar Chart)
surgical_summary <- sq_data %>% 
  group_by(surgical_intervention, Diagnosis) %>% 
  summarise(Count = n()) %>% 
  ungroup()

surgical_plot <- ggplot(surgical_summary, aes(x = factor(surgical_intervention), y = Count, fill = factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Effect of Surgical Interventions on Sperm Quality", x = "Surgical Intervention (0 = No, 1 = Yes)", y = "Count", fill = "Diagnosis (1 = Normal, 0 = Altered)") +
  theme_minimal() +
  scale_fill_viridis_d()

print(surgical_plot)

# 8. High Fevers vs. Diagnosis (Diverging Bar Chart)
sq_data$high_fevers <- recode(sq_data$high_fevers, "Less than 3 months ago" = 1, "More than 3 months ago" = 2, "No" = 3)

fever_summary <- sq_data %>% 
  group_by(high_fevers, Diagnosis) %>% 
  summarise(Count = n()) %>% 
  mutate(Divergence = ifelse(Diagnosis == 1, Count, -Count))

fever_plot <- ggplot(fever_summary, aes(x = factor(high_fevers), y = Divergence, fill = factor(Diagnosis))) +
  geom_bar(stat = "identity") +
  labs(title = "Impact of High Fevers on Sperm Quality", x = "High Fevers (1 = Less than 3 months ago, 2 = More than 3 months ago, 3 = No)", y = "Count", fill = "Diagnosis (1 = Normal, 0 = Altered)") +
  theme_minimal() +
  scale_fill_viridis_d()

print(fever_plot)

##################################################
trauma_summary <- sq_data %>% 
  group_by(accident, Diagnosis) %>% 
  summarise(Count = n()) %>% 
  group_by(accident) %>% 
  mutate(Proportion = Count / sum(Count)) %>% 
  ungroup()

trauma_plot <- ggplot(trauma_summary, aes(x = factor(accident), y = Proportion, fill = factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion of Diagnosis by Trauma", x = "Trauma (0 = No, 1 = Yes)", y = "Proportion", fill = "Diagnosis (1 = Normal, 0 = Altered)") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue"))

print(trauma_plot)



########### Data Preprocessing ##################

# Recode categorical variables
sq_data$Season <- recode(sq_data$Season, "Winter" = -1, "Spring" = -0.33, "Summer" = 0.33, "Fall" = 1)
sq_data$Child_Diseases <- recode(sq_data$Child_Diseases, "Yes" = 0, "No" = 1)
sq_data$accident <- recode(sq_data$accident, "Yes" = 0, "No" = 1)
sq_data$surgical_intervention <- recode(sq_data$surgical_intervention, "Yes" = 0, "No" = 1)

sq_data$high_fevers <- recode(sq_data$high_fevers, "Less than 3 months ago" = -1, 
                              "More than 3 months ago" = 0, "No" = 1)
sq_data$alcohol <- recode(sq_data$alcohol, "Every day" = 0.2, "Several times a week" = 0.6, 
                          "Once a week" = 0.8, "Hardly ever or never" = 1)
sq_data$smoking <- recode(sq_data$smoking, "Never" = -1, "Occasional" = 0, "Daily" = 1)
sq_data$Diagnosis <- recode(sq_data$Diagnosis, "Normal" = 1, "Altered" = 0)

# Normalize continuous variables
sq_data$Age <- scales::rescale(sq_data$Age, to = c(0, 1))
sq_data$hrs_sitting <- scales::rescale(sq_data$hrs_sitting, to = c(0, 1))

head(sq_data)

# Balance classes using oversampling
minority <- sq_data[sq_data$Diagnosis == 0, ]
majority <- sq_data[sq_data$Diagnosis == 1, ]
oversampled_minority <- minority[sample(1:nrow(minority), nrow(majority), replace = TRUE), ]
balanced_data <- rbind(majority, oversampled_minority)
head(balanced_data)
summary(balanced_data)
str(balanced_data)
unique(balanced_data$Season)

# Split data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(balanced_data$Diagnosis, p = 0.8, list = FALSE)
trainData <- balanced_data[trainIndex, ]
testData <- balanced_data[-trainIndex, ]

# Fit logistic regression model
logit_model <- glm(Diagnosis ~ ., data = trainData, family = binomial())
summary(logit_model)

# Predict on test data
logit_prob <- predict(logit_model, newdata = testData, type = "response")
logit_pred <- ifelse(logit_prob > 0.5, 1, 0)

# Ensure predictions and actual values are factors
logit_pred <- factor(logit_pred, levels = c(0, 1))
testData$Diagnosis <- factor(testData$Diagnosis, levels = c(0, 1))

# Confusion matrix
conf_matrix <- confusionMatrix(logit_pred, testData$Diagnosis)
print(conf_matrix)
# Extract metrics
specificity <- conf_matrix$byClass["Specificity"]
sensitivity <- conf_matrix$byClass["Sensitivity"] # Same as recall
precision <- conf_matrix$byClass["Pos Pred Value"]

# Display metrics
print(paste("Specificity:", round(specificity, 2)))
print(paste("Recall (Sensitivity):", round(sensitivity, 2)))
print(paste("Precision:", round(precision, 2)))
#############################

# Predict on the training data
logit_prob_train <- predict(logit_model, newdata = trainData, type = "response")
logit_pred_train <- ifelse(logit_prob_train > 0.5, 1, 0)

# Ensure predictions and actual values are factors for training data
logit_pred_train <- factor(logit_pred_train, levels = c(0, 1))
trainData$Diagnosis <- factor(trainData$Diagnosis, levels = c(0, 1))

# Training confusion matrix
conf_matrix_train <- confusionMatrix(logit_pred_train, trainData$Diagnosis)
print(conf_matrix_train)
# Training accuracy
train_accuracy <- conf_matrix_train$overall["Accuracy"]

# Test accuracy (already computed in existing code)
test_accuracy <- conf_matrix$overall["Accuracy"]

# Print results
print(paste("Training Accuracy:", round(train_accuracy, 2)))
print(paste("Test Accuracy:", round(test_accuracy, 2)))



##############################
# ROC curve and AUC
roc_obj <- roc(testData$Diagnosis, logit_prob)
plot(roc_obj, main = "Logistic Regression ROC Curve", col = "blue")
auc_value <- auc(roc_obj)
print(paste("AUC:", round(auc_value, 2)))



