
.libPaths(c("C:/Users/aivis/R/win-library/4.5", .libPaths()))
#install.packages("tidyverse")
library(tidyverse)
Cancer_data <- read_csv("C:/Users/sruth/Downloads/The_Cancer_prediction_data.csv")
head(Cancer_data)
str(Cancer_data)
summary(Cancer_data)
#data cleaning 
#identifying the missing values 
colSums(is.na(Cancer_data))
library(dplyr)
library(ggplot2)
#checking for the unique values in the each column
table(Cancer_data$Age)
table(Cancer_data$Gender)
table(Cancer_data$BMI)
table(Cancer_data$Smoking)
table(Cancer_data$GeneticRisk)
table(Cancer_data$PhysicalActivity)
table(Cancer_data$AlcoholIntake)
table(Cancer_data$CancerHistory)
#Exploratory data analysis 
plot_Age <- ggplot(Cancer_data, aes(x = Age , fill = as.factor(Diagnosis
))) + 
  geom_histogram(position = "dodge") + 
  labs(title = "Age Distribution by target Outcome",
       fill = "Diagnosis") + 
  theme_minimal()

plot_Gender <- ggplot(Cancer_data, aes(x = Gender , fill = as.factor(Diagnosis
))) + 
  geom_bar(position = "dodge") + 
  labs(title = "Gender Distribution by target Outcome",
       fill = "Diagnosis") + 
  theme_minimal()

plot_BMI <- ggplot(Cancer_data, aes(x = BMI , fill = as.factor(Diagnosis
))) + 
  geom_histogram(position = "dodge") + 
  labs(title = "BMI Distribution by target Outcome",
       fill = "Diagnosis") + 
  theme_minimal()

plot_Smoking <- ggplot(Cancer_data, aes(x = Smoking , fill = as.factor(Diagnosis
))) + 
  geom_bar(position = "dodge") + 
  labs(title = "Smoking Distribution by target Outcome",
       fill = "Diagnosis") + 
  theme_minimal()

plot_GeneticRisk <- ggplot(Cancer_data, aes(x = GeneticRisk , fill = as.factor(Diagnosis
))) + 
  geom_bar(position = "dodge") + 
  labs(title = "GeneticRisk Distribution by target Outcome",
       fill = "Diagnosis") + 
  theme_minimal()

plot_PhysicalActivity <- ggplot(Cancer_data, aes(x = PhysicalActivity , fill = as.factor(Diagnosis
))) + 
  geom_histogram(position = "dodge") + 
  labs(title = "PhysicalActivity Distribution by target Outcome",
       fill = "Diagnosis") + 
  theme_minimal()

plot_AlcoholIntake <- ggplot(Cancer_data, aes(x = AlcoholIntake , fill = as.factor(Diagnosis
))) + 
  geom_histogram(position = "dodge") + 
  labs(title = "AlcoholIntake Distribution by target Outcome",
       fill = "Diagnosis") + 
  theme_minimal()

plot_CancerHistory <- ggplot(Cancer_data, aes(x = CancerHistory , fill = as.factor(Diagnosis
))) + 
  geom_bar(position = "dodge") + 
  labs(title = "CancerHistory Distribution by target Outcome",
       fill = "Diagnosis") + 
  theme_minimal()



#install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot_Age,
             plot_Gender,
             plot_BMI,
             plot_Smoking,
             plot_GeneticRisk,
             plot_PhysicalActivity,
             plot_AlcoholIntake,
             plot_CancerHistory,ncol = 2)

#checking for the missing values in each column

colSums(is.na(Cancer_data))

#function to impute missing with the mean 

impute_mean <- function(Cancer_data, columns)
{
  Cancer_data %>% mutate(across(all_of(columns), ~ ifelse(is.na(.),mean(.,na.rm=TRUE), .)))
}

#input mean for the numerical values http://127.0.0.1:30887/graphics/63ebdeb9-69d6-4afc-985d-c8d7ccbda0e1.png
Cancerpred_data <- impute_mean(Cancer_data, 
                               c("Age",	
                                 "BMI",
                                 "PhysicalActivity",
                                 "AlcoholIntake"))


#input mode for the categorical columns

impute_mode <-function(Cancer_data, columns)
{
  mode_function <- function(x) names(sort(table(x), decreasing = TRUE))[1]
  
  Cancer_data %>%
    mutate(across(all_of(columns),~ifelse(is.na(.), mode_function(.),  .)))
}

#apply the function to the relevant columns

Cancerpred_data  <-impute_mode(Cancer_data,
                               c("Gender", "Smoking",
                                 "GeneticRisk", 
                                 "CancerHistory"))
#remove Duplicate rows
Cancerpred_data <- Cancerpred_data %>% distinct()

# Create RiskLevel based on your dataset features
Cancerpred_data <- Cancerpred_data %>% 
  mutate(Risk_level= ifelse( BMI >= 30 | Smoking == 1 
                             | GeneticRisk >=2 | 
                               CancerHistory == 1, "High",
                             ifelse(
                               (BMI >= 25 & BMI < 30) |
                                 AlcoholIntake > 4 |
                                 PhysicalActivity < 3,"Medium",
                               "Low"
                             )))

#checking dataset after cleaning 
head(Cancerpred_data)
summary(Cancerpred_data)
#checking Bmi Distribution before scaling 
ggplot(Cancerpred_data, aes(x = BMI)) +
  geom_histogram(fill = "lightblue") +
  labs(title = "BMI Distribution Before Scaling")

#encoding Variables is not needed as all are numerical variables 
#Model Building 
#Split the data into 80% training and 20% testing
#Caret is using for spling the data 
#Using logestic Regression model 
library(caret) 
set.seed(42)
trainIndex <- createDataPartition(Cancerpred_data$Diagnosis, p = 0.8 , list = FALSE  )
train_data <- Cancerpred_data[trainIndex,]
test_data <- Cancerpred_data[-trainIndex,]

#separate train features and labels

train_features <- train_data %>% select(-Diagnosis)
train_labels <- train_data$Diagnosis

#making sure that we do not have missing values in the test data
sum(is.na(test_data))

#checking Bmi distribution after scaling 
ggplot(train_data, aes(x = BMI)) +
  geom_histogram(fill = "lightgreen") +
  labs(title = "BMI Distribution After Scaling")

#train the model
model <- glm(Diagnosis ~ ., data = train_data, family = binomial)
#save the trained model to a file
saveRDS(model, file="Cancer_Prediction_model.rds")
#load the save file 

loaded_model <- readRDS("Cancer_Prediction_model.rds")

#predict on test data

predictions <- predict(loaded_model, newdata = test_data, type= "response")
predicted_classes <- ifelse(predictions >  0.5, 1, 0)

#calculate accuracy

accuracy <- mean(predicted_classes == test_data$Diagnosis , na.rm = TRUE)
print(paste("Logistic Regression Accuracy:",accuracy))

#generate confusion matrix

confusion <- confusionMatrix(factor(predicted_classes),factor(test_data$Diagnosis))
#print(paste("confusion:",confusion))
#convert the confusion matrix to a data frame for plotting

confusion_df  <-as.data.frame(confusion$table)
colnames(confusion_df)  <- c("Predicted", "Actual", "Frequency")


library(ggplot2)

ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frequency), color = "black", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix for Logistic regreson ",
       x = "Actual Class",
       y = "Predicted class") +
  theme_minimal()


library(pROC)

# Use probability of class "1"
logit_prob <- predictions   # this is already between 0 and 1

# Compute ROC
roc_obj <- roc(response = test_data$Diagnosis, predictor = logit_prob)

# Print AUC
auc_value <- auc(roc_obj)
print(paste("AUC =", auc_value))

dev.new()
# Plot ROC curve
plot(roc_obj, main = "ROC Curve )",
     print.auc =TRUE ,
     col = "blue")

#Training with the SVM model
library(e1071) 
library(caTools)
library(caret) 
set.seed(42)

split <- sample.split(Cancerpred_data$Diagnosis, SplitRatio = 0.8)

training_set <- subset(Cancerpred_data,split == TRUE)
test_set <- subset(Cancerpred_data, split == FALSE)

print(paste("Training set size:", nrow(training_set)))
print(paste("Test set size:", nrow(test_set)))

print("Training set balance")
prop.table(table(training_set$Diagnosis))

training_set$Diagnosis <- factor(training_set$Diagnosis, levels = c(0,1))
test_set$Diagnosis <- factor(test_set$Diagnosis, levels = c(0,1))

#scale the data 

Scale_col <- c("Age", "BMI", "PhysicalActivity", "AlcoholIntake")


#Calculating Scaling parameters

preprocvalues <- preProcess(training_set[,Scale_col], method= c("center", "scale"))

training_set[,Scale_col] <- predict(preprocvalues,training_set[,Scale_col])

test_set[,Scale_col] <- predict(preprocvalues,test_set[,Scale_col])

print("----Summary of Scaled Training Data-----")
summary(training_set)

#svm model on the training_set
svm_model <- svm(formula = Diagnosis ~ ., 
                 data = training_set, 
                 kernel = "radial",
                 probability = TRUE)
print(svm_model)

#making predictions
predict_svm <-predict(svm_model, newdata = test_set[, -which(names(test_set) == "Diagnosis")],
                      probability = TRUE)
print(predict_svm)

accuracy <- mean(predict_svm == test_set$Diagnosis)
print(paste("SVM Accuracy:", accuracy))

#generate confusion matrix

confusionm <- confusionMatrix(data=predict_svm, test_set$Diagnosis)
print(confusionm)

#plot the confusion matrix

print("Generating confusion matrix plot..")

confusionma_table <- as.data.frame(confusionm$table)
#install.packages("reshape2")
library(reshape2)
library(ggplot2)

confusionm_melted <- melt(confusionma_table)

ggplot(data = confusionm_melted, aes(x = Reference, y = Prediction, fill = value)) + 
  geom_tile()+
  geom_text(aes(label = value), color ="black",size = 6)+
  scale_fill_gradient(low = "white", high = "blue")+
  labs(x = "predicted class" , y ="Actual (Reference) class",
       title = "confusion Matrix for SVM") +
  theme_minimal() + 
  theme(legend.position = "none") 

print(confusionm_melted)


#plot roc curve

prob_predictions <- predict(svm_model, newdata =  test_set[ , -which(names(test_set) == "Diagnosis")],
                            probability = TRUE)
prob_positive  <- attr(prob_predictions, "probabilities")

print(prob_positive)
#install.packages("pROC")
library(pROC)

prob_cls <- prob_positive[, 1]

roc_obj <- roc(response = test_set$Diagnosis, predictor = prob_cls)
auc_value <- auc(roc_obj)

print(roc_obj)
print(paste("AUC value", round(auc_value,4)))

dev.new()
plot(roc_obj, main = "ROC Curve )",
     print.auc =TRUE ,
     col = "purple") 

#comparition Curve for logestic and SVM
# Logistic Regression probabilities
logit_prob <- predictions  # already between 0 and 1

# SVM probabilities
prob_predictions <- predict(svm_model, newdata =  test_set[ , -which(names(test_set) == "Diagnosis")],
                            probability = TRUE)
prob_positive <- attr(prob_predictions, "probabilities")
svm_prob <- prob_positive[, "1"]  # probability of class "1"

library(pROC)

roc_logit <- roc(response = test_labels, predictor = logit_prob)
roc_svm   <- roc(response = test_set$Diagnosis, predictor = svm_prob)

# Base plot for Logistic Regression
plot(roc_logit, col="blue", lwd=2, main="ROC Curve Comparison", print.auc=FALSE)

# Add SVM ROC
lines(roc_svm, col="purple", lwd=2)


# Add legend
legend("bottomright",
       legend = c(
         paste0("Logistic (AUC=", round(auc(roc_logit), 2), ")"),
         paste0("SVM (AUC=", round(auc(roc_svm), 2), ")")
       ),
       col = c("blue","purple"), lwd = 2)
