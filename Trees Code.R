library(readxl)
library(ggplot2)
library(lattice)

setwd(r"(/Users/halla.d/Library/Mobile Documents/com~apple~CloudDocs/Desktop/STA401)")
df = read_excel("Organic.xlsx")
head(df)
df = df[-c(1,4,13)] # remove rejected variables
head(df)
df = na.omit(df)
sum(is.na(df))
str(df)
df$TargetBuy = as.factor(df$TargetBuy)
df$DemGender = as.factor(df$DemGender)
df$PromClass = as.factor(df$PromClass)
head(df)
str(df)

######
proportions = table(df$TargetBuy) / nrow(df)
# Display the proportions
print(proportions)

# Create a bar chart
barplot(
  proportions,
  main = "Proportion of TargetBuy",
  xlab = "TargetBuy (1 = Yes, 0 = No)",
  ylab = "Proportion",
  col = c("red", "green"),
  names.arg = c("No", "Yes")
)
#######

ggplot(df, aes(x = TargetBuy, fill = TargetBuy)) +
     geom_bar(alpha = 0.8, width = 0.7) +
     scale_fill_manual(values = c("skyblue", "orange")) +
     labs(
         title = "Distribution of TargetBuy (0 = No, 1 = Yes)",
         x = "TargetBuy",
         y = "Count"
       ) 
     theme_minimal() +
     theme(
         plot.title = element_text(size = 14, face = "bold"),
         axis.title = element_text(size = 12),
         legend.position = "none"
  )

     
     install.packages("caret")
     
library(caret)
train_index <- createDataPartition(df$TargetBuy, p = 0.5, list = FALSE)
train_data <- df[train_index, ]  # Training dataset
test_data <- df[-train_index, ]  # Testing dataset
prop_train <- prop.table(table(train_data$TargetBuy))
prop_test <- prop.table(table(test_data$TargetBuy))
print("Proportion in Training Set:")
print(prop_train)
print("Proportion in Testing Set:")
print(prop_test)

if (!require("rpart")) install.packages("rpart")
library(rpart)
library(rpart.plot)

head(df)
df = df[-1]
head(df)
# Set seed for reproducibility
set.seed(12345)

# Fit the Decision Tree model
tree_model <- rpart(TargetBuy ~ ., data = train_data, method = "class")

# Plot the Decision Tree
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE,
           main = "Decision Tree for TargetBuy")

# Predict on the test dataset
predictions <- predict(tree_model, testData, type = "class")

# Confusion Matrix
conf_matrix <- table(Predicted = predictions, Actual = testData$TargetBuy)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate the test misclassification rate
misclassification_rate <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Test Misclassification Rate:", round(misclassification_rate, 4)))

