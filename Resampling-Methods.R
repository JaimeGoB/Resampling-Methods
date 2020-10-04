library(caret)
library(e1071)

################################################
# 2) Consider the German credit dataset from #1. 
# Although in the class we have discussed cv.glm for computing test error rates 
#using cross-validation, you may use the caret package 
#for doing so as it is not restricted to the GLMs.
################################################
german_credit <- read.csv("germancredit.csv")

#checking for missing data
sum(is.na(german_credit))
sum(is.null(german_credit))

#Changing ShellType from chr to factor
german_credit$Default <- factor(german_credit$Default)
german_credit$checkingstatus1 = as.factor(german_credit$checkingstatus1)
german_credit$history = as.factor(german_credit$history)
german_credit$purpose = as.factor(german_credit$purpose)
german_credit$savings = as.factor(german_credit$savings)
german_credit$employ = as.factor(german_credit$employ)
german_credit$status = as.factor(german_credit$status)
german_credit$others = as.factor(german_credit$others)
german_credit$property = as.factor(german_credit$property)
german_credit$otherplans = as.factor(german_credit$otherplans)
german_credit$housing = as.factor(german_credit$housing)
german_credit$job = as.factor(german_credit$job)
german_credit$tele = as.factor(german_credit$tele)
german_credit$foreign = as.factor(german_credit$foreign)
str(german_credit)

# (a) Fit a logistic regression model using all predictors in the data. 
#Provide its error rate, sensitivity, and specificity based on training data.
full_model = glm(Default ~ ., 
                  family = binomial, 
                  data = german_credit)

train_X <- german_credit[,-1]
train_Y <- german_credit[,1]
predict_glm <- predict(full_model, train_X, type = "response")
prediction <- ifelse(predict_glm >= 0.5, "1", "0")
confusion_matrix <- table(prediction, train_Y)

glm_train_error = mean(prediction!=german_credit$Default)
sensitivity_glm <- confusion_matrix[2,2] / (confusion_matrix[1,2] + confusion_matrix[2,2])
specificity_glm <-confusion_matrix[1,1] / (confusion_matrix[1,1] + confusion_matrix[2,1])

glm_train_error
sensitivity_glm
specificity_glm
#(b) Write your own code to estimate the test error rate of the model in (a) using LOOCV.
loocv_error_vector = c()

for (observation in 1:nrow(german_credit)) {
  train_set <- german_credit[-observation, ]
  test_set <- german_credit[observation, ]
  
  model <- glm(Default ~ ., family = binomial, data = train_set)
  
  germanpredict <- predict(model, test_set, type = "response")
  prediction <- ifelse(germanpredict >= 0.5, "1", "0")

  loocv_error_vector[observation] <- mean(prediction!=test_set$Default)
}

loocv_from_scratch_error = sum(loocv_error_vector) / length(loocv_error_vector)

#(c) Verify your results in (b) using a package. Make sure the two results match.train.control <- trainControl(method = "LOOCV")
train.control <- trainControl(method = "LOOCV")
loocv_caret <- train(Default ~ ., 
                     data = german_credit, 
                     method = "glm",
                     trControl = train.control)
loocv_caret_error =  1 - loocv_caret$results$Accuracy 

#creating a table to checck if LOOCV from sractch and library get same value.
loocv_error_comparison <- cbind(loocv_from_scratch_error, loocv_caret_error)
colnames(loocv_error_comparison) <- c("loocv_from_scratch_error", "loocv_caret_error")
rownames(loocv_error_comparison) <- c("Error")
loocv_error_comparison

# (d) For the logistic regression model you proposed in #1, estimate the test error rate using LOOCV.
glm <- train(Default ~ 
               checkingstatus1 + 
               history + 
               purpose + 
               amount + 
               savings + 
               installment + 
               otherplans + 
               foreign, data = german_credit, method = "glm",
             trControl = train.control)
glm_error  <- 1 - glm$results$Accuracy

# (e) Repeat (d) using LDA from Mini Project #2.
lda <- train(Default ~ 
               checkingstatus1 + 
               history + 
               purpose + 
               amount + 
               savings + 
               installment + 
               otherplans + 
               foreign, data = german_credit, method = "lda",
             trControl = train.control)
lda_error <- 1 - lda$results$Accuracy

# (f) Repeat (d) using QDA from Mini Project #2.
qda <- train(Default ~ 
               checkingstatus1 + 
               history + 
               purpose + 
               amount + 
               savings + 
               installment + 
               otherplans + 
               foreign, data = german_credit, method = "qda",
             trControl = train.control)
qda_error <- round(1 - qda$results$Accuracy, 3)

#(g) Fit a KNN with K chosen optimally using the LOOCV test error rate. 
#Repeat (d) for the optimal KNN. (You may explore tune.knn function 
#for finding the optimal value of K but this is not required.)

knn <- train(Default ~ 
               checkingstatus1 + 
               history + 
               purpose + 
               amount + 
               savings + 
               installment + 
               otherplans + 
               foreign,, data = german_credit, method = "knn",
             trControl = train.control)
knn_error <- 1 - knn$results$Accuracy[2]

#(h) Compare the results from the various classifiers. 
#Which classifier would you recommend? Justify your answer.
all_models_error <- cbind( glm_error, lda_error, qda_error, knn_error)
colnames(all_models_error) <- c("Logistic", "LDA", "QDA", "KNN")
rownames(all_models_error) <- c("Error")
all_models_error

