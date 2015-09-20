# Clear Workspace
rm(list = ls())

# Necessary libraries
library(caret)
library(plyr)
library(Metrics)
library(mice)
library(ggplot2)
library(RColorBrewer)
library(rms)
library(qdap)
library(tm)


# Set seed for reproducibility and also set working directory
set.seed(1)
setwd("C:/Users/User/Documents/R/kaggle_cooking/")

load(file="submission_data.rda")
load(file="train_data.rda")


# Further partitioning our original training data into training and test sets
inTrain         = createDataPartition(train_data$cuisine, p = 0.8)[[1]]
training        <- train_data[inTrain,]
remainder        <- train_data[-inTrain,]


####


# Parameters for caret's train
fitControl <- trainControl(method = "repeatedcv",        # do repeated Cross Validation
                           number = 2,                   # 2-fold
                           repeats = 1)                  # repeat 2 times each 


#### MODELS


# Generalised linear model
ctree <- train(cuisine~.,
             data = training[-1], 
             method = "rf",
             tuneLength = 1,
             trControl = fitControl
)
save(ctree, file = "ctree.rda")
load(file = "ctree.rda")

prediction_ctree <- predict(ctree, newdata = remainder[-1])
confusionMatrix(remainder$cuisine, prediction_ctree)






# GBM
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:3)*250,
                        shrinkage = 0.1)
                       # n.minobsinnode = 20)


gbm <- train(cuisine~.,
               data = training[-1], 
                method = "gbm",
                trControl = fitControl,
                verbose = FALSE,
            tuneGrid = gbmGrid
)
save(gbm, file = "gbm.rda")
load(file = "gbm.rda")

prediction_gbm <- predict(gbm, newdata = remainder[-1])
confusionMatrix(remainder$cuisine, prediction_gbm)



# RF
mtryGrid <- expand.grid(mtry = 22) # you can put different values for mtry


rf <- train(cuisine~.,
               data = training[-1], 
               method = "rf",
               ntree  = 1500,
               tuneGrid = mtryGrid,
               tuneLength = 1,
               trControl = fitControl
)
save(ctree, file = "rf.rda")
load(file = "rf.rda")

prediction_rf <- predict(rf, newdata = remainder[-1])
confusionMatrix(remainder$cuisine, prediction_rf)

