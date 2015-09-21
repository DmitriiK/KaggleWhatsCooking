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

load(file="submission_data999.rda")
load(file="train_data999.rda")


# Further partitioning our original training data into training and test sets
inTrain         = createDataPartition(train_data$cuisine, p = 0.8)[[1]]
training        <- train_data[inTrain,]
remainder        <- train_data[-inTrain,]


####






#### MODELS


# Generalised linear model

# Parameters for caret's train
fitControl <- trainControl(method = "cv",        # do repeated Cross Validation
                           number = 3)

GLM <- train(cuisine~.,
             data = training[1:15000,-1], 
             method = "bayesglm",
             tuneLength = 2,
             trControl = fitControl
)

prediction_GLM <- predict(GLM, newdata = remainder[-1])
confusionMatrix(remainder$cuisine, prediction_GLM)

save(ctree, file = "ctree.rda")
load(file = "ctree.rda")






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
mtryGrid <- expand.grid(mtry = c(3, 7, 12, 18)) # you can put different values for mtry
rfControl <- trainControl(method = "oob")                  

rf <- train(cuisine~.,
               data = train_data[-1], 
               method = "rf",
               tuneGrid = mtryGrid,
               tuneLength = 1,
               ntree = 1500,
               trControl = rfControl,
               varImp = TRUE
)
prediction_rf <- predict(rf, newdata = remainder[-1])
confusionMatrix(remainder$cuisine, prediction_rf)
save(rf, file = "rf.rda")
load(file = "rf.rda")



# NNET3,5,10,15,20,30,
nnetGrid=expand.grid(.size=c(5),.decay=c(0.001,0.05,0.1,0.2))

nnet <- train(cuisine~.,
            data = training[-1], 
            method = "nnet",
            tuneGrid = nnetGrid,
            max.iter = 1000,
            MaxNWts = 100000
            
)
prediction_nnet <- predict(nnet, newdata = remainder[-1])
confusionMatrix(remainder$cuisine, prediction_nnet)
save(nnet, file = "nnet.rda")
load(file = "nnet.rda")


# RPART


rpart <- train(cuisine~.,
              data = training[-1], 
              method = "rpart",
              trControl = fitControl
              
)
prediction_rpart <- predict(rpart, newdata = remainder[-1])
confusionMatrix(remainder$cuisine, prediction_rpart)
save(rpart, file = "rpart.rda")
load(file = "rpart.rda")


