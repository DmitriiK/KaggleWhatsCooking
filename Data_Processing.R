# Clear Workspace
rm(list = ls())

# Necessary libraries
library(qdap)
library(rjson)
library(tm)


# Set seed for reproducibility and also set working directory
set.seed(1)
setwd("C:/Users/User/Documents/R/kaggle_cooking/")




make_simple_df <- function(data){
        
        N = length(data) 
        DF = data.frame(id=rep(NA, N),  ingredients=rep("", N),
                         stringsAsFactors=FALSE)          
        ingredients_list = c()
        for (i in (1:N))
        {
                ingredients_list = c(data[[i]]$ing)
                
                # collapsing same ingredients
                ingredients_list[agrep("water", ingredients_list)] <- "water"
                ingredients_list[agrep("ginger", ingredients_list)] <- "ginger"
                ingredients_list[agrep("eggs", ingredients_list)] <- "eggs"
                ingredients_list[agrep("beef", ingredients_list)] <- "beef"
                ingredients_list[agrep("garlic", ingredients_list)] <- "garlic"
                ingredients_list[agrep("milk", ingredients_list)] <- "milk"
                ingredients_list[agrep("onion", ingredients_list)] <- "onion"
                
                
                
                
                ingredients_list = gsub(";", " ", gsub(" ", "", paste(ingredients_list, collapse = ";")))
                
                # deleting unnecessary words from ingredients names
                ingredients_list = gsub("ground","", ingredients_list)
                ingredients_list = gsub("fresh","", ingredients_list)
                ingredients_list = gsub("large","", ingredients_list)

                id = data[[i]]$id
                cuisine = NA
                cuisine = data[[i]]$cuisine
                DF[i, ] <- c(id,  ingredients_list)
        }
        DF$id = as.numeric(DF$id)
        return(DF)
}

get_Y <- function(data){
        
        N = length(data) 
        cuisine = c()
        for (i in (1:N))
        {
                cuisine = c(cuisine, data[[i]]$cuisine)
        }
return(cuisine)
}


### MAIN ###

#reading data
path_train = "train.json"
path_test = "test.json"
data_train <-  fromJSON(file=path_train, method='C')
data_test  <-  fromJSON(file=path_test, method='C')

n = length(data_train)
data <- c(data_train, data_test)

DF_unprocessed <- make_simple_df(data)
cuisine <- get_Y(data_train)

# Make document-term matrix, take only terms with less than 0.995 sparcity
corpus <- VCorpus(VectorSource(DF_unprocessed$ingredients))
dtm <- DocumentTermMatrix(corpus,
                          control = list(removePunctuation = TRUE,
                                         stopwords = TRUE))
dtm_clean <- inspect(removeSparseTerms(dtm, 0.899))

# Make final test (submission) and training datasets and save them
train_data = data.frame(cbind(DF_unprocessed$id[1:n], dtm_clean[1:n,], cuisine))
submission_data = data.frame(cbind(DF_unprocessed$id[-1:-n], dtm_clean[-1:-n,]))

train_data <- data.frame(lapply(train_data, as.factor))
submission_data <- data.frame(lapply(submission_data, as.factor))

save(train_data, file = "train_data899.rda")
save(submission_data, file = "submission_data899.rda")
