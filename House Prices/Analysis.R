#RANDOM FOREST CLASSIFIER 

library(dplyr)
library(xgboost)
library(moments)
library(ggplot2)
library(Metrics)

# Importing the dataset
training_set = read.csv('train.csv', stringsAsFactors = FALSE)
test_set = read.csv('test.csv', stringsAsFactors = FALSE)

#plot variables 
qplot(training_set$GrLivArea, training_set$SalePrice, main = "With Outliers")
#correct outlier
training_set <- training_set[!training_set$GrLivArea > 4000, ]

# get data frame of SalePrice and log(SalePrice + 1) for plotting
df <- rbind(data.frame(version="log(price+1)",x=log(training_set$SalePrice + 1)),
            data.frame(version="price",x=training_set$SalePrice))

# plot histogram
ggplot(data=df) +
  facet_wrap(~version,ncol=2,scales="free_x") +
  geom_histogram(aes(x=x))

# transform SalePrice target to log form
training_set$SalePrice <- log(training_set$SalePrice + 1)

#bring train and test set together 
dataset  <- bind_rows(training_set, test_set)


#check if na or empty values 
na_col <- unlist(lapply(dataset, function(x) any(is.na(x))))

dataset$MSSubClass = as.character(dataset$MSSubClass)
dataset$OverallCond = as.character(dataset$OverallCond)
dataset$YrSold = as.character(dataset$YrSold)
dataset$MoSold = as.character(dataset$MoSold)

# first get data type for each feature
feature_classes <- sapply(names(dataset), function(x) {
  class(dataset[[x]])
})
numeric_feats <- names(feature_classes[feature_classes != "character"])

# for numeric feature with excessive skewness, perform log transformation
# determine skew for each numeric feature
skewed_feats <- sapply(numeric_feats,function(x){skewness(dataset[[x]],na.rm=TRUE)})

# keep only features that exceed a threshold for skewness
skewed_feats <- skewed_feats[skewed_feats > 0.75]

# transform excessively skewed features with log(x + 1)
for(x in names(skewed_feats)) {
  dataset[[x]] <- log(dataset[[x]] + 1)
}

# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

for (x in numeric_feats){
  median <- median(dataset[[x]], na.rm= TRUE)
  dataset[[x]][is.na(dataset[[x]])] <- median
}

for (x in categorical_feats){
  dataset[[x]][is.na(dataset[[x]])] <- 'None'
}

#add new feature 
dataset$TotalSF = dataset$TotalBsmtSF + dataset$X1stFlrSF + dataset$X2ndFlrSF


# use caret dummyVars function for hot one encoding for categorical
# features
library(caret)
dummies <- dummyVars(~., dataset[categorical_feats])
categorical_1_hot <- predict(dummies, dataset[categorical_feats])

dataset2 <- cbind(dataset[numeric_feats], categorical_1_hot)
dataset2<-dataset2[,-1]


train <- dataset2[1:1456, ]
test <- dataset2[1457:2915, ]

test$SalePrice<-NA

set.seed(222)
inTrain <- createDataPartition(y = train$SalePrice, p = 0.7, list = FALSE)
train <- train[inTrain, ]
validation <- train[-inTrain, ]


set.seed(123)
## Model parameters trained using xgb.cv function
xgbFit = xgboost(data = as.matrix(train[, -33]), nfold = 5, label = as.matrix(train$SalePrice), 
                 nrounds = 2200, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", 
                 nthread = 8, eta = 0.01, gamma = 0.0468, max_depth = 6, min_child_weight = 1.7817, 
                 subsample = 0.5213, colsample_bytree = 0.4603)
## print(xgbFit)

## Predictions
preds2 <- predict(xgbFit, newdata = as.matrix(validation[, -33]))
rmse(validation$SalePrice, preds2)


set.seed(123)
xgbFit = xgboost(data = as.matrix(train[, -33]), nfold = 5, label = as.matrix(train$SalePrice), 
                 nrounds = 2200, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", 
                 nthread = 8, eta = 0.01, gamma = 0.0468, max_depth = 6, min_child_weight = 1.7817, 
                 subsample = 0.5213, colsample_bytree = 0.4603)
## print(xgbFit)

## Predictions
pred <- exp(predict(xgbFit, newdata = as.matrix(test[, -33])))-1

#write result to csv
result <- data.frame(ID = test_set$Id, SalePrice = pred)
write.csv(x = result, file = 'result.csv', row.names = FALSE, quote = FALSE)