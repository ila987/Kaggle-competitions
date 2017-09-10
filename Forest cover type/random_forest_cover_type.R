# Importing the dataset
training_set = read.csv('train.csv')
test_set = read.csv('test.csv')

na_col_train <- which(unlist(lapply(training_set, function(x) any(is.na(x)))))
na_col_test <- any(unlist(lapply(test_set, function(x) any(is.na(x)))))

training_set$Cover_Type = factor(training_set$Cover_Type)


#training_set <- dataset[1:15120,]
#test_set <- dataset[15121:1309,]

# Feature Scaling
library(randomForest)
classifier = randomForest(Cover_Type ~ ., data= training_set, ntree = 10000, nodeSize=150)
#classifier = randomForest(Cover_Type ~ ., data= training_set, ntree = 10000)

y_pred = predict(classifier, type = 'response', newdata = test_set)

#write result to csv
result <- data.frame(Id = test_set$Id, Cover_Type = y_pred)
write.csv(x = result, file = 'result.csv', row.names = FALSE, quote = FALSE)
