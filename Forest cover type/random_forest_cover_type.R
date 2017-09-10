# Importing the dataset
training_set = read.csv('train.csv')
test_set = read.csv('test.csv')

#merge together Soil and Wilderness variable, to decrease the # of features
training_set$Soil <- apply(training_set[grep("Soil_Type+", colnames(training_set))], 1, function(x) which(x == 1))
training_set$Wilderness <- apply(training_set[grep("Wilderness_Area+", colnames(training_set))], 1, function(x) which(x == 1))

test_set$Soil <- apply(test_set[grep("Soil_Type+", colnames(test_set))], 1, function(x) which(x == 1))
test_set$Wilderness <- apply(test_set[grep("Wilderness_Area+", colnames(test_set))], 1, function(x) which(x == 1))

#check if anything is NA
na_col_train <- which(unlist(lapply(training_set, function(x) any(is.na(x)))))
na_col_test <- any(unlist(lapply(test_set, function(x) any(is.na(x)))))

#transform the response variable in a factor
training_set$Cover_Type = factor(training_set$Cover_Type)

#create a new train and test set with the limited set of variables
new_training <- training_set[,c(1:11, 56,57,58)]
new_test <- test_set[,c(1:11,56,57)]

#modelling
library(randomForest)
classifier = randomForest(Cover_Type ~ ., data= new_training, ntree = 10000, nodeSize=150)
#classifier = randomForest(Cover_Type ~ ., data= training_set, ntree = 10000)

# Show model error
plot(classifier, ylim=c(0,0.36))
legend('topright', colnames(classifier$err.rate), col=1:3, fill=1:3)

#predict 
y_pred = predict(classifier, type = 'response', newdata = new_test)

#write result to csv
result <- data.frame(Id = test_set$Id, Cover_Type = y_pred)
write.csv(x = result, file = 'result.csv', row.names = FALSE, quote = FALSE)
