#RANDOM FOREST CLASSIFIER 

# Importing the dataset
training_set = read.csv('train.csv')
test_set = read.csv('test.csv')

#bring train and test set together 
dataset  <- bind_rows(training_set, test_set)

# Encoding the target feature as factor
#training_set$Survived = factor(training_set$Survived, levels = c(0, 1))

#correct na or empty values 
na_col <- unlist(lapply(dataset, function(x) any(is.na(x))))
#change value of survived : all test set lines have NAs but don't need to be changed
na_col['Survived'] <- FALSE

#Age
dataset['Age'][is.na(dataset['Age'])]<- median(dataset$Age, na.rm= TRUE)

#Fare
dataset['Fare'][is.na(dataset['Fare'])] <- colMeans(test_set['Fare'], na.rm= TRUE)
dataset$Embarked[which(dataset$Embarked %in% '')] <- 'C'


#add new features
dataset$hasCabin<- ifelse(dataset$Cabin %in% '', 0, 1)
dataset$Ageset<- ifelse(dataset$Age>20, 1, 0)
dataset$Family_size <- dataset$SibSp + dataset$Parch + 1
dataset$Title <- gsub('(.*, )|(\\..*)', '', dataset$Name)
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
dataset$Title[dataset$Title == 'Mlle']        <- 'Miss' 
dataset$Title[dataset$Title == 'Ms']          <- 'Miss'
dataset$Title[dataset$Title == 'Mme']         <- 'Mrs' 
dataset$Title[dataset$Title %in% rare_title]  <- 'Rare Title'

dataset$Title <- as.factor(dataset$Title)
dataset$Embarked <- as.factor (dataset$Embarked)

#split data
training_set <- dataset[1:891,]
test_set <- dataset[892:1309,]

# Fitting random forest classification to the Training set
library(randomForest)

classifier = randomForest(factor(Survived) ~ Pclass + Sex + Age  + SibSp + Parch + Embarked + 
                            Ageset + Fare + hasCabin + Family_size + Title,
                   data= training_set, ntree = 10000)

# Show model error
plot(classifier, ylim=c(0,0.36))
legend('topright', colnames(classifier$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(classifier)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
library('dplyr')
# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 

# Predicting the Test set results
y_pred <- predict(classifier, test_set[-2], type='class')

#write result to csv
result <- data.frame(PassengerID = test_set$PassengerId, Survived = y_pred)
write.csv(x = result, file = 'result.csv', row.names = FALSE, quote = FALSE)

