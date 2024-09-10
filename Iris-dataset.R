library(datasets)
data(iris)
iris$Sepal.Length
iris$Species
Species <- iris$Species
iris

head(iris, 4)
tail(iris, 4)

summary(iris)
summary(iris$Sepal.Length)
fivenum(iris$Sepal.Length)

sum(is.na(iris))

skim(iris)

iris %>%
  dplyr::group_by(Species) %>%
  skim()

plot(iris)
plot(iris, col = "blue")
plot(iris$Sepal.Width, iris$Sepal.Length)
plot(iris$Sepal.Width, iris$Sepal.Length, col = "blue",
     xlab = "Sepal Width", ylab = "Sepal Length")

hist(iris$Sepal.Width)
hist(iris$Sepal.Width, col = "blue")

featurePlot(x = iris[,1:4], 
            y = Species, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

set.seed(100)

TrainingIndex <- createDataPartition(Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,] # Training Set
TestingSet <- iris[-TrainingIndex,] # Test Set

plot(TrainingSet, col = "red")
plot(TestingSet, col = "red")

summary(TrainingSet)
summary(TestingSet)

skim(TrainingSet)
skim(TestingSet)

# The distributions of TrainingSet and TestingSet are similar

hist(TrainingSet$Sepal.Length)
hist(TrainingSet$Sepal.Width)
hist(TrainingSet$Petal.Length)
hist(TrainingSet$Petal.Width)

hist(TestingSet$Sepal.Length)
hist(TestingSet$Sepal.Width)
hist(TestingSet$Petal.Length)
hist(TestingSet$Petal.Width)

# Build Training model
Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model
Model.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model)
plot(Importance)
plot(Importance, col = "red")

# We can see petal length and petal width are the most important variables
# Sepal length is important for setosa and virginica, but not that important for versicolor
# Sepal width is not very important. It is slightly useful for setosa and versicolor, but not important at all for virginica








