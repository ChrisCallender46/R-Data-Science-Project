data("BostonHousing")

head(BostonHousing)

sum(is.na(BostonHousing))

set.seed(100)

TrainingIndex <- createDataPartition(BostonHousing$medv, p=0.8, list = FALSE)
TrainingSet <- BostonHousing[TrainingIndex,] # Training Set
TestingSet <- BostonHousing[-TrainingIndex,] # Test Set

# Build Training model
Model <- train(medv ~ ., data = TrainingSet,
               method = "lm",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none")
)

# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set

summary(Model.training)
summary(Model.testing)

# Model performance (Displays scatter plot and performance metrics)
# Scatter plot of Training set
plot(TrainingSet$medv,Model.training, col = "purple" )
plot(TestingSet$medv,Model.testing, col = "purple" )

summary(Model)

R.training <- cor(TrainingSet$medv,Model.training)
R.testing <- cor(TestingSet$medv,Model.testing)
R2.training <- R.training**2
R2.testing <- R.testing**2


