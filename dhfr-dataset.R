library(datasets)
data("dhfr")

head(dhfr, 5)
tail(dhfr, 5)
dhfr$Y

summary(dhfr)
summary(dhfr$Y)

sum(is.na(dhfr))

skim(dhfr)

dhfr %>%
  dplyr::group_by(Y) %>%
  skim()

plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol)
plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = "blue")
plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = dhfr$Y)
plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = "blue",
     xlab = "moe2D_zagreb", ylab = "moe2D_weinerPol")

hist(dhfr$moe2D_zagreb)
hist(dhfr$moe2D_zagreb, col = "red")

featurePlot(x = dhfr[,2:25], 
            y = dhfr$Y, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

set.seed(100)

TrainingIndex <- createDataPartition(dhfr$Y, p=0.8, list = FALSE)
TrainingSet <- dhfr[TrainingIndex,] # Training Set
TestingSet <- dhfr[-TrainingIndex,] # Test Set

summary(TrainingSet)
summary(TestingSet)

skim(TrainingSet)
skim(TestingSet)

# Build Training model
Model <- train(Y ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model
Model.cv <- train(Y ~ ., data = TrainingSet,
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
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Y)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Y)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Y)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model)
plot(Importance, top = 25, col = "red")
# This shows the top 25 most important variables
# We can see moe2D_PEOE_VSA.0.1 is the most important








