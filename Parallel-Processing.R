data(dhfr)

sum(is.na(dhfr))

set.seed(100)

TrainingIndex <- createDataPartition(dhfr$Y, p=0.8, list = FALSE)
TrainingSet <- dhfr[TrainingIndex,] # Training Set
TestingSet <- dhfr[-TrainingIndex,] # Test Set



###############################
# Random forest


# Run normally without parallel processing
start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf" # Learning algorithm
)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

# Time taken is 130.58 seconds

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf" # Learning algorithm
)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopCluster(cl)

# Time taken is 38.05 seconds

130.58 / 38.05

##########################

# Run without parallel processing

registerDoSEQ()

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf", # Learning algorithm
               tuneGrid = data.frame(mtry = seq(5,15, by=5))
)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

#Time taken is 55.65 seconds

# Using doParallel

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf", # Learning algorithm
               tuneGrid = data.frame(mtry = seq(5,15, by=5))
)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopCluster(cl)

# Time taken is 25.44 seconds

55.65 / 25.44


##########################
# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Y)

print(Model.training.confusion)

# Feature importance
Importance <- varImp(Model)
plot(Importance, top = 25)
plot(Importance, col = "red")
