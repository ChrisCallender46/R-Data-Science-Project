data(dhfr)

sum(is.na(dhfr))

# If data is clean, randomly introduce NA to the dataset

na.gen <- function(data,n) {
  i <- 1
  while (i < n+1) {
    idx1 <- sample(1:nrow(data), 1)
    idx2 <- sample(1:ncol(data), 1)
    data[idx1,idx2] <- NA
    i = i+1
  }
  return(data)
}

dhfr <- dhfr[,-1]

dhfr <- na.gen(dhfr,100)

sum(is.na(dhfr))

colSums(is.na(dhfr))

str(dhfr)

missingdata <- dhfr[!complete.cases(dhfr), ]

sum(is.na(missingdata))

# MEAN
dhfr.impute <- dhfr

for (i in which(sapply(dhfr.impute, is.numeric))) { 
  dhfr.impute[is.na(dhfr.impute[, i]), i] <- mean(dhfr.impute[, i],  na.rm = TRUE) 
}

sum(is.na(dhfr.impute))

