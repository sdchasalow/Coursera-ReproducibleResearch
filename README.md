# Coursera-ReproducibleResearch
Stuff for Course 5 (Reproducible Research) in the Data Sciences Specialization at Coursera

## A handy code chunk for quickly exploring cross-validated error rates of
## different potential predictors, one at a time
From a week-1 lecture, Structure of a Data Analysis (part 2):

    trainSpam$numType <- as.numeric(trainSpam$type) - 1
    costFunction <- function(x, y) sum(x != (y > 0.5))
    cvError <- rep(NA, 55)
    library(boot)  # Needed for cv.glm function
    for (i in 1:55) {
       lmFormula <- reformulate( names(trainSpam)[i], response = "numType" )
       glmFit <- glm( lmFormula, family = "binomial", data = trainSpam )
       cvError[i] <- cv.glm( trainSpam, glmFit, costFunction, 2)$delta[2]
    }

    ### Which predictor has minimum cross-validated error?
    names(trainSpam)[which.min(cvError)]

## Get a measure of uncertainty

### Best model:
    predictionModel <- glm( numType ~ charDollar, family = "binomial",
       data = trainSpam)

### Get predictions on the test set
    predictionTest <- predict(predictionModel, testSpam)
    predictedSpam <- rep("nonspam", dim(testSpam)[1])

### Classify as 'spam' for those with prob > 0.5
    predictedSpam[predictionModel$fitted > 0.5] <- "spam"

### Confusion matrix (then can compute error rate, sens, spec, etc)
### Could also, e.g. vary the classification threshold and get ROC curve
    table(predictedSpam, testSpam$type)
