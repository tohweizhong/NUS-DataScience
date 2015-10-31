#install.packages("randomForest")
#library(randomForest)

# finally, the random forest model
rf.mod <- randomForest(type ~ ., data = spam.train,
                       ntree = 300,
                       importance = TRUE)
beep()

# Out-of-bag (OOB) error rate as a function of num. of trees:
plot(rf.mod$err.rate[,1], type = "l", lwd = 3, col = "blue",
     main = "Random forest: OOB estimate of error rate",
     xlab = "Number of Trees", ylab = "OOB error rate")

# variable importance
varImpPlot(rf.mod,
           main = "Random forest: Variable importance")

# now, let's make some predictions
rf.pred <- predict(rf.mod,
                   subset(spam.test,select = -type), 
                   type="class")

# confusion matrix
print(rf.pred.results <- table(rf.pred, spam.test$type))

# Accuracy of our RF model:
print(rf.accuracy <- sum(diag((rf.pred.results))) / sum(rf.pred.results))
