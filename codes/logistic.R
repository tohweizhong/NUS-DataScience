# logistic regression model
log.mod <- glm(type ~ . , data = spam.train,
               family = binomial(link = logit))

# summary
summary(log.mod)

# predictions
log.pred.prob <- predict(log.mod,
                         subset(spam.test, select = -type), 
                         type = "response")
log.pred.class <- factor(sapply(log.pred.prob,
                                FUN = function(x){
                                    if(x >= 0.5) return("spam")
                                    else return("nonspam")
                                }))

# confusion matrix
log.pred.results <- table(log.pred.class, spam.test$type)

# Accuracy of logistic regression model:
print(log.accuracy <- sum(diag((log.pred.results))) / sum(log.pred.results))
