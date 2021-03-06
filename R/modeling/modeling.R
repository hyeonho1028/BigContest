set.seed(10)
# first train1 learning
modelFitRF <- train(audience ~ ., data = training, method = "rf")
modelFitGBM <- train(audience ~ ., data = training, method = "lmStepAIC")
modelFitBSTLM <- train(audience ~ ., data = training, method = "BstLm")
modelFitGLMBOOST <- train(audience ~ ., data = training, method = "xgbLinear")



predRF <- predict(modelFitRF,newdata=testing)
predGBM <- predict(modelFitGBM, newdata = testing)
prefBSTLM <- predict(modelFitBSTLM, newdata = testing)
prefGLMBOOST <- predict(modelFitGLMBOOST, newdata = testing)
predDF1 <- data.frame(predRF, predGBM, prefBSTLM, prefGLMBOOST, audience = testing$audience, stringsAsFactors = F)


testPredRF <- predict(modelFitRF, newdata = predict_dat)
testPredGBM <- predict(modelFitGBM, newdata = predict_dat)
testPredBSTLM <- predict(modelFitBSTLM, newdata = predict_dat)
testPredGLMBOOST <- predict(modelFitGLMBOOST, newdata = predict_dat)


testPredLevelOne <- data.frame(testPredRF, testPredGBM, testPredBSTLM, testPredGLMBOOST, 
                               audience = predict_dat$audience, stringsAsFactors = F)

colnames(testPredLevelOne) = c('predRF','predGBM', 'prefBSTLM','prefGLMBOOST', 'audience')

set.seed(12)
modelStack <- train(audience ~ ., data = predDF1, method = "rf")
combPred1 <- predict(modelStack, newdata = testPredLevelOne)
combPred1

modelStack <- train(audience ~ ., data = predDF1, method = "gamSpline")
combPred2 <- predict(modelStack, newdata = testPredLevelOne)
combPred2

modelStack <- train(audience ~ ., data = predDF1, method = "svmLinear")
combPred3 <- predict(modelStack, newdata = testPredLevelOne)
combPred3

apply(cbind(combPred1, combPred2, combPred3), 1, mean)
