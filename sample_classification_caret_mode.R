load("C:/Users/Usuario/Downloads/fingergrpint_MTBLS237.RData")

library(caret)

set.seed(1);inTraining <- as.vector(createDataPartition(metadata, p = .7, list = FALSE))
ctrl <- trainControl(method = "repeatedcv",
                                  number = 5,
                                  repeats = 3,
                                  classProbs = TRUE)
set.seed(1);pls.fit1=train(metadata ~ .,
           data = data.frame(metadata,dataset)[inTraining,],
           method = "pls",
           verbose = FALSE,
           trControl = ctrl)
set.seed(1);pls.fit2=train(metadata ~ .,
           data = data.frame(metadata,scale(dataset))[inTraining,],
           method = "pls",
           verbose = FALSE,
           trControl = ctrl)
ctrl$sampling='up'
set.seed(1);pls.fit3=train(metadata ~ .,
               data = data.frame(metadata,scale(dataset))[inTraining,],
               method = "pls",
               verbose = FALSE,
               trControl = ctrl)
confusionMatrix(metadata[-inTraining],predict(pls.fit2,data.frame(metadata,scale(dataset))[-inTraining,]))$overall
confusionMatrix(metadata[-inTraining],predict(pls.fit3,data.frame(metadata,scale(dataset))[-inTraining,]))$overall
rfe_tests=rep(NA,10)
for (i in 1:10) {
  set.seed(i);inTraining <- as.vector(createDataPartition(metadata, p = .7, list = FALSE))
  set.seed(1); rfe_tests[i]=length(rfe(dataset[inTraining,], metadata[inTraining], sizes=2^(1:10),rfeControl=rfeControl(functions=rfFuncs, method="cv", number=10))$optVariables)
  }

for (i in 1:10) {
  set.seed(i);inTraining <- as.vector(createDataPartition(metadata, p = .7, list = FALSE))


set.seed(1);rfe_results <- rfe(dataset[inTraining,], metadata[inTraining], sizes=2^(1:10),rfeControl=rfeControl(functions=rfFuncs, method="cv", number=10))
set.seed(1);pls.fit4=train(metadata ~ .,
           data = data.frame(metadata,scale(dataset[,ind]))[inTraining,],
           method = "pls",
           verbose = FALSE,
           trControl = ctrl)





rf.fit1=train(metadata ~ .,
               data = data.frame(metadata,scale(dataset))[inTraining,],
               method = "rf",
               verbose = FALSE,
               trControl = ctrl)
rf.fit2=train(metadata ~ .,
              data = data.frame(metadata,scale(dataset[,ind]))[inTraining,],
              method = "rf",
              verbose = FALSE,
              trControl = ctrl)
gbm.fit1=train(metadata ~ .,
              data = data.frame(metadata,scale(dataset))[inTraining,],
              method = "gbm",
              verbose = FALSE,
              trControl = ctrl)
gbm.fit2=train(metadata ~ .,
              data = data.frame(metadata,scale(dataset[,ind]))[inTraining,],
              method = "gbm",
              verbose = FALSE,
              trControl = ctrl)
models=list(pls.fit,pls.fit2,pls.fit3.pls.fit4,rf.fit1,rf.fit2,gbm.fit1,gbm.fit2)
c(confusionMatrix(metadata[-inTraining],predict(pls.fit1,data.frame(metadata,dataset)[-inTraining,]))$overall[2],
      confusionMatrix(metadata[-inTraining],predict(pls.fit2,data.frame(metadata,scale(dataset))[-inTraining,]))$overall[2],
      confusionMatrix(metadata[-inTraining],predict(pls.fit3,data.frame(metadata,scale(dataset))[-inTraining,]))$overall[2],
      confusionMatrix(metadata[-inTraining],predict(pls.fit4,data.frame(metadata,scale(dataset[,ind]))[-inTraining,]))$overall[2],
      confusionMatrix(metadata[-inTraining],predict(rf.fit1,data.frame(metadata,scale(dataset))[-inTraining,]))$overall[2],
      confusionMatrix(metadata[-inTraining],predict(rf.fit2,data.frame(metadata,scale(dataset[,ind]))[-inTraining,]))$overall[2],
      confusionMatrix(metadata[-inTraining],predict(gbm.fit1,data.frame(metadata,scale(dataset))[-inTraining,]))$overall[2],
      confusionMatrix(metadata[-inTraining],predict(gbm.fit2,data.frame(metadata,scale(dataset[,ind]))[-inTraining,]))$overall[2])



validation_tests=matrix(NA,50,7)
for (i in 1:10) {
set.seed(i);inTraining <- as.vector(createDataPartition(metadata, p = .7, list = FALSE))
ctrl$sampling=NULL
set.seed(i);pls.fit2=train(metadata ~ .,
                           data = data.frame(metadata,scale(dataset))[inTraining,],
                           method = "pls",
                           verbose = FALSE,
                           trControl = ctrl)
ctrl$sampling='up'
set.seed(i);pls.fit3=train(metadata ~ .,
                           data = data.frame(metadata,scale(dataset))[inTraining,],
                           method = "pls",
                           verbose = FALSE,
                           trControl = ctrl)
set.seed(i); rfe_results=rfe(dataset[inTraining,], metadata[inTraining], sizes=2^(1:10),rfeControl=rfeControl(functions=rfFuncs, method="cv", number=10))
ind=colnames(dataset) %in% rfe_results$optVariables
set.seed(i);pls.fit4=train(metadata ~ .,
                           data = data.frame(metadata,scale(dataset[,ind]))[inTraining,],
                           method = "pls",
                           verbose = FALSE,
                           trControl = ctrl)
set.seed(i);rf.fit1=train(metadata ~ .,
              data = data.frame(metadata,scale(dataset[,ind]))[inTraining,],
              method = "rf",
              verbose = FALSE,
              trControl = ctrl)
set.seed(i);gbm.fit1=train(metadata ~ .,
               data = data.frame(metadata,scale(dataset[,ind]))[inTraining,],
               method = "gbm",
               verbose = FALSE,
               trControl = ctrl)
validation_tests[i,]=confusionMatrix(metadata[-inTraining],predict(pls.fit2,data.frame(metadata,scale(dataset))[-inTraining,]))$overall
validation_tests[10+i,]=confusionMatrix(metadata[-inTraining],predict(pls.fit3,data.frame(metadata,scale(dataset))[-inTraining,]))$overall
validation_tests[20+i,]=confusionMatrix(metadata[-inTraining],predict(pls.fit4,data.frame(metadata,scale(dataset[,ind]))[-inTraining,]))$overall
validation_tests[30+i,]=confusionMatrix(metadata[-inTraining],predict(rf.fit1,data.frame(metadata,scale(dataset[,ind]))[-inTraining,]))$overall
validation_tests[40+i,]=confusionMatrix(metadata[-inTraining],predict(gbm.fit1,data.frame(metadata,scale(dataset[,ind]))[-inTraining,]))$overall

}
accuracy_normal=validation_tests[1:10,1]
accuracy_upsampling=validation_tests[11:20,1]
kappa_normal=validation_tests[1:10,2]
kappa_upsampling=validation_tests[11:20,2]
boxplot(data.frame(accuracy_normal,accuracy_upsampling,kappa_normal,kappa_upsampling))
