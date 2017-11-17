load("fingergrpint_MTBLS237.RData")
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


testing_mccv=matrix(NA,20,7)
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
testing_mccv[i,]=confusionMatrix(metadata[-inTraining],predict(pls.fit2,data.frame(metadata,scale(dataset))[-inTraining,]))$overall
testing_mccv[10+i,]=confusionMatrix(metadata[-inTraining],predict(pls.fit3,data.frame(metadata,scale(dataset))[-inTraining,]))$overall
}
accuracy_normal=testing_mccv[1:10,1]
accuracy_upsampling=testing_mccv[11:20,1]
kappa_normal=testing_mccv[1:10,2]
kappa_upsampling=testing_mccv[11:20,2]
boxplot(data.frame(accuracy_normal,accuracy_upsampling,kappa_normal,kappa_upsampling))
