library(e1071)

svm.all <- svm(transactionRevenue ~ .,data=train_all, cost = 10, gamma = 0.01, decision.values = T)

summary(svm.all)

svm.pred.all <- predict(svm.all,test_all[,-10])

Svmconfall<- caret::confusionMatrix(svm.pred.all, test_all$transactionRevenue, positive = "1")

fitted.all <-as.numeric(attributes(predict(svm.all,test_all,decision.values = TRUE))$decision.values)

roc_svm.all <- roc.curve(test_all$transactionRevenue, fitted.all)

print(roc_svm.all)

#tune.svm.all <- tune.svm(transactionRevenue~., data=train_all,kernel='radial',cost= 10^(-3:1), gamma=10^(-3:1))

svm.num <- svm(transactionRevenue ~ .,data=train_num, cost = 10, gamma = 0.01, decision.values = T)

summary(svm.num)

svm.pred.num <- predict(svm.num,test_num[,-8])

Svmconfnum<- caret::confusionMatrix(svm.pred.num, test_num$transactionRevenue,  positive = "1")

fitted.num <-as.numeric(attributes(predict(svm.num,test_num,decision.values = TRUE))$decision.values)

roc_svm.num <- roc.curve(test_num$transactionRevenue, fitted.num)

print(roc_svm.num)

#################################################

svm.mwmote <- svm(transactionRevenue ~ .,data=train_mwmote, cost = 10, gamma = 0.01, decision.values = T)

summary(svm.mwmote)

svm.pred.mwmote <- predict(svm.mwmote,test_num[,-8])

Svmconfmwmote <- caret::confusionMatrix(svm.pred.mwmote, test_num$transactionRevenue, positive = "1")

fitted.mwmote <- as.numeric(attributes(predict(svm.mwmote,test_num,decision.values = TRUE))$decision.values)

roc_svm.mwmote <- roc.curve(test_num$transactionRevenue, fitted.mwmote)

print(roc_svm.mwmote)

##########################

svm.rose <- svm(transactionRevenue ~ .,data=train_rose, cost = 10, gamma = 0.01, decision.values = T)

summary(svm.rose)

svm.pred.rose <- predict(svm.rose,test_num[,-8])

Svmconfrose <- caret::confusionMatrix(svm.pred.rose, test_num$transactionRevenue, positive = "1")

fitted.rose <- as.numeric(attributes(predict(svm.rose,test_num,decision.values = TRUE))$decision.values)

roc_svm.rose <- roc.curve(test_num$transactionRevenue, fitted.rose)

print(roc_svm.rose)


########################

svm.smote <- svm(transactionRevenue ~ .,data=train_smote, cost = 10, gamma = 0.01, decision.values = T)

summary(svm.smote)

svm.pred.smote <- predict(svm.smote,test_num[,-8])

Svmconfsmote <- confusionMatrix(svm.pred.smote, test_num$transactionRevenue, positive = "1")

fitted.smote <- as.numeric(attributes(predict(svm.smote,test_num,decision.values = TRUE))$decision.values)

roc_svm.smote <- roc.curve(test_num$transactionRevenue, fitted.smote)

print(roc_svm.smote)

#####################


svm.info.normal <- roc(test_all$transactionRevenue, fitted.all, legacy.axes = F, plot = TRUE, percent = TRUE,
                       xlab = "Specificità (%)", ylab = "Sensibilità (%)",
                       col = "blue", lwd = 3, asp =NA)

svm.info.num <- plot.roc(test_num$transactionRevenue, fitted.num, percent = TRUE, col = "brown", 
                         print.auc = F, add = TRUE, lwd = 3)

svm.info.mwmote <- plot.roc(test_num$transactionRevenue, fitted.mwmote, percent = TRUE, col = "darkgreen",
                            print.auc = F, add = TRUE, lwd = 3)

svm.info.rose <- roc(test_num$transactionRevenue, fitted.rose, percent = TRUE, col = "orange",
                          print.auc = F, add = TRUE, lwd = 3)

svm.info.smote <- plot.roc(test_num$transactionRevenue, fitted.smote, percent = TRUE, col = "red",
                           print.auc = F, add = TRUE, lwd = 3)


legend("bottomright",c('Originale - AUC: 94.06%','Numerico - AUC: 92.68%',
                       'Mwmote - AUC: 95.47%', 'Rose - AUC: 98.07 %','Smote - AUC: 97.60%'),
       col=c('blue','brown','darkgreen','orange','red'),lwd=3, cex = 1)


