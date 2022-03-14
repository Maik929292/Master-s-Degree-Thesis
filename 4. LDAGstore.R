library(MASS)

lda_all = lda(transactionRevenue ~., data = train_all)

summary(lda_all)

lda.pred.all <- predict(lda_all, newdata = test_all)

lda.class.all = lda.pred.all$class

table(lda.class.all, test_all[,10])

NormLDAconf <- caret::confusionMatrix(lda.class.all, test_all[,10], positive = "1")

roc_lda.all <- roc.curve(test_all$transactionRevenue, lda.pred.all$posterior[,2])

print(roc_lda.all)

####################

lda_num = lda(transactionRevenue ~ ., data = train_num)

summary(lda_num)

lda.pred.num <- predict(lda_num, test_num)

lda.class.num = lda.pred.num$class

table(lda.class.num, test_num[,8])

NumLDAconf <- caret::confusionMatrix(lda.class.num, test_num[,8], positive = "1")

roc_lda.num <- roc.curve(test_num$transactionRevenue, lda.pred.num$posterior[,2])

print(roc_lda.num)

##########################

lda_mwmote = lda(transactionRevenue ~ ., data = train_mwmote)

summary(lda_mwmote)

lda.pred.mwmote <- predict(lda_mwmote, test_num)

lda.class.mwmote = lda.pred.mwmote$class

table(lda.class.mwmote, test_num[,8])

MwmoteLDAconf <- caret::confusionMatrix(lda.class.mwmote, test_num[,8], positive = "1")

roc_lda.mwmote <- roc.curve(test_num$transactionRevenue, lda.pred.mwmote$posterior[,2])

print(roc_lda.mwmote)

#################

lda_rose = lda(transactionRevenue ~ ., data = train_rose)

summary(lda_rose)

lda.pred.rose <- predict(lda_rose, test_num)

lda.class.rose = lda.pred.rose$class

table(lda.class.rose, test_num[,8])

RoseLDAconf <- caret::confusionMatrix(lda.class.rose, test_num[,8], positive = "1")

roc_lda.rose <- roc.curve(test_num$transactionRevenue, lda.pred.rose$posterior[,2])

print(roc_lda.rose)

############################

lda_smote = lda(transactionRevenue ~ ., data = train_smote)

summary(lda_smote)

lda.pred.smote <- predict(lda_smote, test_num)

lda.class.smote = lda.pred.smote$class

table(test_num[,8], lda.class.smote)

SmoteLDAconf <- caret::confusionMatrix(lda.class.smote,test_num[,8], positive = "1")

roc_lda.smote <- roc.curve(test_num$transactionRevenue, lda.pred.smote$posterior[,2])

print(roc_lda.smote)

###################ROC Curves LDA##############


tiff('ROCLDA.tiff', units="in", width=10, height=8, res=300)


roc_lda.all <- roc(test_all$transactionRevenue, lda.pred.all$posterior[,2], legacy.axes = F, plot = TRUE, percent = TRUE,
                   xlab = "Specificità (%)", ylab = "Sensibilità (%)",
                   col = "blue", lwd = 3, asp =NA)

roc_lda.num <- plot.roc(test_num$transactionRevenue, lda.pred.num$posterior[,2], percent = TRUE, col = "brown", 
                        print.auc = F, add = TRUE, lwd = 3)

roc_lda.mwmote <- plot.roc(test_num$transactionRevenue, lda.pred.mwmote$posterior[,2], percent = TRUE, col = "darkgreen",
                           print.auc = F, add = TRUE, lwd = 3)

roc_lda.rose <- plot.roc(test_num$transactionRevenue, lda.pred.rose$posterior[,2], percent = TRUE, col = "orange",
                         print.auc = F, add = TRUE, lwd = 3)

roc_lda.smote <- plot.roc(test_num$transactionRevenue, lda.pred.smote$posterior[,2], percent = TRUE, col = "red",
                          print.auc = F, add = TRUE, lwd = 3)



legend("bottomright",c('Originale - AUC: 98.47%','Numerico - AUC: 98.32%',
                       'Mwmote - AUC: 97.23%', 'Rose - AUC: 95.56 %','Smote - AUC: 96.31%'),
       col=c('blue','brown','darkgreen','orange','red'),lwd=3, cex = 1)



