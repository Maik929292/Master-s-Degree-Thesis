library(imbalance)
library(caret)
library(PRROC)
library(caTools)
library(pROC)
library(DMwR)
library(ROSE)
library(tidyverse) 
library(dplyr)
library(lubridate)
library(class)

str(df) #Dataset df con variabili factor e numeric. 25.000 osservazioni con 14 variabili

####################ALL#################

#Predict logistic regression All

normal_class = glm(formula = transactionRevenue ~ ., family = binomial, data = train_all)

summary(normal_class)

normal_predict <-  predict(normal_class, type = 'response', newdata = test_all[,-10])

yprednumall <- ifelse(normal_predict > 0.5, 1,0)

ypred <- factor(yprednumall, levels = c(0,1))

Normalconf <- caret::confusionMatrix(ypred, test_all$transactionRevenue, positive = "1")

print(Normalconf)

roc_all <- roc.curve(test_all$transactionRevenue, normal_predict)

print(roc_all)

################################NUMERIC#############

num_class = glm(formula = transactionRevenue ~ ., family = binomial, data = train_num)

summary(num_class)

num_predict = predict(num_class, type = 'response', newdata = test_num[,-8])

yprednumnum <- ifelse(num_predict > 0.5, 1,0)

yprednum <- factor(yprednumnum, levels = c(0,1))

Numconf <- caret::confusionMatrix(yprednum, test_num$transactionRevenue, positive = "1")

roc_num <- roc.curve(test_num$transactionRevenue, num_predict)

print(roc_num)

#############################MWMOTE###############

#Logistic regression train_mwmote

mwmote_class = glm(formula = transactionRevenue ~ ., family = binomial, data = train_mwmote)

summary(mwmote_class)

mwmote_predict = predict(mwmote_class, type = 'response', newdata = test_num[-8])

ypredmwmotenum <- ifelse(mwmote_predict > 0.5, 1,0)

ypredmwmote <- factor(ypredmwmotenum, levels = c(0,1))

Mwmoteconf <- caret::confusionMatrix(ypredmwmote, test_num$transactionRevenue, positive = "1")

print(Mwmoteconf)

roc_mwmote <- roc.curve(test_num$transactionRevenue, mwmote_predict)

print(roc_mwmote)

###############################ROSE###############

# Logistic regression for rose

rose_classifier = glm(formula = transactionRevenue ~ ., family = binomial, data = train_rose)

summary(rose_classifier)

rose_predict = predict(rose_classifier, type = 'response', newdata = test_num[-8])

ypredrosenum <- ifelse(rose_predict > 0.5, 1,0)

ypredrose <- factor(ypredrosenum, levels = c(0,1))

roseconf <- caret::confusionMatrix(ypredrose, test_num$transactionRevenue, positive = "1")

print(roseconf)

roc_rose <- roc.curve(test_num$transactionRevenue, rose_predict)

print(roc_rose)

#SMOTE

# Logistic regression smote 

smote_classifier = glm(formula = transactionRevenue ~ ., family = binomial, data = train_smote)

summary(smote_classifier)

smote_predict = predict(smote_classifier, type = 'response', newdata = test_num[-8])

ypredsmotenum <- ifelse(smote_predict > 0.5, 1,0)

ypredsmote <- factor(ypredsmotenum, levels = c(0,1))

smoteconf <- caret::confusionMatrix(ypredsmote, test_num$transactionRevenue, positive = "1")

print(smoteconf)

roc_smote <- roc.curve(test_smote$transactionRevenue, smote_predict)

print(roc_smote)

##########################ROC Curves Logistic Regression#############

roc.log.all <- roc(test_all$transactionRevenue, normal_predict, legacy.axes = F, plot = TRUE, percent = TRUE,
                       xlab = "Specificità(%)", ylab = "Sensibilità (%)",
                       col = "blue", lwd = 3, asp =NA)

roc.log.num <- plot.roc(test_num$transactionRevenue, num_predict, percent = TRUE, col = "brown", 
                    print.auc = F, add = TRUE, lwd = 3)

roc.log.mwmote <- plot.roc(test_num$transactionRevenue, mwmote_predict, percent = TRUE, col = "darkgreen",
                            print.auc = F, add = TRUE, lwd = 3)

roc.log.rose <- plot.roc(test_num$transactionRevenue, rose_predict, percent = TRUE, col = "orange",
                          print.auc = F, add = TRUE, lwd = 3)

roc.log.smote <- plot.roc(test_num$transactionRevenue, smote_predict, percent = TRUE, col = "red",
                           print.auc = F, add = TRUE, lwd = 3)


legend("bottomright",c('Originale - AUC: 97.9%','Numerico - AUC: 96.3%',
               'Mwmote - AUC: 98.2%', 'Rose - AUC: 98.0 %','Smote - AUC: 98.1%'),
       col=c('blue','brown','darkgreen','orange','red'),lwd=3, cex = 1)
##################
