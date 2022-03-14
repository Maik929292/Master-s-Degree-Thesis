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


#ALL
n <- nrow(df)
set.seed(1) 
training_index <- sample(n, round(n*0.8))

train_all <- df[training_index,]
test_all <- df[-training_index,]

table(df$transactionRevenue)

#NUMERIC

dfnum <- select(df,visitNumber, hits,pageviews,newVisits, dow, month, hours, transactionRevenue)

dfnum[-8] <- scale(dfnum[-8])

nnum <- nrow(dfnum)
set.seed(1) 
training_num <- sample(nnum, round(nnum*0.8))

train_num <- dfnum[training_num,]
test_num <- dfnum[-training_num,]

#MWMOTE
nI_tab <- table(train_num$transactionRevenue)
nI <- max(nI_tab)
df_lowclass <- mwmote(data = train_num, numInstances = nI, classAttr = 
                        "transactionRevenue")
train_mwmote <- rbind(df_lowclass, train_num[train_num$transactionRevenue == 
                                        names(nI_tab[which.max(nI_tab)]),])

table(train_mwmote$transactionRevenue)

#ROSE
train_rose <- ROSE(transactionRevenue ~ ., data = train_num,  seed=111)$data

table(train_rose$transactionRevenue)

#SMOTE

train_smote<- SMOTE(transactionRevenue ~ ., data = train_num, perc.over = 100, perc.under=200)

table(train_smote$transactionRevenue)


