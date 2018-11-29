library(mice)
library(dplyr)
library(tidyverse)
library(pROC)
library(arm)

###Load data and data pre-processing
rm(list = ls())
setwd("/Users/Macintosh/Kaggle-HomeCredit/HomeCredit-data")
application_train <- read.csv("application_train.csv")
application_train <- application_train[1:5000,]

#select interested variables
application_train_selected <- subset(application_train, select = c("TARGET", "CODE_GENDER", "NAME_CONTRACT_TYPE", 
                                                                   "AMT_INCOME_TOTAL", "FLAG_OWN_CAR", "FLAG_OWN_REALTY","CNT_CHILDREN",
                                                                   "AMT_CREDIT", "AMT_ANNUITY", "AMT_GOODS_PRICE", "NAME_TYPE_SUITE",
                                                                   "NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS",
                                                                   "NAME_HOUSING_TYPE","REGION_POPULATION_RELATIVE","DAYS_BIRTH", 
                                                                   "DAYS_EMPLOYED","OWN_CAR_AGE","OCCUPATION_TYPE","CNT_FAM_MEMBERS",
                                                                   "ORGANIZATION_TYPE",
                                                                   "AMT_REQ_CREDIT_BUREAU_DAY", "AMT_REQ_CREDIT_BUREAU_WEEK","AMT_REQ_CREDIT_BUREAU_MON",
                                                                   "AMT_REQ_CREDIT_BUREAU_QRT","AMT_REQ_CREDIT_BUREAU_YEAR"))
#combine all the AMT_REQ_CREDIT_BUREAU variables together to compute the aggregated
#number of enquiries to Credit Bureau about the client one year before the application
application_train_selected$AMR_REQ_CREDIT_BUREAU_SUM <- application_train_selected$AMT_REQ_CREDIT_BUREAU_DAY+
                                                        application_train_selected$AMT_REQ_CREDIT_BUREAU_MON+
                                                        application_train_selected$AMT_REQ_CREDIT_BUREAU_QRT+
                                                        application_train_selected$AMT_REQ_CREDIT_BUREAU_WEEK+
                                                        application_train_selected$AMT_REQ_CREDIT_BUREAU_YEAR
#Drop AMT_REQ_CREDIT_BUREAU_DAY, WEEK, MON, QRT, YEAR
application_train_selected <- application_train_selected %>%
    select(-(AMT_REQ_CREDIT_BUREAU_DAY)) %>%
    select(-(AMT_REQ_CREDIT_BUREAU_WEEK)) %>%
    select(-(AMT_REQ_CREDIT_BUREAU_MON)) %>%
    select(-(AMT_REQ_CREDIT_BUREAU_QRT)) %>%
    select(-(AMT_REQ_CREDIT_BUREAU_YEAR))

#factorize CNT_FAM_MEMBERS, AMR_REQ_CREDIT_BUREAU_SUM
application_train_selected$CNT_FAM_MEMBERS <- as.factor(application_train_selected$CNT_FAM_MEMBERS)
application_train_selected$AMR_REQ_CREDIT_BUREAU_SUM <- as.factor(application_train_selected$AMR_REQ_CREDIT_BUREAU_SUM)


#check collinearity
num <- unlist(lapply(application_train_selected, is.numeric))
cor(cc(application_train_selected[,num]), use = "pair")
#CNT_FAM_MEMBERS and CNT_CHILDREN are highly correlated at 0.91
#AMT_CREDIT and AMT_GOODPRICE are highly correlated at 0.98

#DROP CNT_CHILDREN and AMT_GOODPRICE
application_train_selected <- application_train_selected %>%
    select(-(AMT_GOODS_PRICE)) %>%
    select(-(CNT_CHILDREN))
#Factorize categorical variables : all categorical variables are factorized

###Imputation implementation
#check pattern
md.pattern(application_train_selected)
#imputation implementation
#cart: I have tried to use the default method to impute missing values; however, it returned the following error
#"system is computationally singular"
#The cause of the problem here could probably be the large number of unbalanced factor variables in the dataset.
#When these are turned intodummy variables there's a high probability that one colum is a linear combination of another. 
#As the default imputation methods involve linear regression, this results in a X matrix that cannot be inverted 
#Therefore, we consider to change the imputation method that is not stochastic, which require no X matrix inversion.
application_MI <- mice(application_train_selected, m = 5, method = "cart", seed = 8)
#imputation diagnostics
stripplot(application_MI, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(application_MI, OWN_CAR_AGE~TARGET, col=c("grey",mdc(2)),pch=c(1,20), xlab = 'TARGET', ylab = "OWN_CAR_AGE")
stripplot(application_MI, AMR_REQ_CREDIT_BUREAU_SUM~TARGET, col=c("grey",mdc(2)),pch=c(1,20), xlab = 'TARGET', ylab = "AMT_REQ_CREDIT_BUREAU_DAY")

#Posterior Predictive Check on two complete datasets
application_ppcheck <- rbind(application_train_selected, application_train_selected)
application_ppcheck[5001:10000, apply(is.na(application_train_selected), any, MARGIN = 2)] <- NA
application_ppcheck_MI <- mice(application_ppcheck, m = 5, method = "cart", seed = 8)
d1ppcheck <- mice::complete(application_ppcheck_MI, 1)
d2ppcheck <- mice::complete(application_ppcheck_MI, 2)
#dataset1
par(mfrow = c(1,2))
boxplot(d1ppcheck$OWN_CAR_AGE[1:5000]~d1ppcheck$TARGET[1:5000], ylab="OWN_CAR_AGE", xlab="TARGET", main = "OWN_CAR_AGE vs TARGET completed data")
boxplot(d1ppcheck$OWN_CAR_AGE[5001:10000]~d1ppcheck$TARGET[5001:10000], ylab="OWN_CAR_AGE", xlab="TARGET", main = "OWN_CAR_AGE vs TARGET completed data")

par(mfrow = c(1,2))
boxplot(d1ppcheck$AMR_REQ_CREDIT_BUREAU_SUM[1:5000]~d1ppcheck$TARGET[1:5000], ylab="AMR_REQ_CREDIT_BUREAU_SUM", xlab="TARGET", main = "AMR_REQ_CREDIT_BUREAU_SUM vs TARGET completed data")
boxplot(d1ppcheck$AMR_REQ_CREDIT_BUREAU_SUM[5001:10000]~d1ppcheck$TARGET[5001:10000], ylab="AMR_REQ_CREDIT_BUREAU_SUM", xlab="TARGET", main = "AMR_REQ_CREDIT_BUREAU_SUM vs TARGET completed data")

###Regression Model
reg <- with(data = application_MI, glm(TARGET ~ CODE_GENDER + NAME_CONTRACT_TYPE + AMT_INCOME_TOTAL + FLAG_OWN_CAR + FLAG_OWN_REALTY + AMT_CREDIT + AMT_ANNUITY
            + NAME_TYPE_SUITE + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE 
            + REGION_POPULATION_RELATIVE + DAYS_BIRTH + DAYS_EMPLOYED + OWN_CAR_AGE + OCCUPATION_TYPE + CNT_FAM_MEMBERS 
            + ORGANIZATION_TYPE + AMR_REQ_CREDIT_BUREAU_SUM),
            family = binomial)
summary(pool(reg))

##Use completed datasets to see AUCs for the fitted models
#Dataset 1
par(mfrow=c(1,1))
cd1 <- mice::complete(application_MI, 1)
reg_cd1 <- glm(data=cd1, TARGET~CODE_GENDER + NAME_CONTRACT_TYPE + AMT_INCOME_TOTAL + FLAG_OWN_CAR + FLAG_OWN_REALTY + AMT_CREDIT + AMT_ANNUITY
               + NAME_TYPE_SUITE + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE 
               + REGION_POPULATION_RELATIVE + DAYS_BIRTH + DAYS_EMPLOYED + OWN_CAR_AGE + OCCUPATION_TYPE + CNT_FAM_MEMBERS 
               + ORGANIZATION_TYPE + AMR_REQ_CREDIT_BUREAU_SUM, 
               family=binomial)
roc(cd1$TARGET, fitted(reg_cd1), plot=T, legacy.axes=T)

#Dataset 2
cd2 <- mice::complete(application_MI, 2)
reg_cd2 <- glm(data=cd2, TARGET~CODE_GENDER + NAME_CONTRACT_TYPE + AMT_INCOME_TOTAL + FLAG_OWN_CAR + FLAG_OWN_REALTY + AMT_CREDIT + AMT_ANNUITY
              + NAME_TYPE_SUITE + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE 
              + REGION_POPULATION_RELATIVE + DAYS_BIRTH + DAYS_EMPLOYED + OWN_CAR_AGE + OCCUPATION_TYPE + CNT_FAM_MEMBERS 
              + ORGANIZATION_TYPE + AMR_REQ_CREDIT_BUREAU_SUM, 
              family=binomial)
roc(cd2$TARGET, fitted(reg_cd2), plot=T, legacy.axes=T)

#Dataset 3
cd3 <- mice::complete(application_MI, 3)
reg_cd3 <- glm(data=cd3, TARGET~CODE_GENDER + NAME_CONTRACT_TYPE + AMT_INCOME_TOTAL + FLAG_OWN_CAR + FLAG_OWN_REALTY + AMT_CREDIT + AMT_ANNUITY
               + NAME_TYPE_SUITE + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE 
               + REGION_POPULATION_RELATIVE + DAYS_BIRTH + DAYS_EMPLOYED + OWN_CAR_AGE + OCCUPATION_TYPE + CNT_FAM_MEMBERS 
               + ORGANIZATION_TYPE + AMR_REQ_CREDIT_BUREAU_SUM, 
               family=binomial)
roc(cd3$TARGET, fitted(reg_cd3), plot=T, legacy.axes=T)

###Model Diagnostics
#Dataset 1
cd1 <- mice::complete(application_MI, 1)
#Binned residual plots
rawresid1 = cd1$TARGET - fitted(reg_cd1)
#continuous variables
binnedplot(x=cd1$AMT_INCOME_TOTAL, y = rawresid1, xlab = "AMT_INCOME_TOTAL", ylab = "Residuals", main = "Binned residuals versus AMT_INCOME_TOTAL")
binnedplot(x=cd1$AMT_CREDIT, y = rawresid1, xlab = "AMT_CREDIT", ylab = "Residuals", main = "Binned residuals versus AMT_CREDIT")
binnedplot(x=cd1$AMT_ANNUITY, y = rawresid1, xlab = "AMT_ANNUITY", ylab = "Residuals", main = "Binned residuals versus AMT_ANNUITY")
binnedplot(x=cd1$REGION_POPULATION_RELATIVE , y = rawresid1, xlab = "REGION_POPULATION_RELATIVE", ylab = "Residuals", main = "Binned residuals versus REGION_POPULATION_RELATIVE")
binnedplot(x=cd1$DAYS_BIRTH , y = rawresid1, xlab = "DAYS_BIRTH", ylab = "Residuals", main = "Binned residuals versus DAYS_BIRTH")
binnedplot(x=cd1$DAYS_EMPLOYED , y = rawresid1, xlab = "DAYS_EMPLOYED", ylab = "Residuals", main = "Binned residuals versus DAYS_EMPLOYED")
binnedplot(x=cd1$OWN_CAR_AGE , y = rawresid1, xlab = "OWN_CAR_AGE", ylab = "Residuals", main = "Binned residuals versus OWN_CAR_AGE")
binnedplot(x=cd1$AMR_REQ_CREDIT_BUREAU_SUM , y = rawresid1, xlab = "OAMR_REQ_CREDIT_BUREAU_SUM", ylab = "Residuals", main = "Binned residuals versus AMR_REQ_CREDIT_BUREAU_SUM")





####DEMO
complete_data <- cc(application_train_selected)
summary(complete_data)
demo_reg <- glm(TARGET ~ CODE_GENDER + NAME_CONTRACT_TYPE + AMT_INCOME_TOTAL + FLAG_OWN_REALTY + AMT_CREDIT + AMT_ANNUITY
                + NAME_TYPE_SUITE + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE 
                + REGION_POPULATION_RELATIVE + DAYS_BIRTH + DAYS_EMPLOYED + OWN_CAR_AGE + OCCUPATION_TYPE + CNT_FAM_MEMBERS 
                + ORGANIZATION_TYPE + AMR_REQ_CREDIT_BUREAU_SUM,
                data = application_train_selected,
                family = binomial)
summary(demo_reg)
roc(complete_data$TARGET, fitted(demo_reg), plot = T, legacy.axes = T)
 
