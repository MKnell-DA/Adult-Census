##########################################################
# Capstone - Adult Census: R-Script (Martin Knell)
##########################################################


##########################################################
# Load Libraries
##########################################################

if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(rockchalk)) install.packages("rockchalk", repos = "http://cran.us.r-project.org")
if(!require(varImp)) install.packages("varImp", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")


library(dslabs)
library(tidyverse)
library(caret)
library(ggplot2)
library(readr)
library(rockchalk)
library(varImp)
library(gbm)



##########################################################
# Load data set
##########################################################

# Note: Please download GitHub repo and use as working directory
# https://github.com/MKnell-DA/Adult-Census

adult <- read.csv("./adult.csv")


##########################################################
# Initial data set evaluation and cleaning
##########################################################

  # Raw structure and summary

head(adult)
summary(adult)

  # Replace "?" with NA, omit NAs and check for lost rows

adult[adult == "?"] <- NA
nrow(adult)

adult <- na.omit(adult)
nrow(adult)

  # convert factor variables into factors

adult$workclass <- factor(adult$workclass)
adult$marital.status <- factor(adult$marital.status)
adult$occupation <- factor(adult$occupation)
adult$relationship <- factor(adult$relationship)
adult$race <- factor(adult$race)
adult$native.country <- factor(adult$native.country)
adult$sex <- factor(adult$sex)
adult$education <- factor(adult$education)

   # Change income invormation to 1 for >50k and 0 for <=50k

adult$income <- as.numeric(ifelse(adult$income == ">50K", 1, 0))

  # discard fnlwgt column

census <- adult %>% select(-fnlwgt) 

   # summary of cleaned data set

nrow(census)
summary(census)


##########################################################
# Data Exploration
##########################################################

  # Income distribution by age

age_plot <- census %>% ggplot(aes(x = as.factor(income), y= age)) + geom_boxplot() +
  theme_classic(base_size = 15) + xlab("income bracket") + 
  ggtitle("age distribution by income brackets")
age_plot

median(census$age[census$income == 0])
median(census$age[census$income == 1])


  # Income distribution by work class

work_plot <- census %>% ggplot(aes(x = workclass, fill = as.factor(income))) + 
  geom_bar(position = "fill") + theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("income distribution") + 
  ggtitle("income distribution by work class") + labs(fill = "income bracket") 
work_plot


  # Income distribution by years of education

ed_grade_plot <- census %>% ggplot(aes(x = as.factor(education.num), fill = as.factor(income))) + 
  geom_bar(position = "fill") + theme_classic(base_size = 15) + ylab("income distribution") + 
  xlab("years of education") + ggtitle("income distribution by years of education") + 
  labs(fill = "income bracket") 
ed_grade_plot


  # Income level by education level

ed_plot <- census %>% ggplot(aes(x = education, fill = as.factor(income))) + 
  geom_bar(position = "fill") + theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("income distribution") + 
  ggtitle("income distribution by education level") + labs(fill = "income bracket") 
ed_plot

  # Based on observations group school degrees into one bucket and re-plot

census$education <- combineLevels(census$education, 
  levs = c("10th","11th","12th","1st-4th","5th-6th","7th-8th","9th","Preschool"), "School")

ed_plot <- census %>% ggplot(aes(x = education, fill = as.factor(income))) + 
  geom_bar(position = "fill") + theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("income distribution") + 
  ggtitle("income bracket by education level") + labs(fill = "income bracket") 
ed_plot

   # Income distribution by marital status

marital_plot <- census %>% ggplot(aes(x = marital.status, fill = as.factor(income))) + 
  geom_bar(position = "fill") + theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("income distribution") + 
  xlab("marital status") + ggtitle("income distriubtion by marital status") + 
  labs(fill = "income bracket") 
marital_plot


  # Income distribution by relationship status

relation_plot <- census %>% ggplot(aes(x = relationship, fill = as.factor(income))) + 
  geom_bar(position = "fill") + theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("income distribution") + 
  xlab("relationship status") + ggtitle("income distribution by relationship status") + 
  labs(fill = "income bracket") 
relation_plot

  # Group into Married and Unmarried

census$relationship <- combineLevels(census$relationship, 
  levs = c("Husband","Wife"), "Married")

census$relationship <- combineLevels(census$relationship, 
  levs = c("Not-in-family","Other-relative","Own-child","Unmarried"), "Unmarried")

relation_plot <- census %>% ggplot(aes(x = relationship, fill = as.factor(income))) + 
  geom_bar(position = "fill") + theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("income distribution") + 
  xlab("relationship status") + ggtitle("income distribution by relationship status") + 
  labs(fill = "income bracket") 
relation_plot


  # Income distribution by occupation

occ_plot <- census %>% ggplot(aes(x = occupation, fill = as.factor(income))) + 
  geom_bar(position = "fill") + theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("income distribution") + 
  xlab("occupation") + ggtitle("income distribution by occupation") + 
  labs(fill = "income bracket") 
occ_plot


  # Income distribution by race

race_plot <- census %>% ggplot(aes(x = race, fill = as.factor(income))) + 
  geom_bar(position = "fill") + theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("income distribution") + 
  xlab("race") + ggtitle("income bracket by race") + 
  labs(fill = "income bracket") 
race_plot


  # Income distribution by gender

gender_plot <- census %>% ggplot(aes(x = sex, fill = as.factor(income))) + 
  geom_bar(position = "fill") + theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("income distribution") + 
  xlab("gender") + ggtitle("income bracket by gender") + 
  labs(fill = "income bracket") 
gender_plot


  # Income distribution by work hours

hours_plot <- census %>% ggplot(aes(x = as.factor(income), y= hours.per.week)) + 
  geom_boxplot() + theme_classic(base_size = 15) + xlab("income bracket") + 
  ylab("work hours per week") +
  ggtitle("income bracket by work hours per week")
hours_plot

median(census$hours.per.week[census$income == 0])
median(census$hours.per.week[census$income == 1])


   # Income bracket by capital gain

gain_plot <- census %>% ggplot(aes(x = as.factor(income), y= capital.gain)) + 
  geom_boxplot() + theme_classic(base_size = 15) + xlab("income bracket") + 
  ylab("capital gain") + ggtitle("income bracket by capital gain")
gain_plot


  # Income bracket by capital loss

loss_plot <- census %>% ggplot(aes(x = as.factor(income), y= capital.loss)) + 
  geom_boxplot() + theme_classic(base_size = 15) + xlab("income bracket") + 
  ylab("capital loss") + ggtitle("income bracket by capital loss")
loss_plot


   # Income bracket by country of birth

country_plot <- census %>% ggplot(aes(x = native.country, fill = as.factor(income))) + 
  geom_bar(position = "fill") + theme_classic(base_size = 10) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("income distribution") + 
  xlab("country of birth") + ggtitle("income distribution by country of birth") + 
  labs(fill = "income bracket") 
country_plot


   # Based on observations, disregard marital status, work hours, capital gain and capital loss

census <- census %>% select(-hours.per.week, - capital.gain, - capital.loss, - marital.status)
census


##########################################################
# Modeling Preparation
##########################################################

  # Converting income class into factor

census$income = as.factor(census$income)


  # Creating train, test and validation set

validation_index <- createDataPartition(census$income, times = 1, p = 0.2, list = FALSE)
validation <- census[validation_index,]
remain <- census[-validation_index,]

test_index <- createDataPartition(remain$income, times = 1, p = 0.25, list = FALSE)
test <- remain[test_index,]
train <- remain[-test_index,]

nrow(train)
nrow(test)
nrow(validation)


##########################################################
# Linear Discriminant Analysis
##########################################################

lda.fit = train(income ~ ., data=train, method="lda", trControl = trainControl(method = "cv"))

forecast <- predict(lda.fit, test)

acc_lda <- mean(forecast == test$income)

accuracy <- data_frame(method = "LDA", Accuracy = acc_lda)
accuracy %>% knitr::kable()


##########################################################
# K nearest neighbors
##########################################################

knn.fit = train(income ~ ., data = train, method = "knn", 
                tuneGrid = data.frame(k = seq(3,51,2)), 
                trControl = trainControl(method="repeatedcv",number = 10, repeats = 3))

forecast <- predict(knn.fit, test)

acc_knn <- mean(forecast == test$income)

plot(knn.fit)
knn.fit$bestTune

accuracy <- bind_rows(accuracy, data_frame(method = "Knn", Accuracy = acc_knn))
accuracy %>% knitr::kable()


##########################################################
# Random Forest
##########################################################

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search= "grid")
rfGrid <- expand.grid(.mtry = c(1:10))

rf.fit <- train(income~., data = train, method = "rf", trControl = fitControl, tuneGrid = rfGrid)

forecast <- predict(rf.fit, test)

acc_rf <- mean(forecast == test$income)

plot(rf.fit)
rf.fit$bestTune

rfImp <- caret::varImp(rf.fit)
plot(rfImp, top = 10)

accuracy <- bind_rows(accuracy, data_frame(method = "R.Forest", Accuracy = acc_rf))
accuracy %>% knitr::kable()


##########################################################
# Gradient Boosting Machines
##########################################################

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
gbmGrid <-  expand.grid(interaction.depth = seq(3,11,2), n.trees = seq(500,3000,500), 
                        shrinkage = c(0.1, 0.01), n.minobsinnode = 10)

gbm.fit <- train(income ~ ., data = train, method = "gbm", 
                 trControl = fitControl, tuneGrid = gbmGrid)

forecast <- predict(gbm.fit, test)

acc_gbm <- mean(forecast == test$income)

plot(gbm.fit)
gbm.fit$bestTune

gbmImp <- caret::varImp(gbm.fit)
plot(gbmImp, top = 10)

accuracy <- bind_rows(accuracy, data_frame(method = "Gbm", Accuracy = acc_gbm))
accuracy %>% knitr::kable()


##########################################################
# Validation of Final Model
##########################################################

forecast <- predict(gbm.fit, validation)

validation_final <- mean(forecast == validation$income)

accuracy <- data_frame(method = "Final Model", Accuracy = validation_final)
accuracy %>% knitr::kable()