library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(gridExtra)
library(dslabs)

#Read the data file from folder
#download data files Bachelordataclean.xlsx and Elimdistance.xlsx

Bachelordataclean <- read_xlsx("./Bachelordataclean.xlsx")

#this is not a data frame but incorporates a tibble

Bachelordataclean2 <- as.data.frame(Bachelordataclean)
#take a look at the elimination weeks

histogram(Bachelordataclean2$ElimWeek)
summary(Bachelordataclean2)

histogram(Bachelordataclean2$ElimWeek)

plot(Bachelordataclean2$ElimWeek, Bachelordataclean2$Distance)
plot(Bachelordataclean2$ElimWeek, Bachelordataclean2$Age)
Elimdistance <- read_excel('./Elimdistance.xlsx')

#split into training and test sets with a 70/30 split
#first we create an 'id' column to join by

Elimdistance$id <- 1:nrow(Elimdistance)
trainbachelor <- Elimdistance %>% dplyr::sample_frac(0.70)
testbachelor <- dplyr::anti_join(Elimdistance, trainbachelor, by = 'id')
View(trainbachelor)
View(testbachelor)

#let's calculate the mean of the training data

mu_hat <- mean(trainbachelor$ElimWeek)
rmse1 <- RMSE(testbachelor$ElimWeek, mu_hat)

#if we just use a crude mean then our RMSE is 2.74736

#Now we look at the linear regression model including all the predictors

fit_1 <- lm(ElimWeek ~ ., data=trainbachelor)
bachelorprediction <- predict(fit_1, testbachelor)
actuals_preds <- data.frame(cbind(actuals = testbachelor$ElimWeek, predicted = bachelorprediction))

summary(fit_1)

#It should be noted that the additional character column "id" created NA values but these can be ignored for now.
#A better way would be to create a dataset without that column

plot(actuals_preds$actuals, actuals_preds$predicted, ylim = c(1, 10))

#There is a band of predicted elimination week from 2 weeks to 4 weeks for each actual week.

rmse2 <- RMSE(bachelorprediction, testbachelor$ElimWeek)

#RMSE for the linear regression analysis with all predictors is 2.73258 and not an improvement from just using the mean.

fit_knn <- train(ElimWeek ~ ., method = 'knn', data = trainbachelor)
#
#let's see what this fit looks like
#
summary(fit_knn)
#
#now we can plot this to see where the optimimum nearest neighbors might be
#
plot(fit_knn)
#
#we can see that we might have to go higher in neighbors to decrease the RMSE however, it is still around the same value, and therefore the neighbors is not the problem here
#
testknn <- predict(fit_knn, newdata = testbachelor)
predictedknn <- predict(fit_knn, newdata = testbachelor)
rmse3 <- RMSE(predictedknn, testbachelor$ElimWeek)

#this gives us an RMSE of 3.08453 which is a little worse than the previous models.

plot(Bachelordataclean2$ElimWeek, Bachelordataclean2$Distance)

plot(Bachelordataclean2$ElimWeek, Bachelordataclean2$Age)

bachelordist <- Bachelordataclean2$Distance
boxplot(bachelordist)

fit_3 <- glm(ElimWeek ~ Distance, data = trainbachelor)
predict2 <- predict(fit_3, testbachelor)
summary(fit_3)
rmse4 <- RMSE(predict2, testbachelor$ElimWeek)

summary(fit_knn)



