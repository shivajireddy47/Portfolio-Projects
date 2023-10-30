
# install.packages("devtools")
# install.packages("datarium")
# install.packages("outliers")
# install.packages("olsrr")
# install.packages("Metrics")
# install.packages("rcompanion")
# install.packages("caret")
# install.packages("glmnet")




#library(datarium)
# library(ggplot2)
# library(outliers)
# library(car)
# library(olsrr)
# library(tidyverse)
# require(caTools)
# library(Metrics)
# library(MASS)
# library(corrplot)
# library(caret)
# library(plyr)
# library(tidyverse)



head(marketing)
summary(marketing)
youtubeonsales<-marketing[,c("youtube","sales")]
#View(youtubeonsales)
fbonsales<-marketing[,c("facebook","sales")]
#View(fbonsales)
newsonsales<-marketing[,c("newspaper","sales")]
#View(newsonsales)


ggplot(marketing,aes(youtube,sales))+geom_point()+geom_smooth()
ggplot(marketing,aes(facebook,sales))+geom_point()+geom_smooth()
ggplot(marketing,aes(newspaper,sales))+geom_point()+geom_smooth()

plot(marketing)
cor(cbind(marketing))
corrplot(cor(marketing))

####################################################################################################################

#model1

set.seed(007)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(marketing,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
training_dataset =subset(marketing,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
testing_dataset=subset(marketing, sample==FALSE)


model<-lm(sales~.,data=training_dataset)
summary(model)

par(mfrow=c(2,2))
plot(model)


predictions <- predict(model, testing_dataset)

plot(x = predictions, y = testing_dataset$sales, main = "model")
abline(0, 1, col = "red", lwd = 2)

actual_vs_pred = cbind(predictions, testing_dataset$sales)
head(actual_vs_pred)


data.frame( RMSE = RMSE(predictions, testing_dataset $ sales),
            MAE = MAE(predictions, testing_dataset $ sales))


actuals_preds <- data.frame(cbind(actuals=testing_dataset$sales, predicteds=predictions))  # make actuals_predicteds dataframe. 
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy




###########################

#finding the appropriate model



ols_step_forward_p(model)

ols_step_backward_p(model)

ols_step_best_subset(model)


##############################
vif(model)
#create vector of VIF values
vif_values <- vif(model)

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")

#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)


#############################
# rstud_resid = rstudent(model)
# rstud_resid
# #boxplot(model$residuals)
# boxplot(rstud_resid)

#outlier detection

plot(model,4)
cooksd <- cooks.distance(model)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

#influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
#head(marketing[influential, ])  # influential observations.

#outlier(youtubeonsales)


car::outlierTest(model)

###################################################################################################################################




#model2

marketing_new = marketing[-c(131),]
youtube = marketing_new$youtube
facebook = marketing_new$facebook
sales = marketing_new$sales
marketing_new = data.frame(youtube, facebook, sales)

head(marketing_new)

set.seed(007)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(marketing_new,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
training_dataset =subset(marketing_new,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
testing_dataset=subset(marketing_new, sample==FALSE)



model2<-lm(sales~.,data=training_dataset)
summary(model2)

par(mfrow=c(2,2))
plot(model2)

# predicting the target variable
predictions <- predict(model2, testing_dataset)

plot(x = predictions, y = testing_dataset$sales, main = "model2")
abline(0, 1, col = "red", lwd = 2)

actual_vs_pred = cbind(predictions, testing_dataset$sales)
head(actual_vs_pred)

data.frame( RMSE = RMSE(predictions, testing_dataset $ sales),
            MAE = MAE(predictions, testing_dataset $ sales))


actuals_preds <- data.frame(cbind(actuals=testing_dataset$sales, predicteds=predictions))   
# correlation_accuracy <- cor(actuals_preds)
# correlation_accuracy

head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy



########################################################################################################


#final model



set.seed(007)  
sample = sample.split(marketing_new,SplitRatio = 0.75) 
training_dataset =subset(marketing_new,sample ==TRUE) 
testing_dataset=subset(marketing_new, sample==FALSE)


final_model <- lm(sales ~ facebook + poly(youtube, 3)+ facebook*youtube,
              data = training_dataset)

summary(final_model)

par(mfrow=c(2,2))
plot(final_model)


suppressWarnings(predictions <- predict(final_model, testing_dataset))


#predictions <- predict(final_model, testing_dataset)

plot(x = predictions, y = testing_dataset$sales, main = "final_model")
abline(0, 1, col = "red", lwd = 2)

actual_vs_pred = cbind(predictions, testing_dataset$sales)
head(actual_vs_pred)


data.frame( RMSE = RMSE(predictions, testing_dataset $ sales),
            MAE = MAE(predictions, testing_dataset $ sales))


actuals_preds <- data.frame(cbind(actuals=testing_dataset$sales, predicteds=predictions))   
head(actuals_preds)


min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy




#calculating predicted r squared to test if the model is overfit



PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}
pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}
model_fit_stats <- function(linear.model) {
  r.sqr <- summary(linear.model)$r.squared
  adj.r.sqr <- summary(linear.model)$adj.r.squared
  ratio.adjr2.to.r2 <- (adj.r.sqr/r.sqr)
  pre.r.sqr <- pred_r_squared(linear.model)
  press <- PRESS(linear.model)
  return.df <- data.frame("R-squared" = r.sqr, "Adj R-squared" = adj.r.sqr, 
                          "Ratio Adj.R2 to R2" = ratio.adjr2.to.r2, "Pred R-squared" = pre.r.sqr, PRESS = press)
  return(round(return.df,3))
}
ldply(list(final_model), model_fit_stats)




#k-fold cross validation


#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 10)

#fit a regression model and use k-fold CV to evaluate performance
suppressWarnings(model <- train(sales ~ facebook + poly(youtube, 3)+ facebook*youtube, data = marketing_new, method = "lm", trControl = ctrl)
)
#model <- train(sales ~ facebook + poly(youtube, 3)+ facebook*youtube, data = marketing_new, method = "lm", trControl = ctrl)

#view summary of k-fold CV               
print(model)


###################################################################################################################

#quadratic fit


quad <- lm(sales ~ facebook + I(facebook^2) + youtube + I(youtube^2),
              data = marketing_new)

summary(quad)



#polynomial fit

poly <- lm(sales ~ facebook + poly(youtube, 5),
              data = marketing_new)

summary(poly)




















##################################################################################################################################

#finalmodel

# youtube_new<-sqrt(marketing_new$youtube)
# youtube_new
# 
# 
# facebook_new<-marketing_new$facebook
# facebook_new
# 
# sales_new<-marketing_new$sales
# sales_new
# 
# marketing_final_data = data.frame(youtube_new, facebook_new, sales_new)
# marketing_final_data
# 
# #model3<-lm(marketing_new$sales~youtube_new+marketing_new$facebook)
# #summary(model3)
# #par(mfrow=c(2,2))
# #plot(model3)
# #shapiro.test(model$residuals)
# #bptest(sales~.,data=marketing)
# 
# 
# set.seed(007)   #  set seed to ensure you always have same random numbers generated
# sample = sample.split(marketing_final_data,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
# training_dataset =subset(marketing_final_data,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
# testing_dataset=subset(marketing_final_data, sample==FALSE)
# 
# final_model <- lm(sales_new ~., data = training_dataset)
# summary(final_model)
# anova(final_model)
# 
# par(mfrow=c(2,2))
# plot(final_model)
# 
# # predicting the target variable
# predictions <- predict(final_model, testing_dataset)
# 
# 
# plot(x = predictions, y = testing_dataset$sales_new, main = "final model")
# abline(0, 1, col = "red", lwd = 2)
# 
# actual_vs_pred = cbind(predictions, testing_dataset$sales_new)
# 
# head(actual_vs_pred)
# 
# mean_squared_error <- mse(testing_dataset$sales_new,predictions)
# mean_squared_error
# root_mean_squared_error <- rmse(testing_dataset$sales_new,predictions)
# root_mean_squared_error
# mean_abs_error <- mae(testing_dataset$sales_new,predictions)
# mean_abs_error
# 
# #accuracy(testing_dataset$sales_new, predictions)
# #A simple correlation between the actuals and predicted values can be used as a form of accuracy measure.
# #A higher correlation accuracy implies that the actuals and predicted values have similar directional movement, i.e. when the actuals values increase the predicted values also increase and vice-versa.
# 
# actuals_preds <- data.frame(cbind(actuals=testing_dataset$sales_new, predicteds=predictions))  # make actuals_predicteds dataframe. 
# correlation_accuracy <- cor(actuals_preds)
# correlation_accuracy
# 
# head(actuals_preds)
# 
# min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
# min_max_accuracy

##################################################################################################











