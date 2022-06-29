# Group 10 - Rumainum & Osborn
# Intelligence Data Analysis
# Final Project

# Libraries for the Final Project
library(tidyverse)
library(tidyr)
library(dplyr)
library(outliers)
library(EnvStats)
library(ggplot2)
library(mice)
library(caret)
library(earth)
library(Metrics) 
# ggbiplot is attached later on 
# to avoid conflict of plyr and dplyr package
#library(ggbiplot)

# upload data
#setwd("/Users/chrisosborn/Dropbox/Intelligent Design - 5103/Final Project/OnlineNewsPopularity")
setwd("C:/Users/Lince/Documents/Fall 2019/DSA5103-IntelligentDataAnalysis/Final_Project/")
dat <- read.csv("OnlineNewsPopularity.csv")

# make a copy of original data
dat2 <- dat
dat3 <- dat

# look at missing data
myMeanNAsFun <- function(x) mean(is.na(x))
apply(dat, 2, myMeanNAsFun)
md.pattern(dat)

# box plot 
par(mfrow=c(1,2)) # display 1x2
boxplot(dat3$shares)
dat3 <- dat3[-which(dat3$shares > 2900),] 
boxplot(dat3$shares)
par(mfrow=c(1,1)) # reset display

#changing the binary variables to a factor
dat$weekday_is_friday[dat$weekday_is_friday > 0] <- "Friday"
dat$weekday_is_thursday[dat$weekday_is_thursday > 0] <- "Thursday"
dat$weekday_is_wednesday[dat$weekday_is_wednesday > 0] <- "Wednesday"
dat$weekday_is_tuesday[dat$weekday_is_tuesday > 0] <- "Tuesday"
dat$weekday_is_monday[dat$weekday_is_monday > 0] <- "Monday"
dat$weekday_is_saturday[dat$weekday_is_saturday > 0] <- "Saturday"
dat$weekday_is_sunday[dat$weekday_is_sunday > 0] <- "Sunday"
dat$is_weekend[dat$is_weekend > 0] <- "Weekend"

#making all the 0's Null to combine the the columns 
dat$weekday_is_friday[dat$weekday_is_friday == 0] <- NA
dat$weekday_is_thursday[dat$weekday_is_thursday == 0] <- NA
dat$weekday_is_wednesday[dat$weekday_is_wednesday == 0] <- NA
dat$weekday_is_tuesday[dat$weekday_is_tuesday == 0] <- NA
dat$weekday_is_monday[dat$weekday_is_monday == 0] <- NA
dat$weekday_is_saturday[dat$weekday_is_saturday == 0] <- NA
dat$weekday_is_sunday[dat$weekday_is_sunday == 0] <- NA
dat$is_weekend[dat$is_weekend == 0] <- NA

# unite the data for all "day" columns
dat <- unite(dat, "day", weekday_is_monday:weekday_is_sunday, remove = TRUE)

dat$day[dat$day == "Monday_NA_NA_NA_NA_NA_NA"] <- "Monday"
dat$day[dat$day == "NA_Tuesday_NA_NA_NA_NA_NA"] <- "Tuesday"
dat$day[dat$day == "NA_NA_Wednesday_NA_NA_NA_NA"] <- "Wednesday"
dat$day[dat$day == "NA_NA_NA_Thursday_NA_NA_NA"] <- "Thursday"
dat$day[dat$day == "NA_NA_NA_NA_Friday_NA_NA"] <- "Friday"
dat$day[dat$day == "NA_NA_NA_NA_NA_Saturday_NA"] <- "Saturday"
dat$day[dat$day == "NA_NA_NA_NA_NA_NA_Sunday"] <- "Sunday"

# save it as factor
dat$day <- as.factor(dat$day)

#COmbining the data_channel 
dat$data_channel_is_bus[dat$data_channel_is_bus > 0] <- "Business"
dat$data_channel_is_bus[dat$data_channel_is_bus == 0] <- NA

dat$data_channel_is_entertainment[dat$data_channel_is_entertainment > 0] <- "Entertainment"
dat$data_channel_is_entertainment[dat$data_channel_is_entertainment == 0] <- NA

dat$data_channel_is_lifestyle[dat$data_channel_is_lifestyle > 0] <- "Lifestyle"
dat$data_channel_is_lifestyle[dat$data_channel_is_lifestyle == 0] <- NA

dat$data_channel_is_socmed[dat$data_channel_is_socmed > 0] <- "SocialMedia"
dat$data_channel_is_socmed[dat$data_channel_is_socmed == 0] <- NA

dat$data_channel_is_tech[dat$data_channel_is_tech > 0] <- "Technology"
dat$data_channel_is_tech[dat$data_channel_is_tech == 0] <- NA

dat$data_channel_is_world[dat$data_channel_is_world > 0] <- "World"
dat$data_channel_is_world[dat$data_channel_is_world == 0] <- NA

dat <- unite(dat, "data_channel",  data_channel_is_lifestyle:data_channel_is_world, remove = TRUE)

dat$data_channel[dat$data_channel == "NA_NA_Business_NA_NA_NA"] <- "Business"
dat$data_channel[dat$data_channel == "NA_Entertainment_NA_NA_NA_NA"] <- "Entertainment"
dat$data_channel[dat$data_channel == "Lifestyle_NA_NA_NA_NA_NA"] <- "Lifestyle"
dat$data_channel[dat$data_channel == "NA_NA_NA_NA_Technology_NA"] <- "Technology"
dat$data_channel[dat$data_channel == "NA_NA_NA_NA_NA_World"] <- "World"
dat$data_channel[dat$data_channel == "NA_NA_NA_SocialMedia_NA_NA"] <- "SocialMedia"
dat$data_channel[dat$data_channel == "NA_NA_NA_NA_NA_NA"] <- "Other"

# save it as factor
dat$data_channel <- as.factor(dat$data_channel)

#removing unwanted columns
dat <- subset(dat, select = -c(url, timedelta, is_weekend, kw_min_min:self_reference_avg_sharess, 
                               LDA_00:LDA_04,  min_positive_polarity:max_positive_polarity, 
                               min_negative_polarity:max_negative_polarity ))

# visualization of shares vs day 
ggplot(dat, aes(x = day, y = shares)) + geom_point()
# visualization of shares vs data_channel
ggplot(dat, aes(x = data_channel, y = shares)) + geom_point()

#testing Rosner test for outliers
boxplot(dat$shares)
rosnerTest(dat$shares, k = 9216, warn = F)

# Grubbs test for outlier
grubbs.test(dat$shares)
#H0: No outliers; H1:exactly 1 pval 2.2e-16, H0 is rejected in favor of the alternative hypothesis
#we have outlier

# check precentage of how many rows left when shares > 100,000 is removed
nrow(all_dat[(all_dat$shares > 100000), ])/nrow(all_dat)*100 # 58 rows -> 0.15% of data

# remove data with shares > 100,000
dat <- dat[-which(dat$shares > 100000),] 
# check update data
boxplot(dat$shares) 
# check boxplot of log of the update data
boxplot(log(dat$shares+1)) 

# run PCA on the data
dfcTrain_pca <- prcomp(dat[, -c(12, 13)], scale = T)
summary(dfcTrain_pca) # summary of PCA


# create PCA plot
library(ggbiplot)
# biplot for PCA 1 and PCA 2
ggbiplot(dfcTrain_pca, obs.scale = 1, var.scale = 1, varname.size = 6, varname.adjust = 1.1, varname.abbrev = FALSE,
         labels.size = 10, alpha = 0.05, choices = (c(1,2)), circle = TRUE, ellipse = TRUE) +
  xlim(-5,5) +
  ylim(-5,5)
# to resolve error for dplyr
detach(package:ggbiplot)
detach(package:plyr)

# create 4x3 for display for plots
par(mfrow=c(4,3)) 
# create histogram for all attributes
for (i in 1:ncol(dat)){
  if (i != 12 && i != 13) {
    curCol <-  toString(colnames(dat[i]))
    hist(dat[,i], xlab = curCol, main =paste(c("Histogram of ", curCol)))
  }
}
par(mfrow=c(1,1)) # reset display

#colnames(dat)
#log tranformation of all the attributes with the skewed data
dat$n_tokens_content <- log(dat$n_tokens_content+1)
dat$n_unique_tokens <- log(dat$n_unique_tokens+1)
dat$n_non_stop_words <- log(dat$n_non_stop_words+1)
dat$n_non_stop_unique_tokens <- log(dat$n_non_stop_unique_tokens+1)
dat$num_hrefs <- log(dat$num_hrefs+1)
dat$num_self_hrefs <- log(dat$num_self_hrefs+1)
dat$num_imgs <- log(dat$num_imgs+1)
dat$num_videos <- log(dat$num_videos+1)
dat$average_token_length <- log(dat$average_token_length+1)
dat$num_keywords <- log(dat$num_keywords+1)
dat$global_subjectivity <- log(dat$global_subjectivity+1)
dat$global_rate_positive_words <- log(dat$global_rate_positive_words+1)
dat$global_rate_negative_words = log(dat$global_rate_negative_words+1)
dat$rate_positive_words <- log(dat$rate_positive_words+1)
dat$rate_negative_words <- log(dat$rate_negative_words+1)
dat$avg_positive_polarity <- log(dat$avg_positive_polarity+1)
dat$avg_negative_polarity <- log(dat$avg_negative_polarity+1)
dat$title_subjectivity <- log(dat$title_subjectivity+1)
dat$title_sentiment_polarity <- log(dat$title_sentiment_polarity+1)
dat$abs_title_subjectivity <- log(dat$abs_title_subjectivity+1)
dat$abs_title_sentiment_polarity <- log(dat$abs_title_sentiment_polarity+1)


# separate data into training and test data
df_Train1 <-  sample_frac(dat, 0.70) # 70% of data for training data 
rowsNum <- as.numeric(rownames(dat)) # turn rownames() to num
df_Test  <-  dat[-rowNums,] # 30% of data for testing data

# turn shares of Test to NAs
dfTest_wNAs <- df_Test
dfTest_wNAs$shares <- NA

########################################################################################################################

# LINEAR MODEL

########################################################################################################################
# avg_negative_polarity & title_sentiment_polarity removed
# because it causing error in linear model
dfTrain2 <- df_Train1[,-c(12,13,21,23)] 

# using the data from chosen variables after fixing the skewness
lm1 = lm(log(shares+1) ~ ., data = dfTrain2) 

# analysis the behavior of the linear model 1 
summary(lm1)
par(mfrow=c(2,2)) 
plot(lm1)
par(mfrow=c(1,1)) 

# predict the shares and replace anything < 0 with 0
result_shares = predict(lm1, newdata = dfTest_wNAs) 
result_shares = replace(result_shares, which(result_shares < 0), 0)

# RMSE using the first linear model
rmse(log(df_Test$shares+1), result_shares)
mean((log(df_Test$shares+1) - result_shares) ^ 2)


########################################################################################################################

# NON- LINEAR MODEL

########################################################################################################################

# MARS model
marsFit <- earth(log(shares+1) ~ ., dfTrain1, degree = 3, nfold = 3)
# look at summary, the behavior of the model, and its variable importance
summary(marsFit)
plot(marsFit)
varImp(marsFit)

# predict result for MARS model
result_mars <- predict(marsFit, dfTest_wNAs)
result_mars <- replace(result_mars, which(result_mars< 0), 0)

# RMSE for the mars fit
rmse(log(df_Test$shares+1), result_mars)
mean((log(df_Test$shares+1) - result_mars) ^ 2)

# log transform shares data
dat$shares <- log(dat$shares+1)
# re-create new variables with shares already being log transformed
train <-  sample_frac(dat, 0.70) # 70% of data
rowNums <- as.numeric(rownames(train)) # turn rownames() to num
test2  <-  dat[-rowNums,] # 30% of data
test <- test2[ ,-26]

set.seed(321) # create seed 

# using number of resampling iterations of 3 and k-fold cross-validation of 5 
splitRule<-trainControl(method="repeatedcv",repeats=3,number=5)

# train data, view its summary and results for treeBagModel
treeBagModel<-train(shares ~ .,data = train,preProcess= c("center", "scale"), na.action = na.exclude, method = "bagEarth", trControl=splitRule)
summary(treeBagModel)
treeBagModel$results

# train data, view its summary and results for netModel
netModel<-train(shares ~ .,data = train,preProcess= c("center", "scale"), na.action = na.exclude, method = "avNNet", trControl=splitRule)
summary(netModel)
netModel$results

# train data, view its summary and results for knnModel
knnModel<-train(shares ~ .,data = train,preProcess= c("center", "scale"), na.action = na.exclude, method = "knn", trControl=splitRule)
summary(knnModel)
knnModel$results

# train data, view its summary and results for svmModel
svmModel<-train(shares ~ .,data = train,preProcess= c("center", "scale"), na.action = na.exclude, method = "svmLinear", trControl=splitRule)
summary(svmModel)
svmModel$results

#Prediction for treeBagModel, netModel, knnModel, and svmModel, respetively
predictionTree <- treeBagModel %>% predict(test)
predictionNet <- netModel %>% predict(test)
predictionKNN <- knnModel %>% predict(test)
predictionSVM <- svmModel %>% predict(test)

#Prediction for treeBagModel, netModel, knnModel, and svmModel, respetively
rmse(test2$shares, predictionTree)
mean((test2$shares - predictionTree) ^ 2)

rmse(test2$shares, predictionNet)
mean((test2$shares - predictionNet) ^ 2)

rmse(test2$shares, predictionKNN)
mean((test2$shares - predictionKNN) ^ 2)

rmse(test2$shares, predictionSVM)
mean((test2$shares - predictionSVM) ^ 2)

# create normal Q-Q plot for treeBagModel, netModel, knnModel, and svmModel, respetively
qqnorm(predictionTree, pch = 1, frame = FALSE)
qqline(predictionTree, col = "steelblue", lwd = 2)

qqnorm(predictionNet, pch = 1, frame = FALSE)
qqline(predictionNet, col = "steelblue", lwd = 2)

qqnorm(predictionKNN, pch = 1, frame = FALSE)
qqline(predictionKNN, col = "steelblue", lwd = 2)

qqnorm(predictionSVM, pch = 1, frame = FALSE)
qqline(predictionSVM, col = "steelblue", lwd = 2)

# variable importance of kNN model
varimp_kNN <- varImp(knnModel)
plot(varimp_kNN, main="Variable Importance with kNN")