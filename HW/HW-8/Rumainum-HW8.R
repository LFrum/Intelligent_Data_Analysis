#---
#title: "Rumainum-HW8"
#author: "Lince Rumainum"
#date: "December 3, 2019"
#output: word_document
#---

## Intelligence Data Analysis - HW 8

#```{r, echo=TRUE, fig.width=16, fig.height=10}
# list of libraries for HW5
#load all of the packages associated with the tidyverse
#includes dplyr, tidyr, and magritter
library(tidyverse)
#library(car) # for 'symbox' function
#library(EnvStats) # for "boxcox" function
library(ggplot2) # plots
library (VIM) # for aggr function
library (mice) # for imputation 
library(caret)
# for clustering
library(cluster)    #provides more cluster algorithms than base R (e.g. PAM)
library(useful)     #provides a plot function for clusters and "FitKMeans" and "PlotHartigan" functions
library(NbClust)    #provides tons of tools for identifying the "right" number of clusters
library(rgl)        #for 3D rotating plots

################################################################################################################################################
# load data
dfAll <- read.csv("C:/Users/Lince/Documents/Fall 2019/DSA5103-IntelligentDataAnalysis/Final_Project/OnlineNewsPopularity.csv")

# make a copy of the data frame
df_All <- dfAll

# get row length
data_length <- nrow(df_All) 
data_length

# creating function for the mean of the missing values
myMeanNAsFun <- function(x) mean(is.na(x))

# see the percentage of missing data for each variable
apply(df_All, 2, myMeanNAsFun)
# result: complete data without anything missing

# see column info
str(df_All) # 61 columns inlcude URLs

# separate data into training and test data
dfTrain1 <-  sample_frac(df_All, 0.70) # 70% of data for training
rowNums <- as.numeric(rownames(dfTrain1)) # turn rownames() to num
dfTest1  <-  df_All[-rowNums,] # 30% of data for testing

# turn shares of Test to NAs
dfTest_wNA <- dfTest1
dfTest_wNA$shares <- NA

# see if any data missing using md.pattern  
md.pattern(dfTrain1)

# PCA
# compute pca using prcomp
dfcTrain_pca <- prcomp(dfTrain1[,-c(1)],scale = T)
summary(dfcTrain_pca) # summary of PCA

# 30 variables needed to reach a ~90% cummulative proportion 
# 23 variables needed to reach a ~80% cummulative proportion 
# 18 variables needed to reach a ~70% cummulative proportion 
# 13 variables needed to reach a ~60% cummulative proportion 
# 10 variables needed to reach a ~50% cummulative proportion 

library(ggbiplot)
# biplot for PCA 1 and PCA 2
ggbiplot(dfcTrain_pca, obs.scale = 1, var.scale = 1, varname.size = 6, varname.adjust = 1.1, varname.abbrev = FALSE,
         labels.size = 10, alpha = 0.05, choices = (c(1,2)), circle = TRUE, ellipse = TRUE) +
  xlim(-5,5) +
  ylim(-5,5)
# to resolve error for dplyr
detach(package:ggbiplot)
detach(package:plyr)


# analyze the data against shares for chosen variables from PCA

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = n_tokens_title, y = shares)) 

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = n_tokens_content, y = shares)) # skewed
# n_tokens_content > 4000 are outliers
new_dfTr <- dfTrain1[dfTrain1$n_tokens_content < 4000,]   

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = num_hrefs, y = shares)) # skewed
# num_hrefs > 150 are outliers
new_dfTr <- new_dfTr[new_dfTr$num_hrefs < 150,]  

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = num_imgs, y = shares)) # skewed
# num_imgs > 100 are outliers
new_dfTr <- new_dfTr[new_dfTr$num_imgs < 100,]  

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = num_videos, y = shares)) #  skewed
# num_videos > 75 are outliers
new_dfTr <- new_dfTr[new_dfTr$num_videos < 75,]  

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = num_keywords, y = shares)) # skewed
# num_keywords values between 0 to 10

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = data_channel_is_entertainment, y = shares)) #  0 or 1
# data_channel_is_entertainment

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = kw_max_min, y = shares)) # skewed
# kw_max_min > 1.5e5 are outliers
new_dfTr <- new_dfTr[new_dfTr$kw_max_min < 1.5e5,]  

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = kw_avg_min, y = shares)) #  skewed
# kw_avg_min > 20000 are outliers
new_dfTr <- new_dfTr[new_dfTr$kw_avg_min < 20000,]  

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = kw_min_max, y = shares))  #  skewed
# kw_min_max 

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = kw_max_max, y = shares)) # skewed
# kw_max_max

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = kw_avg_max, y = shares)) # skewed
# kw_avg_max

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = kw_min_avg, y = shares)) #  skewed
# kw_min_avg

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = kw_max_avg, y = shares)) #  skewed
# kw_max_avg > 1.5e5 are outliers
new_dfTr <- new_dfTr[new_dfTr$kw_max_avg < 1.5e5,]  

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = kw_avg_avg, y = shares)) # skewed
#kw_avg_avg > 25000 are outliers
new_dfTr <- new_dfTr[new_dfTr$kw_avg_avg < 25000,]  

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = self_reference_min_shares, y = shares)) # skewed
# self_reference_min_shares > 3e5 are outliers
new_dfTr <- new_dfTr[new_dfTr$self_reference_min_shares < 3e5,] 

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = self_reference_max_shares, y = shares)) #  skewed
# self_reference_max_shares > 7e5 are outliers
new_dfTr <- new_dfTr[new_dfTr$self_reference_max_shares < 7e5,] 

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = self_reference_avg_sharess, y = shares)) #  skewed
# self_reference_avg_sharess > 7e5 are outliers
new_dfTr <- new_dfTr[new_dfTr$self_reference_avg_sharess < 7e5,] 

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = weekday_is_saturday, y = shares)) # 0 or 1

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = weekday_is_sunday, y = shares)) #  0 or 1

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = is_weekend, y = shares)) #  0 or 1


ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = LDA_00, y = shares)) # 

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = LDA_01, y = shares)) # 

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = LDA_03, y = shares)) #  

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = global_rate_negative_words, y = shares)) # skewed

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = rate_negative_words, y = shares)) #  


ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = title_subjectivity, y = shares)) # 

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = abs_title_subjectivity, y = shares)) #  

ggplot(data = dfTrain1) + 
  geom_point(mapping = aes(x = abs_title_sentiment_polarity, y = shares)) # 

nrow(dfTrain1[dfTrain1$shares > 100000,]) # only 44 rows out of 27751
new_dfTr <- new_dfTr[new_dfTr$shares < 100000,] # shares over 100,000 removed


colnames(new_dfTr)

# we are taking url, n_tokens_title, n_tokens_content,
# num_hrefs, num_self_hrefs, num_imgs, num_videos, num_keywords,
# all the days of the week columns, and shares
new_dfTrain <- new_dfTr[,c(1, 3, 4, 8, 9, 
                           10, 11, 13,   
                           32, 33, 34, 35, 
                           36, 37, 38, 39,
                           61)]
# row & column info
colnames(new_dfTrain)
ncol(new_dfTrain)
nrow(new_dfTrain)
str(new_dfTrain)

#clustData <-  sample_frac(new_dfTrain, 0.10) # 10% of data for training
clustData <-  sample_frac(new_dfTrain, 0.25) # 25% of data for training

# scale the data..
trainData<-data.frame(round(scale(clustData[,-1]), 3))  #default is mean centering with scaling by standard deviation
row.names(trainData)<-clustData$url   
trainData <-as.data.frame(trainData)


#we will start by looking at partitions
wssplot(trainData, nc = 50)   #-- no clear elbow!  

set.seed(1000)      #just so that we will all get the same results!
newsScaled <- data.frame(round(scale(clustData[,-1]), 3))
kclus <- kmeans(newsScaled, 2, nstart = 15) 
pclus <- pam(newsScaled, 2) 

# any cluster above 2 becomes more mixed in to each other while only one 
# that is always seperated from the other clusters

# compare centroids and medoids
round(kclus$centers, 3)   # centroids from KM
pclus$medoids  # medoids of PAM

# plot the k-mean clustering
plot(kclus, data = trainData) #this plot function comes from the "useful" libary uses PCA 
# plot the k-medoids PAM clustering
plot(pclus, data = trainData) 

# put centers and medoids into data frame
KM <- data.frame(kclus$centers)
PAM <- data.frame(pclus$medoids)

# plot the shares vs weekday_is_tuesday and the cluster color
p <- qplot(data=newsScaled, x=weekday_is_tuesday, y=shares, color=factor(pclus$clustering)) 
g <- guide_legend("Cluster")                     # retitle the legend...
p <- p+guides(color = g, size = g, shape = g)    # retitle the legend...
# add the k-medoids  
p <- p + geom_point(data = PAM, aes(x = weekday_is_tuesday, y = shares), size=4,colour="black")
p
# add the k-mean's centroids 
p <- p +  geom_point(data = KM, aes(x = weekday_is_tuesday, y = shares), size=4,colour="blue")
p

# plot the shares vs weekday_is_wednesday and the cluster color
p <- qplot(data=newsScaled, x=weekday_is_wednesday, y=shares, color=factor(pclus$clustering)) 
g <- guide_legend("Cluster")                     # retitle the legend...
p <- p+guides(color = g, size = g, shape = g)    # retitle the legend...
# add the k-medoids  
p <- p + geom_point(data = PAM, aes(x = weekday_is_wednesday, y = shares), size=4,colour="black")
p
# add the k-mean's centroids 
p <- p +  geom_point(data = KM, aes(x = weekday_is_wednesday, y = shares), size=4,colour="blue")
p

# cluster information
clusInfo<-data.frame(kclus$centers,kclus$size)
clusInfo

colnames(newsClus)
newsClus <- data.frame(trainData, clust=kclus$cluster, trainData)
head(newsClus[newsClus$clust==1,c("n_tokens_title","n_tokens_content","num_imgs","num_videos","num_keywords","is_weekend","shares")])
head(newsClus[newsClus$clust==2,c("n_tokens_title","n_tokens_content","num_imgs","num_videos","num_keywords","is_weekend","shares")])


#hierarchical clustering 
d <- daisy(trainData)

hclustering <- hclust(d,method="complete")   
plot(hclustering, labels=FALSE)
# if we were to "cut" at k = 2, what are the groups?
rect.hclust(hclustering, k=2, border="red")     

# pca
pcaNews<-prcomp(trainData,scale=T)

trainData$hcluster <- as.factor(cutree(hclustering, k=2))   #cutting at k=2

p <- qplot(data = data.frame(pcaNews$x), x = PC1, y = PC2, color=factor(trainData$hcluster))  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g)    #retitle the legend...



############################################################################################

######################################
####################
# END OF PROBLEM 1 #
####################
```