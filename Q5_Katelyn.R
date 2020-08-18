rm(list=ls())

library(tm)
library(topicmodels)
library(caret)
library(ggplot2)
library(keras)
library(readtext)
library(tidyverse)
library(glmnet)
library(tidytext)
library(dplyr)
library(tibble)
library(textdata)

########################################################################
# READ IN FILES
########################################################################

setwd("C:/Users/katel/OneDrive/Desktop/QUESTION5/C50train")

# read in txt files for training set
a <- 1
txtfile <- list()
X_train <- list()
y_train <- list()
tmp = list.files(pattern="*.*")

for (i in 1: length(tmp)) {for (x in 1:50)
{ y_train[a] <- tmp[i]
  a <- a+1}
  author <- tmp[i]
  files <- paste0("C:/Users/katel/OneDrive/Desktop/QUESTION5/C50train/", author, "/*")
  nextauth <- readtext(files)
  X_train <- rbind(X_train, nextauth)}
X_train <- X_train$text

# read in txt files for testing set
a <- 1
txtfile <- list()
X_test <- list()
y_test <- list()
tmp = list.files(pattern="*.*")

for (i in 1: length(tmp)) {for (x in 1:50)
{ y_test[a] <- tmp[i]
a <- a+1}
  author <- tmp[i]
  files <- paste0("C:/Users/katel/OneDrive/Desktop/QUESTION5/C50train/", author, "/*")
  nextauth <- readtext(files)
  X_test <- rbind(X_test, nextauth)}
X_test <- X_test$text

########################################################################
# PREPROCESSING - TRAIN SET
########################################################################

# create and export csv file for train info
df_train <- data.frame(matrix(unlist(y_train), nrow=length(y_train), byrow=T))
author_train <- cbind(df_train,X_train)
colnames(author_train) <- c("author", "article")
write.csv(author_train,"C:/Users/katel/OneDrive/Desktop/QUESTION5/author_train.csv", row.names = FALSE)

# create article number column
author_train$articlenum <- 1:nrow(author_train)

# get word counts for individual words (minus fillers)
# get sentiment scores for words (negative or positive: values from -5 to 5)
afinn <- get_sentiments("afinn")
train_a <- unnest_tokens(author_train, input = article, output = word) %>%
  anti_join(stop_words) %>% 
  count(articlenum, author, word, sort = TRUE) %>%
  inner_join(afinn, by = "word")

# transpose word count columns to create variables based on particular word counts
train_b <- train_a %>% group_by(articlenum,author,word=word) %>% 
  summarise(word_=sum(n)) %>%
  melt(id.var=c("articlenum","author","word")) %>%
  dcast(articlenum + author ~ variable + word)

# fill n/a values with 0
train_b[is.na(train_b)] <- 0

# create positive and negative score columns that add up the total sentiment
# for example: if 'cut' has a score of -1 and is used 16 times, the negative
# score column would have a value of 16
train_a$positive_score <- ifelse(train_a$n*train_a$value >=0, train_a$n*train_a$value, 0)
train_a$negative_score <- ifelse(train_a$n*train_a$value <0, -1*(train_a$n*train_a$value), 0)

avgsentiment <-  train_a %>% group_by(articlenum) %>% 
  summarise(tot_pos_score = sum(positive_score), 
            tot_neg_score = sum(negative_score))

trainfinal <- merge(train_b,avgsentiment,by="articlenum")
trainfinal <- trainfinal[,c(1,2,1484,1485,3:1483)]
y_train_final <- trainfinal[,c('author')]
X_train_final <- trainfinal[,!(colnames(trainfinal) %in% c('author', 'articlenum'))]

########################################################################
# PREPROCESSING - TEST SET
########################################################################

# create and export csv file for test info
df_test <- data.frame(matrix(unlist(y_test), nrow=length(y_test), byrow=T))
author_test <- cbind(df_test,X_test)
colnames(author_test) <- c("author", "article")
write.csv(author_test,"C:/Users/katel/OneDrive/Desktop/QUESTION5/author_test.csv", row.names = FALSE)

# create article number column
author_test$articlenum <- 1:nrow(author_test)

# get word counts for individual words (minus fillers)
# get sentiment scores for words (negative or positive: values from -5 to 5)
afinn <- get_sentiments("afinn")
test_a <- unnest_tokens(author_test, input = article, output = word) %>%
  anti_join(stop_words) %>% 
  count(articlenum, author, word, sort = TRUE) %>%
  inner_join(afinn, by = "word")

# transpose word count columns to create variables based on particular word counts
test_b <- test_a %>% group_by(articlenum,author,word=word) %>% 
  summarise(word_=sum(n)) %>%
  melt(id.var=c("articlenum","author","word")) %>%
  dcast(articlenum + author ~ variable + word)

# fill n/a values with 0
test_b[is.na(test_b)] <- 0

# create positive and negative score columns that add up the total sentiment
# for example: if 'cut' has a score of -1 and is used 16 times, the negative
# score column would have a value of 16
test_a$positive_score <- ifelse(test_a$n*test_a$value >=0, test_a$n*test_a$value, 0)
test_a$negative_score <- ifelse(test_a$n*test_a$value <0, -1*(test_a$n*test_a$value), 0)

avgsentiment <-  test_a %>% group_by(articlenum) %>% 
  summarise(tot_pos_score = sum(positive_score), 
            tot_neg_score = sum(negative_score))

testfinal <- merge(test_b,avgsentiment,by="articlenum")
testfinal <- testfinal[,c(1,2,1484,1485,3:1483)]

y_test_final <- testfinal[,c('author')]
X_test_final <- testfinal[,!(colnames(testfinal) %in% c('author', 'articlenum'))]

########################################################################
# PCA
########################################################################

# First normalize phrase counts to phrase frequencies.
# (often a sensible first step for count data, before z-scoring)
Z = X_train_final/rowSums(X_train_final)

# PCA
pc2 = prcomp(Z, scale=TRUE, rank=5)
loadings = pc2$rotation
scores = pc2$x

# How are the individual PCs loaded on the original variables?
# The top words associated with each component

# PC1 = looks to be associated with achievement words (eg gains, increase, stronger, boosted)
# opposite end are punishments/harsh things (eg penalty, detained, accused, guilty)
o1 = order(loadings[,1], decreasing=TRUE)
colnames(Z)[head(o1,25)]
colnames(Z)[tail(o1,25)]

# PC2
o2 = order(loadings[,2], decreasing=TRUE)
colnames(Z)[head(o2,25)]
colnames(Z)[tail(o2,25)]

