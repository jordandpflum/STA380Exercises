rm(list=ls())

library(tm)
library(topicmodels)
library(caret)
library(ggplot2)
library(keras)
library(reshape2)
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
  files <- paste0("C:/Users/katel/OneDrive/Desktop/QUESTION5/C50test/", author, "/*")
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
X_train_almost <- trainfinal[,!(colnames(trainfinal) %in% c('author', 'articlenum'))]

# rearrange columns based on column sums
xtrain_sorted <- X_train_almost[,order(colSums(-X_train_almost,na.rm=TRUE))]
# create a dataframe with pos/neg scores and top 1000 used words
top1k_words <- xtrain_sorted %>% select(1:1002)
keepcols <- colnames(top1k_words)
# create a dataframe with all other words (not in the top 1000)
idx <- match(keepcols, names(xtrain_sorted))
other_words <- xtrain_sorted[,-idx] 
other_words_total <- rowSums(other_words)

X_train_final <- cbind(top1k_words,other_words_total)

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
testfinal <- testfinal[,c(1,2,1534,1535,3:1533)]

y_test_final <- testfinal[,c('author')]
X_test_almost <- testfinal[,!(colnames(testfinal) %in% c('author', 'articlenum'))]

# create a dataframe with all other words (not in the top 1000)
idx_test <- match(keepcols, names(X_test_almost))
idx_use <- idx_test[!is.na(idx_test)]
missingfromtest <- which(is.na(idx_test))
xtest_keepfromtrain <- X_test_almost[,idx_use]

keepcolsdf <- enframe(keepcols)
missingfromtestdf <- enframe(missingfromtest)
main_data2 <- keepcolsdf[keepcolsdf$name %in% missingfromtestdf$value, ]

avector <- as.vector(main_data2$value)
for(i in avector)
  xtest_keepfromtrain[,i] <- NA

xtest_keepfromtrain[is.na(xtest_keepfromtrain)] <- 0

other_words_test <- X_test_almost[,-idx_use] 
other_words_total <- rowSums(other_words_test)
X_test_almostfinal <- cbind(xtest_keepfromtrain,other_words_total)

library(data.table)

X_test_final <- setcolorder(X_test_almostfinal, c(keepcols))
X_test_final <- X_test_final[,!(names(X_test_final) %in% avector)]
X_train_final <- X_train_final[,!(names(X_train_final) %in% avector)]

########################################################################
# PCA - training
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

########################################################################
# PCA - testing
########################################################################

# First normalize phrase counts to phrase frequencies.
# (often a sensible first step for count data, before z-scoring)
Z_test = X_test_final/rowSums(X_test_final)

# PCA
pc_test = prcomp(Z_test, scale=TRUE, rank=5)
loadings_test = pc_test$rotation
scores_test = pc_test$x

# How are the individual PCs loaded on the original variables?
# The top words associated with each component

# PC1 = looks to be associated with achievement words (eg gains, increase, stronger, boosted)
# opposite end are punishments/harsh things (eg penalty, detained, accused, guilty)
o1_test = order(loadings_test[,1], decreasing=TRUE)
colnames(Z_test)[head(o1_test,25)]
colnames(Z_test)[tail(o1_test,25)]

# PC2
o2_test = order(loadings_test[,2], decreasing=TRUE)
colnames(Z_test)[head(o2_test,25)]
colnames(Z_test)[tail(o2_test,25)]

########################################################################
# MODELING
########################################################################

# Look at the data, call caret package and normalization package
library(caret)
library(BBmisc)
library(kernlab)
library(openxlsx)

# Model 1 (Logistic Regression)
set.seed(123)
mode(scores) = "numeric"
scorex <- data.frame(scores)
training <- cbind(y_train_final, scorex)
training$y_train_final <- factor(training$y_train_final)

scores_test <- data.frame(scores_test)
testing <- cbind(y_test_final, scores_test)
testing$y_test_final <- factor(testing$y_test_final)

logr.fit = glm(y_train_final~., data = training, 
              family = binomial)