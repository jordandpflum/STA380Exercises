rm(list=ls())

library(tinytex)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(png)
library(arules)
library(arulesViz)

setwd("C:/Users/katel/OneDrive/Desktop/R")

# read transactions
groceries <- read.transactions("groceries.txt", format = "basket", sep = ",")
groceries_df <- as(groceries, "data.frame") 
# see dimensions
dim(groceries)
# see basic summary
summary(groceries)
# see the first 10 transactions
arules::inspect(groceries[1:10])
# see the number of items in first 20 transactions
size(groceries[1:20])
# see list of item labels
itemLabels(groceries)

##########################################################
# initial plot
##########################################################

# plot most frequently sold items
itemFrequencyGGPlot <- function(x, topN) {
  x %>%
    itemFrequency %>%
    sort %>%
    tail(topN) %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>%
    ggplot(aes(reorder(rowname, `.`),`.`)) + 
    theme_bw() +
    geom_col(fill="#F05483") + 
    coord_flip() +
    labs(y="Frequency", x ="Item", title = "Most Frequently Purchased Grocery Items") +
    theme(plot.title = element_text(hjust = 0.5))
}  

itemFrequencyGGPlot(groceries, topN=20)

# plot least purchased items
itemFrequencyGGPlot <- function(x, topN) {
  x %>%
    itemFrequency %>%
    sort %>%
    head(topN) %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>%
    ggplot(aes(reorder(rowname, `.`),`.`)) + 
    theme_bw() +
    geom_col(fill="#FFAAC4") + 
    coord_flip() +
    labs(y="Frequency", x ="Item", title = "Least Frequently Purchased Grocery Items") +
    theme(plot.title = element_text(hjust = 0.5))
}  

itemFrequencyGGPlot(groceries, topN=20)

##########################################################
# APRIORI
##########################################################

# Which groups of items are most frequently purchased together? (highest support)
# apriori alogrithm (support 5%)
grocRules <- apriori(groceries, parameter = list(supp = 0.05, conf = .0001, minlen=2, maxlen=5))
grocRulesDF <- as(grocRules, "data.frame") 
grocRulesDF[order(grocRulesDF$support, decreasing=TRUE), ] 

# Which groups of items have the highest lift?
# apriori alogrithm
grocRules <- apriori(groceries, parameter = list(supp = 0.001, conf = .001, minlen=2, maxlen=5))
grocRulesDF <- as(grocRules, "data.frame") 
grocRulesDF[order(grocRulesDF$lift, decreasing=TRUE)[0:20], ] 

# Interesting rules - which ones have higher confidence and higher lift
# apriori alogrithm
grocRulesInt <- apriori(groceries, parameter = list(supp = 0.001, conf = .5, minlen=2, maxlen=5))
inspect(sort(subset(grocRulesInt,subset = lift > 2,by = 'support',decreasing = T)))

# Interesting rules - without whole milk and other vegetables?
# apriori alogrithm
grocRulesInt2 <- apriori(groceries, parameter = list(supp = 0.0015, conf = .5, minlen=2, maxlen=5))
inspect(sort(subset(grocRulesInt2,subset=!(rhs %in% c('whole milk','other vegetables')) & lift > 3,by = 'support',decreasing = T)))

##########################################################
# plots
##########################################################

plot(grocRulesInt)
plot(grocRulesInt2)
plot(grocRulesInt2, measure = c("support", "lift"), shading = "confidence")
plot(grocRules, method='two-key plot')
plot(grocRulesInt2, method='two-key plot')

top10_confidence = head(grocRulesInt, n = 10, by = "lift")
plot(top10_confidence, method = "graph",  engine = "htmlwidget")

# prep rules for gephi
saveAsGraph(head(grocRulesInt, n = 1000, by = "lift"), file = "grocRulesInt.graphml")
