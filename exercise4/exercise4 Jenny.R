rm(list=ls())

library(dplyr)
library(klaR)

social_marketing <- read.csv('social_marketing.csv')

social_marketing_filtered <- social_marketing %>% filter(adult<=5) %>% dplyr::select(-spam)
rownames(social_marketing_filtered) <- social_marketing_filtered$X


## find percentages of tweet types by each user
total_col = apply(social_marketing_filtered[,-1], 1, sum)
pcts = lapply(social_marketing_filtered[,-1], function(x) {
  x / total_col
})

pcts$X = social_marketing_filtered$X

pcts = as.data.frame(pcts)


#k-means cluster

X = pcts[ , !(names(pcts) %in% c('X'))]


X = scale(X, center=TRUE, scale=TRUE)
clusters <- kmeans(X, 6, nstart=25)

mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

cluster1 <- clusters$center[1,]*sigma + mu
cluster2 <- clusters$center[2,]*sigma + mu
cluster3 <- clusters$center[3,]*sigma + mu
cluster4 <- clusters$center[4,]*sigma + mu
cluster5 <- clusters$center[5,]*sigma + mu
cluster6 <- clusters$center[6,]*sigma + mu

#sort each cluster's values
cluster1 <- as.data.frame(cluster1)
cluster1$categories <- row.names(cluster1)
cluster1[order(cluster1$cluster1, decreasing = TRUE),]

cluster2 <- as.data.frame(cluster2)
cluster2$categories <- row.names(cluster2)
cluster2[order(cluster2$cluster2, decreasing = TRUE),]

cluster3 <- as.data.frame(cluster3)
cluster3$categories <- row.names(cluster3)
cluster3[order(cluster3$cluster3, decreasing = TRUE),]

cluster4 <- as.data.frame(cluster4)
cluster4$categories <- row.names(cluster4)
cluster4[order(cluster4$cluster4, decreasing = TRUE),]

cluster5 <- as.data.frame(cluster5)
cluster5$categories <- row.names(cluster5)
cluster5[order(cluster5$cluster5, decreasing = TRUE),]

cluster6 <- as.data.frame(cluster6)
cluster6$categories <- row.names(cluster6)
cluster6[order(cluster6$cluster6, decreasing = TRUE),]
