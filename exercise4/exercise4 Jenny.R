rm(list=ls())

library(dplyr)
library(klaR)

rm(list=ls())

library(dplyr)

social_marketing <- read.csv('social_marketing.csv')

social_marketing_filtered <- social_marketing %>% filter(adult<=5) %>% dplyr::select(-spam)
rownames(social_marketing_filtered) <- social_marketing_filtered$X

## find percentages of tweet types by each user
total_col = apply(social_marketing_filtered[,-1], 1, sum)
pcts = lapply(social_marketing_filtered[,-1], function(x) {
  x / total_col
})


#rank percentages 
pcts_rank = lapply(pcts, rank)


#k-means cluster
X = social_marketing_filtered[,-(1)]


X = scale(X, center=TRUE, scale=TRUE)
clusters <- kmeans(X, 6, nstart=25)

mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

clusters$center[1,]*sigma + mu
clusters$center[2,]*sigma + mu
clusters$center[4,]*sigma + mu

social_marketing_filtered$clustered <- as.factor(clusters$cluster)

str(clusters)

which(clusters$cluster == 1)
which(clusters$cluster == 2)

#k++
clusters2 <- kmeanspp(X, k=4, nstart=25)

which(clusters2$cluster == 1)
which(clusters2$cluster == 2)


clusters$withinss
clusters2$withinss
sum(clusters$withinss)
sum(clusters2$withinss)
clusters$tot.withinss
clusters2$tot.withinss
clusters$betweenss
clusters2$betweenss
