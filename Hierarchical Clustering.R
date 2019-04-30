# HIERARCHICAL CLUSTERING

#Same output (typically) as k-means but different method

dataset <- read.csv('Mall_Customers.csv')
View(dataset)
  
#Start with AGGLOMARATIVE HC (as oppose to DIVISIVE)
#Between data POINTS, Euclidean distance is used
#   EucDis between P1 and P2 = sqrt((x2 - x1)^2 + (y2 - y1)^2)
#Between data CLUSTERS, Average/Point distance within cluster
#   Opt1: closest points, opt2: furthest points, opt3: ave points, opt4: dist between centroids
#AGGLOMARATIVE Clustering algorithm keeps clustering until only one cluster is left
#   this is useful because it stores what clusters it had throughout in a dendrogram

x <- dataset[4:5]

#Using the Dendrogram
#euc dis between points x, then ward.D between clusters
dendrogram <- hclust(dist(x, method = 'euclidean'), method = 'ward.D')
plot(dendrogram, main = paste('Dendrogram'), xlab = 'Customers', ylab = 'Distance')

#fitting the hierarchical clustering algorithm
hc <- hclust(dist(x, method = 'euclidean'), method = 'ward.D')
y_hc <- cutree(hc, 6)
y_hc

#visualise
library(cluster)
clusplot(x,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = 'Cluster of Clients',
         xlab = 'Annual Income',
         ylab = 'Spending Score')
