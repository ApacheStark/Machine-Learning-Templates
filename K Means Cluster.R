# K MEANS CLUSTERING

# To avoid Random Intiliasation Trap by using K-Means++ algorithm
# this allows you to actually choose your centroids

# WCSS and elbow method for selecting clusters

dataset <- read.csv('Mall_Customers.csv')
head(dataset)
x <- dataset[4:5]

#ELBOW METHOD
set.seed(6)
wcss <- vector()
for (i in 1:10) wcss[i] <- sum(kmeans(x, i)$withins)
plot(1:10, wcss, type = 'b', main = 'Number of Clusters', xlab = 'Clusters', ylab = 'WCSS')
#clearly shows the significant drops stop at 5
# therefore K = 5

#Applying K Means
set.seed(10)
kmeans <- kmeans(x, 5, iter.max = 300, nstart = 10)

#Visulaing Clusters
library(cluster)
clusplot(x,
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = 'Cluster of Clients',
         xlab = 'Annual Income',
         ylab = 'Spending Score')
