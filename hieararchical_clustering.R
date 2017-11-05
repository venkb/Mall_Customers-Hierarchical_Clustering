#hierarchical clustering
library(stats)#to build the dendrogram & build the hierarchical clusters
library(cluster)#to visualize the cluster

#load the data
#setwd('set the folder that contains Mall_Customers.csv')
dataset_full = read.csv('Mall_Customers.csv')

#select only annual income and spending score for hierarchical clustering
dataset = dataset_full[,4:5]

#scale the variables
dataset = scale(dataset_full)

#determine the optimal number of clusters using dendrogram
dendrogram = hclust(dist(x = dataset, method = 'euclidean'),
                    method = 'ward.D')
plot(dendrogram,
     xlab = 'Customers',
     ylab = 'Euclidean Distance',
     main = 'Dendrogram')

#the optimal number of clusters is 5. lets build 5 clusters using hierarchical clustering
hierarchical_cluster = hclust(dist(x = dataset, method = 'euclidean'),
                              method = 'ward.D')
y_hc = cutree(tree = hierarchical_cluster,
              k = 5)

#plot the clusters
clusplot(x = dataset,
         clus = y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 4,
         span = TRUE,
         xlab = 'Annual Income',
         ylab = 'Spending Score',
         main = 'Cluster Plot')
