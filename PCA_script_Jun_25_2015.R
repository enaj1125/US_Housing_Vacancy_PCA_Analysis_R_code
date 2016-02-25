# Import Library
library(FactoMineR)
library(NbClust)  ## package for determing the best number of clusters
library(RColorBrewer)
library(scales)

# Read and Sort Input Data
mydata <- read.csv(file="three_county_6_25_2.csv", head=TRUE, sep=",")
#mydata <- read.csv(file="US_CBG_Census_Urban_AllData.csv", head=TRUE, sep=",")
head(mydata)

# Get rid of rows with any missing data including -9999 and no values
mydata <- subset(mydata, employ_rat != -9999 & n_price_less_5!= -9999 & n_price_5_10 != -9999)
mydata$CBG_encode <- NULL  # delete columns
mydata$NOBJECTID <- NULL
mydata$OBJECTID <- NULL
mydata$rec <- NULL
mydata$healthcenter <- NULL
mydata$under_1pct <- NULL
mydata <- mydata[complete.cases(mydata),] 

# Normalize the data
mydata2 <- scale(mydata)

# Inspect the correlation 
correlation <- cor(mydata)

# Principal components
pc <- princomp(mydata2)
plot(pc)
plot(pc, type='l')
summary(pc) # 4 components is both 'elbow' and explains >85% variance
pc$loadings

# PCA analysis using factoMinR
pca_res = PCA(mydata, scale.unit = TRUE, ncp=5, graph = T)
#pca_res = PCA(mydata, scale.unit = TRUE, ncp=5, quanti.sup= c(15,14),quali.sup=16, graph = T)


################################################################
###################### Clustering Analysis

# Determine number of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(mydata2)

set.seed(1234)
nc <- NbClust(mydata2, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]), xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")

# K-means clustering
set.seed(1234)
fit.km <- kmeans(mydata2, centers = 2, nstart=25)   ## Change K here!!!                        
fit.km$size
fit.km$centers
aggregate(mydata, by=list(cluster=fit.km$cluster), mean)

# Visualize the clusters
library(cluster)
library(fpc)
# Fig 1
plotcluster(mydata2, fit.km$cluster)
# Fig 2
clusplot(mydata2, fit.km$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
# Fig 3
with(mydata, pairs(mydata2, col=c(1:3)[fit.km$cluster]))

# Cluster sizes
sort(table(fit.km$clust))
clust <- names(sort(table(fit.km$clust)))

# First cluster
cluster1 <- row.names(mydata[fit.km$clust==clust[1],])
# Second Cluster
cluster2 <- row.names(mydata[fit.km$clust==clust[2],])
# Third Cluster
cluster3 <- row.names(mydata[fit.km$clust==clust[3],])
# Fourth Cluster
#row.names(mydata[k$clust==clust[4],])
# Fifth Cluster
#row.names(mydata[k$clust==clust[5],])


##############################################
# # Analyze the propoertities of clusters

mydata$cluster <- fit.km$clust  # add cluster number to each group

# subset into three groups:
Group1 <- subset(mydata, cluster == 1)
Group2 <- subset(mydata, cluster == 2)
Group3 <- subset(mydata, cluster == 3)

summary(Group1)
summary(Group2)
summary(Group3)

# Write Output 
write.csv(data, file = "summary.csv")
write.csv(fit.km$clust, file = "cluster.csv")