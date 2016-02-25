
# Import Library
library(FactoMineR)
library(NbClust)        ## package for determing the best number of clusters
library(scales)

# Read and Sort Input Data
mydata <- read.csv(file="MWI_social_eco_env_vac_2012.csv", head=TRUE, sep=",")

# Drop variables
drop_1 <- c("P_30k_75k", "P_30_60", "P_60_90")
# Drop non-numerical data
drop_2 <- c("OBJECTID", "encode_CBG") 
# Drop vacancy data
drop_3 <- c("RES_Sho", "BUS_Sho")

drops <- c(drop_1, drop_2, drop_3)
mydata <- mydata[,!(names(mydata) %in% drops)]

# Inspect missing data
mydata[mydata==-9999]<-NA                   # delete all -9999
sapply(mydata, function(x) sum(is.na(x)))
mydata <- mydata[complete.cases(mydata),]   # get only complete cases
# Normalize the data
mydata2 <- scale(mydata[,1:22])             # make changes here!

# Inspect the correlation 
correlation <- cor(mydata2)

##############################################################################
# Principal components analysis

# 1. PCA analysis using princomp
pc <- princomp(mydata2)
plot(pc)
plot(pc, type='l')
summary(pc) # 4 components is both 'elbow' and explains >85% variance
# 
# # 2. PCA analysis using prcomp
pc <- prcomp(mydata2)

# First for principal components
comp <- data.frame(pc$x[,1:4])

# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

# 3. PCA analysis using factoMinR
pca_res = PCA(mydata, scale.unit = TRUE, ncp=5, quanti.sup=c(21, 22), graph = T)  # change here!
pca_res = PCA(mydata, scale.unit = TRUE, ncp=5, quanti.sup= c(22,23),quali.sup=21, graph = T)

# matrix with eigenvalues
pca_res$eig

# correlations between variables and PCs
pca_res$var$coord  # show loadings associated to each axis

# PCs (aka scores)
head(pca_res$ind$coord)

# The dimdesc() function allows to describe the dimensions. 
result <- dimdesc(pca_res, axes=c(1,2,3,4,5))  # show correlation of variables with 1st axis

###########################################################
result$Dim.1
result$Dim.2
result$Dim.4



write.csv(pca_res$eig, file = "MyResult_1.csv")
write.csv(correlation, file = "MyResult_2.csv")
write.csv(pca_res$var$coord, file = "MyResult_3.csv")
write.csv(result$Dim.1, file = "MyResult_a1.csv")
write.csv(result$Dim.2, file = "MyResult_a2.csv")
write.csv(result$Dim.3, file = "MyResult_a3.csv")
write.csv(result$Dim.4, file = "MyResult_a4.csv")
write.csv(result$Dim.5, file = "MyResult_a5.csv")
write.csv(summary(mydata), file = "MyResult_a6.csv")

aggregate(mydata$RES_Long, by=list(Category=mydata$coastal), FUN=median)
aggregate(mydata$RES_Long, by=list(Category=mydata$coastal), FUN=mean)
aggregate(mydata$RES_Long, by=list(Category=mydata$coastal), FUN=max)
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
