## Using the dtwclust package for time series clustering
## dtwclust::tsclust
library(dtwclust)
library(UsingR)
library(readxl)
Dataset <- read_excel("Data Kemiskinan Provinsi Riau (1).xlsx")
head(Dataset)
Kabupaten <- Dataset$`Kabupaten/Kota`
Dataset <- Dataset[,-c(1,2)]
Year <- names(Dataset)
Dataset <- as.matrix(Dataset)
rownames(Dataset) <- Kabupaten
colnames(Dataset) <- Year
Dataset
# k-medoids
Dataset.norm <- BBmisc::normalize(Dataset, method="standardize")
Dataset.norm
# Check the best cluster amount
Result <- matrix(0,nrow = 7,ncol = 10)
for (i in 2:11){
  clust.pam <- tsclust(Dataset.norm, type="partitional", k=i, distance="dtw", centroid="pam")
  Result[,(i-1)]<- t(cvi(clust.pam))
}
rownames(Result)<-names(cvi(clust.pam))
colnames(Result) <- c(2:11)
Result

Sil_max <- which.max(Result[1,]);Sil_max
SF_max <- which.max(Result[2,]);SF_max
CH_max <- which.max(Result[3,]);CH_max
DB_min <- which.min(Result[4,]);DB_min
DBstar_min <- which.min(Result[5,]);DBstar_min
D_max <- which.max(Result[6,]);D_max
COP_min <- which.min(Result[7,]);COP_min
# K = 11 punya nilai paling optimal

# Clustering with 6 clusters
clust.pam <- tsclust(Dataset.norm, type="partitional", k=11, distance="dtw", clustering="pam")

plot(clust.pam, type = "sc")
t(cbind(Dataset[,0], cluster = clust.pam@cluster))

# For each cluster
# Cluster 1
plot(clust.pam, type = "sc", clus = 1L)
plot(clust.pam, type = "series", clus = 1L)
plot(clust.pam, type = "centroids", clus = 1L)

# Cluster 2
plot(clust.pam, type = "sc", clus = 2L)

# Cluster 3
plot(clust.pam, type = "sc", clus = 3L)

# Cluster 4
plot(clust.pam, type = "sc", clus = 4L)

# Cluster 5
plot(clust.pam, type = "sc", clus = 5L)

# CLuster 6
plot(clust.pam, type = "sc", clus = 6L)

# CLuster 7
plot(clust.pam, type = "sc", clus = 7L)

# CLuster 8
plot(clust.pam, type = "sc", clus = 8L)

# CLuster 9
plot(clust.pam, type = "sc", clus = 9L)

# CLuster 10
plot(clust.pam, type = "sc", clus = 10L)

# CLuster 11
plot(clust.pam, type = "sc", clus = 11L)

## Hierarcical Method
clust.hier <- tsclust(Dataset.norm, type = "h", k = 11L, distance = "dtw")
plot(clust.hier)
plot(clust.hier, type="sc")

cutree(clust.hier, k=11L)

cvi(clust.hier)

cvi(clust.pam)

