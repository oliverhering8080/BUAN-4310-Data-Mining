# Oliver Hering
# Lab Hierarchical Clustering
# BUAN 4310 Data Mining and Big Data

# LOAD DATA
univ <- read.csv("Universities.csv", header = TRUE)
str(univ)
t(t(names(univ)))

# REMOVE MISSING RECORDS

univ_clean <- na.omit(univ)
str(univ_clean)
nrow(univ_clean)

univ_no_na <- na.omit(univ)

# REMOVING VARIABLES THAT ARENT num or int

univ_clean <- univ_clean[, -c(1:3)]
str(univ_clean)

# NORMALIZE

univ_clean_norm <- sapply(univ_clean, scale)
head(univ_clean_norm)

# NORMALIZED DISTANCE

univ_clean_norm_dist <- dist(univ_clean_norm, method = 'manhattan')

# CREATING CLUSTERS

univ_cluster <- hclust(univ_clean_norm_dist, method = 'complete')
plot(univ_cluster, hang = -100, ann = TRUE)

# CHECKING QUALITY OF CLUSTERS

library(factoextra)

fviz_nbclust(univ_clean_norm, 
             hcut, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# it looks like 3 clusters is best

# cutting tree into 3 clusters
univ_cluster_membership <- cutree(univ_cluster, k = 3)
str(univ_cluster_membership)

# creating data frame from clusters
univ_cluster_membership_df <- as.data.frame(univ_cluster_membership)
str(univ_cluster_membership_df)

colnames(univ_cluster_membership_df) <- c("Cluster #")
head(univ_cluster_membership_df)

# MERGERING BACK WITH DATA

univ_clean_clustered <- cbind(univ_no_na, univ_cluster_membership_df)
head(univ_clean_clustered)

# COMPARING CATEGORICAL VARIABLES IN CLUSTERS

table(univ_clean_clustered$'Public.vs.Private', univ_clean_clustered$'Cluster #')

table(univ_clean_clustered$'State', univ_clean_clustered$'Cluster #')



