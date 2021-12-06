# Hierarchical Clustering Demo
# Week 9


star_wars_planets <- read.csv("star_wars_planets_v5.csv", header = TRUE)
head(star_wars_planets)

str(star_wars_planets)

star_wars_planets_filter <- subset(star_wars_planets[c(7, 10, 12)])
str(star_wars_planets_filter)



star_wars_planets_filter_norm <- sapply(star_wars_planets_filter, scale)
head(star_wars_planets_filter_norm)



star_wars_planets_filter_norm_m <- dist(star_wars_planets_filter_norm, 
                                        method = "euclidean")
head(star_wars_planets_filter_norm_m)



star_wars_planets_filter_norm_m_hc <- hclust(star_wars_planets_filter_norm_m, 
                                             method = "ward.D2")
plot(star_wars_planets_filter_norm_m_hc, 
     hang = -100, 
     ann = TRUE,
     xlab = "", 
     main = "Clusters in a galaxy far, far away")


install.packages('factoextra')
library(factoextra)

fviz_nbclust(star_wars_planets_filter_norm, 
             hcut, method = "silhouette") +
  labs(subtitle = "Silhouette method")









