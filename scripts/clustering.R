#Spatial data handling
library(rgdal)
library(spdep)
library(rgeos)
#For charting
library(tmap)
library(cartogram)
library(ggplot2)
library(gridExtra)
library(GGally)
#For data loading and munging
library(readr)
library(dplyr)
library(tidyr)
#For spatial stats
library(GWmodel)
library(spdep)
#For cluster analysis
library(cluster)
#For reshape datasets
library(reshape2)
#VIF library
library(car)

sp_elections <- readOGR(dsn = "shapefile", layer = "spain_elections")

####################################################################################################################

# Clustering of the regions by vote tendency
nClusters = 2

parties <- kmeans(sp_elections@data[c("PP","PSOE","Podemos","Ciudadanos")], centers = nClusters,nstart = 5 )$cluster
trends_new <- kmeans(sp_elections@data[c("new_old")], centers = nClusters,nstart = 5)$cluster
trends_right <- kmeans(sp_elections@data[c("right_left")], centers = nClusters,nstart = 5)$cluster
demograph <- kmeans(scale(sp_elections@data[c("child","young","adult","old","male","female","spanish","foreigners")]), centers = nClusters,nstart = 5)$cluster
economic <- kmeans(scale(sp_elections@data[c("agriculture","construction","industry","services","unemployed","gdp")]), centers = nClusters,nstart = 5)$cluster
features <- kmeans(scale(sp_elections@data[c("agriculture","construction","industry","services","unemployed","gdp",
                                             "child","young","adult","old","male","female","spanish","foreigners")]), centers = nClusters,nstart = 5)$cluster


sp_elections@data <- sp_elections@data %>%
  mutate(cluster_parties = parties,
         cluster_new = trends_new,
         cluster_right = trends_right,
         cluster_demographs = demograph,
         cluster_economic = economic,
         cluster_features = features)

# Visualize the clusters and the regions by parties
tm_shape(sp_elections)+
  tm_fill(col="cluster_parties",style="kmeans",palette="Dark2")+
  tm_layout(
    title="Clustering by similitude of vote",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )

# Visualize the clusters and the regions by trend
tm_shape(sp_elections)+
  tm_fill(col="cluster_new",style="kmeans",palette="Dark2")+
  tm_layout(
    title="Clustering by similitude of new and old parties",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )

# Visualize the clusters and the regions by trend
tm_shape(sp_elections)+
  tm_fill(col="cluster_right",style="kmeans",palette="Dark2")+
  tm_layout(
    title="Clustering by similitude of political trend",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )


# Visualize the clusters and the regions by demography
tm_shape(sp_elections)+
  tm_fill(col="cluster_demographs",style="kmeans",palette="Dark2")+
  tm_layout(
    title="Clustering by similitude of demographic features",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )

# Visualize the clusters and the regions by economic similitudes
tm_shape(sp_elections)+
  tm_fill(col="cluster_economic",style="kmeans",palette="Dark2")+
  tm_layout(
    title="Clustering by similitude of economic features",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )

# Visualize the clusters and the regions by economic similitudes
tm_shape(sp_elections)+
  tm_fill(col="cluster_features",style="kmeans",palette="Dark2")+
  tm_layout(
    title="Clustering by similitude of features",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )