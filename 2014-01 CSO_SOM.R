# SOM demonstration for Irish Census data in Dublin Area.
#
# Set your R working directory to the location of this file.
# - you will require subdirectories "boundary_files" and "census_data"
#
# Developed with R 3.0.2.
# Accompanying blog post and slides at:
# - http://www.shanelynn.ie/
# - http://www.slideshare.net/shanelynn/2014-0117-dublin-r-selforganising-maps-for-customer-segmentation-shane-lynn
#
# Shane Lynn 12/01/2014
# @shane_a_lynn

### LOAD LIBRARIES - install with:
#install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos", "rgdal"))
library(kohonen)
library(dummies)
library(ggplot2)
library(rgdal)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(rgdal)

# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

### DATA PREPARATION

# Census data comes in counts of people per area. 
# To compare areas, we will convert a number of the
# stats collected into percentages. Without this, 
# the primary differentiator between the different 
# areas would be population size.

#options to explore a few different map types:
small_areas <- TRUE  # Choose between Small Areas or Electoral Districts
filter <- TRUE       # choose to filter output to Dublin area only (good for small areas)

# Load the data into a data frame
# Get the map of these areas and filter for Dublin areas.
if (small_areas){
  data_raw <- read.csv("./census_data/AllThemesTablesSA.csv")  
  ireland_map <- readOGR('./boundary_files/Census2011_Small_Areas_generalised20m.shp', encoding = 'utf8')
  #Note that the map polygons and the census data are not in the same order - rearrangement:
  data_raw <- data_raw[match(ireland_map$SMALL_AREA, data_raw$GEOGDESC),]
  idcol="GEOGDESC"
  
} else {
  data_raw <- read.csv("./census_data/AllThemesTablesED.csv")  
  names(data_raw)[1] <- "GEOGID" 
  ireland_map <- readOGR('./boundary_files/Census2011_Electoral_Divisions_generalised20m.shp', encoding='utf8')
  ireland_map$CSOED <- paste0("E", ireland_map$CSOED)
  #Note that the map polygons and the census data are not in the same order
  data_raw <- data_raw[match(ireland_map$CSOED, data_raw$GEOGID),]
  idcol="GEOGID"
}

#Filter now for certain counties
if (filter){
  counties <- c("Fingal", "Dublin City", "South Dublin", "Dn Laoghaire-Rathdown")
  plot_idx <- ireland_map$COUNTYNAME %in% counties
  data_raw <- data_raw[plot_idx,]
  ireland_map <- ireland_map[plot_idx,]
  rm(counties, filter, plot_idx)  
}

### -------------- Data processing -------------------------

#convert the data from summations to percentages such 
#that the characteristics of each area will be comparable.
source("convertCSOdata.R")
data <- convertCSOdata(data_raw, idcol=idcol)

#Create SOM for Census data - simple as data is well behaved.
#remove incomplete samples:
incompletes <- which(!complete.cases(data))
#where the avr_education_level is NaN - replace with mean
data$avr_education_level[incompletes] <- mean(data$avr_education_level, na.rm=TRUE)
#recalculate after adjustment
incompletes <- which(!complete.cases(data))
if (length(incompletes) > 0){
  print(paste0("Removing ", length(incompletes), " data points that have missing values."))
  data <- data[-incompletes, ]
}
rm(incompletes)

# ------------------- SOM TRAINING ---------------------------

#choose the variables with which to train the SOM
data_train <- data[, c(2,4,5,8)]

# now train the SOM using the Kohonen method
data_train_matrix <- as.matrix(scale(data_train))
names(data_train_matrix) <- names(data_train)
if (small_areas){
  # larger grid for the small areas example (more samples)
  som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")  
} else {
  som_grid <- somgrid(xdim = 10, ydim=10, topo="hexagonal")  
}
# Train the SOM model!
system.time(som_model <- som(data_train_matrix, 
                             grid=som_grid, 
                             rlen=500, 
                             alpha=c(0.1,0.01), 
                             keep.data = TRUE ))

# -------------------- SOM VISUALISATION -----------------

#Visualise the SOM model results
# Plot of the training progress - how the node distances have stabilised over time.

## custom palette as per kohonen package (not compulsory)
source('coolBlueHotRed.R')

plot(som_model, type = "changes")
#counts within nodes
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)
#map quality
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)
#neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
#code spread
plot(som_model, type = "codes")

# Plot the heatmap for a variable at scaled / normalised values
var <- 4 #define the variable to plot
plot(som_model, type = "property", property = getCodes(som_model)[,var], main=colnames(getCodes(som_model))[var], palette.name=coolBlueHotRed)

# Plot the original scale heatmap for a variable from the training set:
var <- 2 #define the variable to plot
var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var], palette.name=coolBlueHotRed)
rm(var_unscaled)

#plot a variable from the original data set (will be uncapped etc.)
# This function produces a menu for multiple heatmaps if a factor or character is chosen
source('plotHeatMap.R')
# A menu of all variables should be displayed if variable=0 
# (note on Mac this will required working XQuartz installation.)
plotHeatMap(som_model, data, variable=0)

# ------------------ Clustering SOM results -------------------

# show the WCSS metric for kmeans for different clustering sizes.
# Can be used as a "rough" indicator of the ideal number of clusters
mydata <- getCodes(som_model)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
par(mar=c(5.1,4.1,4.1,2.1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")

# Form clusters on grid
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 6)

# Show the map with different colours for every cluster						  
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

#show the same plot with the codes instead of just colours
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

# -------------------- MAPPING OF SMALL AREAS (GEO) --------------------------
# Plot the map of ireland, coloured by the clusters the map to show locations.

#plotting map with ggplot requires some data preprocessing.

#get the colour for each area defined in the data
cluster_details <- data.frame(id=data$id, cluster=som_cluster[som_model$unit.classif])

# WARNING - these operations are computationally heavy (~ 30 seconds).
if (small_areas){
  mappoints <- fortify(ireland_map, region="SMALL_AREA")
  mappoints <- merge(mappoints, data, by="id")
  mappoints <- merge(mappoints, cluster_details, by="id")  
} else {
  mappoints <- fortify(ireland_map, region="CSOED")
  mappoints <- merge(mappoints, data, by="id")
  mappoints <- merge(mappoints, cluster_details, by="id") 
}
rm(cluster_details)

# Finally map the areas and colour by cluster
ggplot(mappoints) + aes(long, lat, group=group, fill=factor(cluster)) + geom_polygon()  + coord_equal() + scale_fill_manual(values = pretty_palette) + 
  geom_path(colour="white", alpha=0.5, size=0.05) # if you want an outline

