#get buffers of probabale mining areas
#cluster them to find the border of mining sites
library(caret)
library(randomForest)
library(e1071) 
library(raster)
library(maptools)
library(RStoolbox)
library(rgeos)
library(rgdal)
library(parallel)
#load MNF
MNF <- stack(file.choose())
MNF <- dropLayer(MNF, c(5,6))
# names(MNF) <- sapply(1:dim(MNF)[3], function(i) paste("MNF", i, sep="."))
# rasterlist <- list(raster(MNF, layer = 1), raster(MNF, layer = 2), 
#                    raster(MNF, layer = 3), raster(MNF, layer = 4))
# #load Reflectance
# Reflectance <- stack(file.choose())
# #load thermal bands
# thermal <- stack(file.choose())
# #compute related ratios
# ironoxid <- raster(Reflectance, layer=4)/raster(Reflectance, layer=2)
# ironoxid <- crop(ironoxid, MNF)
# clay  <- raster(Reflectance, layer=6)/raster(Reflectance, layer=7)
# clay <- crop(clay, MNF)
# ratio3 <- raster(Reflectance, layer=7)/raster(Reflectance, layer=3)
# ratio3 <- crop(ratio3, MNF)
# ratio4 <- raster(Reflectance, layer=2)/raster(Reflectance, layer=3)
# ratio4 <- crop(ratio4, MNF)
# quartz <- raster(thermal, layer=1) #first thermal band (band 10)
# #last ratio have different extent
# #crop
# quartz <- crop(quartz, ratio4)
# 
# 
#   
# ratioList <- list(ironoxid, clay, ratio3, ratio4, quartz)
#stack first four MNF layers and ratios
toclass <- MNF
#toclass <- stack(rasterlist, ratioList))

#extract 
values <- sampleRandom(toclass, 10000, na.rm=T)
values <- as.data.frame(values)
##when load from disk
#values <- read.csv(file.choose())
# values <- values[,-1]
##a backup
# valuesBackup <- values

##check for NA
sum(is.na(values))

#normalize the data frame to have comparable data
normal <- preProcess(values)
valuesNormal <- predict(normal, values)
#distances <- dist(valuesNormal)
#clus <- hclust(distances)
#plot(clus)
#
#cut <- cutree(clus, h=12)#distances <- dist(valuesNormal)


#clustering wit kmean
kclus <- kmeans(valuesNormal, 3) 
cut <- kclus$cluster
##clustering without normalization
#distances <- dist(values)
#clus <- hclust(distances)
#plot(clus)
#
#cut <- cutree(clus, h=12)
year <- "2007-2-"
for (i in 2:5){
#clustering wit kmean without normalization
kclus <- kmeans(values, i) 
cut <- kclus$cluster


#train RF model
train_control <- trainControl(method="cv", number=10)

system.time(
modelrf <- train(values, factor(cut), trControl=train_control, method = "rf")
)
print(modelrf$results)

system.time(
  predraster <- predict(toclass, modelrf, 
                        na.rm=T,inf.rm = TRUE)
)

predraster <- calc(predraster, as.integer)
writeRaster(predraster, 
            paste("C:\\Reza\\Randomforest classification\\Mining Borders\\Mining_Borders_", year,"_",i,"class_kmean-Notnormal.tif", sep=""))
print(paste((i-1)*100/4, "%", sep = ""))
}


predshape <- rasterToPolygons(predraster, dissolve = F)
setwd("C:\\Reza\\Randomforest classification\\Mining Borders\\shape")
writeOGR(predshape, "./shape", "predshape2011-4class_kmean",driver = "ESRI Shapefile")
#writeOGR(predshape, "./shape", "predshape",driver = "KML")
