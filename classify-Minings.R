#get classifcation rasters of soil vs urban and soil vs roof. 
#gather soils of both
#classify soil types
library(caret)
library(randomForest)
library(e1071) 
library(raster)
library(maptools)
library(RStoolbox)
library(rgeos)
library(parallel)
#load MNF
MNF <- stack(file.choose())
MNF <- dropLayer(MNF, c(5,6))
names(MNF) <- sapply(1:dim(MNF)[3], function(i) paste("MNF", i, sep="."))
rasterlist <- list(raster(MNF, layer = 1), raster(MNF, layer = 2), 
                   raster(MNF, layer = 3), raster(MNF, layer = 4))
#load Reflectance
Reflectance <- stack(file.choose())
#load thermal bands
thermal <- stack(file.choose())
#compute related ratios
ironoxid <- raster(Reflectance, layer=4)/raster(Reflectance, layer=2)
clay  <- raster(Reflectance, layer=6)/raster(Reflectance, layer=7)
ratio3 <- raster(Reflectance, layer=7)/raster(Reflectance, layer=3)
ratio4 <- raster(Reflectance, layer=2)/raster(Reflectance, layer=3)
quartz <- raster(thermal, layer=1) #first thermal band (band 10)
#last ratio have different extent
#crop
quartz <- crop(quartz, ratio4)


  
ratioList <- list(ironoxid, clay, ratio3, ratio4, quartz)
#stack first four MNF layers and ratios
toclass <- stack(c(rasterlist, ratioList))

#extract 
values <- sampleRandom(toclass, 10000, na.rm=T)
##when load from disk
#values <- read.csv(file.choose())
# values <- values[,-1]
##a backup
valuesBackup <- values
##No need for ID
values <- values[,-2]
#check fot too many NA
NAs <- sapply(1:dim(toclass2)[3],function(i) sum(is.na(getValues(subset(toclass2,i)))))
#convert inf to NA for the model to work
for (i in 1:dim(values)[1]){
  for (j in 1:dim(values)[2]){
    if (is.infinite(values[i,j])){values[i,j] <- NA}
  }
}
#fill NAs
##model
Nafill <- preProcess(values, method = "bagImpute")
##fill
valuesNAfilled <- predict(Nafill, values)
##check
sum(is.na(valuesNAfilled))

#normalize the data frame to have comparable data
normal <- preProcess(valuesNAfilled[,-1])
valuesNormal <- predict(normal, valuesNAfilled[,-1])
distances <- dist(valuesNormal)
clus <- hclust(distances)
plot(clus)
#
cut <- cutree(clus, h=5)
table(Class, cut)


#train RF model
train_control <- trainControl(method="cv", number=10)

system.time(
modelrf <- train(valuesNAfilled[,-1], factor(cut), trControl=train_control, method = "rf")
)

system.time(
  predraster <- predict(toclass2, modelrf, 
                        na.rm=T,inf.rm = TRUE)
)

predraster <- calc(predraster, as.integer)
writeRaster(predraster, file.choose())
predshape <- rasterToPolygons(predraster)
