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
#input data
soilurban <- raster(file.choose())
soilroof <- raster(file.choose())
soilurban <- calc(soilurban, as.integer)
#values over 5 or 6 are not soil so convert them to 1 and soils to zero
soilurban <- soilurban %/% 6
soilroof <- soilroof %/% 5

#now that in both rasters, soils are 0 and urbans are 1 make a function that inion soil pixels
sumNA <- function(a){
        #if (sum(is.na(a)) >= 2) return(NA)
        if (!is.na(sum(a))){if (sum(a) > 1) return(NA) else return(1)}
        if (!is.na(a[1])) {if (a[1] > 0) return(NA) else return(1)}
        if (!is.na(a[2])) {if (a[2] > 0) return(NA) else return(1)}
        else return(NA)
}
#use function above to produce soil raster (i.e 1 at soil pixels and NA elswhere)
soil <- calc(stack(soilurban, soilroof), sumNA)
writeRaster(soil,file.choose())
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
toclass <- stack(c(rasterlist, ratioList)) * soil
#mask by soil
# toclass2 <- toclass * soil

MNFvalues <- getValues(toclass)
MNFvalues <- as.data.frame(MNFvalues)
year <- 2000
classesNo <- 3:6
for (i in classesNo){
        system.time(
                clus <- kmeans(na.omit(MNFvalues), i, iter.max = 100, nstart = 10)
        )
        rNA <- setValues(raster(MNF), 0)
        for (j in 1:nlayers(MNF)){
                rNA[is.na(MNF[[j]])] <- 1
        }
        rNA <- getValues(rNA)
        MNFvalues$Class[rNA ==0] <- clus$cluster
        MNFvalues$Class[rNA ==1] <- NA
        predraster <- raster(MNF)
        predraster <- setValues(predraster, MNFvalues$Class)
        # writeRaster(predraster, 
        #             paste("C:\\Reza\\kmean classification\\Mining Borders\\Mining_Borders_", year,"_",i,"class_kmean-total.tif", sep=""))
        writeRaster(predraster, 
                    paste("C:\\Reza\\kmean classification\\Main Image\\","Classification", year,"_",i,"classes_kmean_MNF.tif", sep=""))
        print(paste(floor((i-classesNo[1]+1)*100/length(classesNo)), "%", sep = ""))
        
}
