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
names(MNF) <- sapply(1:nlayers(MNF), function(i) paste("MNF", i, sep="."))
#extract values to data.frame
MNFvalues <- getValues(MNF)
MNFvalues <- as.data.frame(MNFvalues)
year <- 2007
for (i in 2:6){
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
        writeRaster(predraster, 
                    paste("C:\\Reza\\kmean classification\\Mining Borders\\Mining_Borders_", year,"_",i,"class_kmean-total.tif", sep=""))
        print(paste((i-1)*100/5, "%", sep = ""))
        
}
