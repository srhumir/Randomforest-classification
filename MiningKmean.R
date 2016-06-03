#get buffers of probabale mining areas
#cluster them to find the border of mining sites
#Equally can be used to classify th whole image if the system can handle it :)
#This algorithm need to load the whole image into memory.
library(caret)
# library(randomForest)
# library(e1071) 
library(raster)
# library(maptools)
# library(RStoolbox)
# library(rgeos)
# library(rgdal)
# library(parallel)
#load MNF
MNF <- stack(file.choose())
MNF <- dropLayer(MNF, c(5,6))
names(MNF) <- sapply(1:nlayers(MNF), function(i) paste("MNF", i, sep="."))
#extract values to data.frame
MNFvalues <- getValues(MNF)
MNFvalues <- as.data.frame(MNFvalues)
year <- 2007
classesNo <- 3:8
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
        writeRaster(predraster,
                    paste("C:\\Reza\\kmean classification\\Mining Borders2\\Mining_Borders_", year,"_",i,"class_kmean-total.tif", sep=""))
        # writeRaster(predraster, 
        #             paste("C:\\Reza\\kmean classification\\Main Image\\","Classification", year,"_",i,"classes_kmean_MNF.tif", sep=""))
        print(paste(floor((i-classesNo[1]+1)*100/length(classesNo)), "%", sep = ""))
        
}
