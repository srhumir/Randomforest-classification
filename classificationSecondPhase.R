cleanup <- function(v){
        v <- as.vector(v)
        if (!(v[5] %in% v[-5])){
            return(as.integer(names(sort(-table(v)))[1]))
        }
        else return(v[5])
}
#get the classification of the whole image 
##gatehr togheter Green land, Dry land and Wet land
##classify each of classes to subclasse. Necessaruly use NDBaI etc

library(caret)
library(randomForest)
library(e1071) 
library(raster)
library(maptools)
library(RStoolbox)
library(rgeos)
library(rgdal)
library(parallel)

GreenLandInd <- c(1,4,5,8)
DryLandInd <- c(2,6)
WetlandInd <- c(3)
CloudInd <- c(7)
#input data
Classification <- raster(file.choose())
image <- brick(file.choose())
MNF <- brick(file.choose())

Classification <- focal(Classification, matrix(nrow = 3, ncol = 3,1), cleanup)
#create masks
GreenLandMask <- setValues(raster(Classification),1)
GreenLandMask[!getValues(Classification) %in% GreenLandInd] <- NA

DryLandMask <- setValues(raster(Classification), 1)
DryLandMask[!getValues(Classification) %in% DryLandInd] <- NA
DryLandMNF <- mask(MNF, DryLandMask)

correlationDry3<- glcm(raster(DryLandMNF,3), n_grey = 32, window = c(3, 3), shift = c(1, 1), statistics = 
             c("correlation"), min_x=NULL, max_x=NULL, na_opt="center", 
     na_val=NA, scale_factor=1, asinteger=FALSE)


