cleanup <- function(v){
        v <- as.vector(v)
        if (!(v[5] %in% v[-5])){
            return(as.integer(names(sort(-table(v)))[1]))
        }
        else return(v[5])
}
#get the classification of the whole image 
# For 06.07.2011 I used 
# Classification06-07-2011_8classes_kmean_Reflectance.tif
##gatehr togheter Dry land and Wet land
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
library(glcm)

GreenLandInd <- c(1,4,5,8)
DryLandInd <- c(2,6)
WetlandInd <- c(3)
CloudInd <- c(7)
#input data
Classification <- raster(file.choose())
image <- brick(file.choose())
MNF <- brick(file.choose())

# Classification <- focal(Classification, matrix(nrow = 3, ncol = 3,1), cleanup)
#create masks
GreenLandMask <- setValues(raster(Classification),1)
GreenLandMask[!getValues(Classification) %in% GreenLandInd] <- NA

DryLandMask <- setValues(raster(Classification), 1)
DryLandMask[!getValues(Classification) %in% DryLandInd] <- NA
DryLandMNF <- mask(MNF, DryLandMask)

statistics <- c("variance", "homogeneity", "contrast", "dissimilarity", "entropy")

no_cores <- detectCores() - 1
##Create a list of first four MNF bands
rasterlist <- list(raster(DryLandMNF, layer = 1), raster(DryLandMNF, layer = 2), 
                   raster(DryLandMNF, layer = 3))
## Initiate cluster

cl <- makeCluster(no_cores)
##Send variable to cluster
clusterExport(cl,c("rasterlist", "statistics"))
##send packages to cluster
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(glcm))
##compute texture
system.time(
        texlist <- parLapply(cl, rasterlist, glcm, n_grey = 32, window = c(3, 3), shift = c(1, 1), 
                             statistics =statistics, min_x=NULL, max_x=NULL, na_opt="center", 
                             na_val=NA, scale_factor=1, asinteger=FALSE)
)
stopCluster(cl)
#

toclassdry <- stack(c(list(image), texlist))
system.time(
        values <- sampleRandom(toclassdry, 10000, na.rm=T)
)
values <- as.data.frame(values)
savepath <- "C:\\Reza\\kmean classification\\Main Image"
year <- "06-07-2011"
targetdir <- paste(savepath, "\\", year, sep="")
classesNo <- 3:8
for (i in classesNo){
        predraster <- kmeanRF(toclassdry, i, values)
if (!dir.exists(targetdir)){
                dir.create(targetdir)
        }
        writeRaster(predraster,
                    paste(targetdir, "\\","Classification_drylands","_",i,"classes_kmean_ReflectanceTexture.tif", sep=""))
        print(paste(floor((i-classesNo[1]+1)*100/length(classesNo)), "%", sep = ""))
}



#was not satisfied with the results. trying supervised RF
#supervised also was not satisfactory decided to add NDBaI 
#load dn
DNlist <- list()
for (i in 1:6){
        DN <- raster(file.choose())
        DN <- crop(DN,toclassdry)
        DNlist[[i]] <- DN
}

DN <- stack(DNlist)
DN6 <- raster(file.choose())
DN6 <- crop(DN6,toclassdry)

NDBaI <- (raster(DN,5) - DN6)/(raster(DN,5) + DN6)
NDBI <- (raster(DN,5) - raster(DN,4))/(raster(DN,5) + raster(DN,4))

#NDBI was not so usefull, so add just NDBaI to the raster stack
toclassdry <- stack(toclassdry, NDBaI)
#compute related ratios
ironoxid <- raster(image, 3)/ raster(image, 1)
saprolite <- raster(image, 5)/ raster(image, 4)
clay <- raster(image, 5)/ raster(image, 6)
ferros <- raster(image, 4)/ raster(image, 2)
settelment <- raster(image, 3)/ raster(image, 5)
ratioList <- list(ironoxid, saprolite, clay, ferros, settelment)

toclassdry <- stack(c(list(toclassdry), ratioList))
# # removing textures and use just ratios
# toclassdry <- dropLayer(toclassdry, 7:21)
# #lets try adding NDBI too
# toclassdry <- stack(toclassdry, NDBI) # made the results worse. too mcuh urban
# toclassdry <- dropLayer(toclassdry, nlayers(toclassdry))



#load training data
xx <- readShapePoly( file.choose())
projection(xx) <- projection(toclassdry)

#extract values on training data
system.time(
        values <- extract(toclassdry, xx, df = TRUE)
)
#take out the attributr table of the trainig and assigne ID to polygons
classes <- data.frame(ID = 1:length(xx@data$CLASS_NAME), xx@data)
#Assign class to extracted values
values <- data.frame(Class = classes$CLASS_NAME[values$ID], values)
##when load from disk
#values <- read.csv(file.choose())
#values <- values[,-1]
#drop rows with all zero MNF
values <- values[values[names(values)[3]] > 0 & !is.na(values[names(values)[3]]),]
##a backup
valuesBackup <- values
##No need for ID
values <- values[,-2]
## keep class seperate for speeding up the training and ...
Class <- factor(values$Class)
#convert inf to NA for the model to work
for (i in 1:dim(values)[1]){
        for (j in 1:dim(values)[2]){
                if (is.infinite(values[i,j])){values[i,j] <- NA}
        }
}
#fill NAs
##model
Nafill <- preProcess(values[-1], method = "bagImpute")
##fill
valuesNAfilled <- predict(Nafill, values[-1])
##check
sum(is.na(valuesNAfilled))
# #Omit roof
# omitindex <- which(Class == levels(Class)[4])
# valuesNAfilled <- valuesNAfilled[-omitindex,]
# Class <- Class[-omitindex]
# define training control
train_control <- trainControl(method="cv", number=10)

#traing RF model
system.time(
        modelRF <- train(valuesNAfilled,droplevels(Class), trControl=train_control,
                         method="rf")
)
modelRF

#predict on raster
system.time(
        predraster <- predict(toclassdry, modelRF,
                              filename = paste(targetdir, "\\","Classification_drylands","_",2,"classes_RF_ReflectanceTextureNDBaIRatiosNoRoof.tif", sep=""),
                              na.rm=T,inf.rm = TRUE)
)

