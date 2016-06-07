#get the classification of the whole image 
# For 06.07.2011 I used 
# Classification06-07-2011_8classes_kmean_Reflectance.tif
##gatehr togheter Dry land, green land and Wet land
##classify each of classes to subclasse. Necessaruly use NDBaI etc
##this codes are just for Green lands
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
# DryLandInd <- c(2,6)
# WetlandInd <- c(3)
# CloudInd <- c(7)
#input data
Classification <- raster(file.choose())
image <- brick(file.choose())
# MNF <- brick(file.choose())

#create masks
GreenLandMask <- setValues(raster(Classification),1)
GreenLandMask[!getValues(Classification) %in% GreenLandInd] <- NA


toclassgreen <- mask(image, GreenLandMask)
system.time(
        values <- sampleRandom(toclassgreen, 10000, na.rm=T)
)
values <- as.data.frame(values)
savepath <- "C:\\Reza\\kmean classification\\Main Image"
year <- "06-07-2011"
targetdir <- paste(savepath, "\\", year,"\\Greenland", sep="")
classesNo <- 3:8
system.time(
for (i in classesNo){
        predraster <- kmeanRF(toclassgreen, i, values)
        if (!dir.exists(targetdir)){
                dir.create(targetdir)
        }
        writeRaster(predraster,
                    paste(targetdir, "\\","Classification_Greenlands","_",i,"classes_kmean_Reflectance.tif", sep=""))
        print(paste(floor((i-classesNo[1]+1)*100/length(classesNo)), "%", sep = ""))
})

#######################supervised
#load training data
xx <- readShapePoly( file.choose())
projection(xx) <- projection(toclassgreen)

#extract values on training data
system.time(
        values <- extract(toclassgreen, xx, df = TRUE)
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
#subset values to have really seperate classes
values <- 
##No need for ID
values <- values[,-2]
## keep class seperate for speeding up the training and ...
Class <- factor(values$Class)
# #convert inf to NA for the model to work
# for (i in 1:dim(values)[1]){
#         for (j in 1:dim(values)[2]){
#                 if (is.infinite(values[i,j])){values[i,j] <- NA}
#         }
# }
# #fill NAs
# ##model
# Nafill <- preProcess(values[-1], method = "bagImpute")
# ##fill
# valuesNAfilled <- predict(Nafill, values[-1])
# ##check
# sum(is.na(valuesNAfilled))
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
        predraster <- predict(toclassgreen, modelRF,
                              filename = paste(targetdir, "\\","Classification_greenlands","_",3,"classes_SuperRF_Reflectance.tif", sep=""),
                              na.rm=T,inf.rm = TRUE)
)


##########################Final notes
# From Classification_greenlands_3classes_SuperRF_Reflectance.tif
# classes 1,2 are considered forest
# Classes 3 as vegetation
# Belove I will make a raster of Greenlands classification
forestInd <- c(1,2)
vegind <- 3
greenland3classes <- raster(file.choose())
Final.forest <- setValues(raster(greenland3classes), NA)
Final.forest[getValues(greenland3classes) %in% forestInd] <- 9
writeRaster(Final.forest, file.choose())

Final.vegetation <- setValues(raster(greenland3classes), NA)
Final.vegetation[getValues(greenland3classes) %in% vegind] <- 10
writeRaster(Final.vegetation, file.choose())

Final.Greenland <- setValues(raster(greenland3classes), NA)
Final.Greenland <- sum(Final.forest,Final.vegetation, na.rm = T)
writeRaster(Final.Greenland, file.choose())
