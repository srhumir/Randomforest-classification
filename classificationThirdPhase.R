image <- brick(file.choose())
predraster <- raster(file.choose())
savepath <- "C:\\Reza\\kmean classification\\Main Image\\06-07-2011\\"


#mask values of soil-roof
soil.roof <- c(1,2,3)
mask1 <-  setValues(raster(predraster), 1)
mask1[!getValues(predraster) %in%  soil.roof | is.na(getValues(predraster))] <- NA

#mask values of soil-settelment
soil.settel <- c(4,5)
mask2 <-  setValues(raster(predraster), 1)
mask2[!getValues(predraster) %in% soil.settel | is.na(getValues(predraster))] <- NA

reflectance1 <- mask(image, mask1)
reflectance2 <- mask(image, mask2)

# #load thermal bands
# thermal <- stack(file.choose())
# thermal <- crop(thermal, reflectace1)
#compute related ratios
ironoxid <- raster(image, 3)/ raster(image, 1)
saprolite <- raster(image, 5)/ raster(image, 4)
clay <- raster(image, 5)/ raster(image, 6)
ferros <- raster(image, 4)/ raster(image, 2)
settelment <- raster(image, 3)/ raster(image, 5)
ratioList <- list(ironoxid, saprolite, clay, ferros, settelment)

toclassroof <- stack(c(list(reflectace1), ratioList))

system.time(
toclassroofvalues <- getValues(toclassroof)
)
toclassroofvalues <- as.data.frame(toclassroofvalues)

classesNo <- 9:11
system.time(
for (i in classesNo){
        predraster2 <- kmeanRaster(image = toclassroof, 
                                   imagevalues = toclassroofvalues,
                                   no.classes = i)
        writeRaster(predraster2,
                    paste(savepath,i,"Classes-kmean-SoilRoof.tif", sep=""))
        print(paste(floor((i-classesNo[1]+1)*100/length(classesNo)), "%", sep = ""))
        
})

#####################supervised
xx <- readShapePoly( file.choose())
projection(xx) <- projection(toclassdry)

#extract values on training data
system.time(
        values <- extract(toclassroof, xx, df = TRUE)
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
                              filename = paste(savepath,3,"Classes-Supervised-SoilRoof.tif", sep=""),
                              na.rm=T,inf.rm = TRUE)
)



toclasssettel <- stack(c(list(reflectace2), ratioList))

system.time(
        toclasssettelvalues <- getValues(toclasssettel)
)
toclasssettelvalues <- as.data.frame(toclasssettelvalues)

classesNo <- 5:10
system.time(
        for (i in classesNo){
                predraster2 <- kmeanRaster(image = toclasssettel, 
                                           imagevalues = toclasssettelvalues,
                                           no.classes = i)
                writeRaster(predraster2,
                            paste(savepath,i,"Classes-kmean-SoilSettel.tif", sep=""))
                print(paste(floor((i-classesNo[1]+1)*100/length(classesNo)), "%", sep = ""))
                
        })