image <- brick(file.choose())
predraster <- raster(file.choose())
savepath <- "C:\\Reza\\kmean classification\\Main Image\\06-07-2011\\"

# it seems that 1 is soil and sometimes settelment and 
# 2 is settelment and sometimes soil

#mask values of 1
mask1 <-  setValues(raster(predraster), 1)
mask1[getValues(predraster) != 1 | is.na(getValues(predraster))] <- NA

#mask values of 2
mask2 <-  setValues(raster(predraster), 1)
mask2[getValues(predraster) != 2 | is.na(getValues(predraster))] <- NA

reflectance1 <- mask(image, mask1)
reflectance2 <- mask(image, mask2)

# #load thermal bands
# thermal <- stack(file.choose())
# thermal <- crop(thermal, reflectace1)
#compute related ratios
ironoxid <- raster(reflectance1, 3)/ raster(reflectance1, 1)
saprolite <- raster(reflectance1, 5)/ raster(reflectance1, 4)
clay <- raster(reflectance1, 5)/ raster(reflectance1, 6)
ferros <- raster(reflectance1, 4)/ raster(reflectance1, 2)
settelment <- raster(reflectance1, 3)/ raster(reflectance1, 5)


ratioList <- list(ironoxid, saprolite, clay, ferros, settelment)

##to be continiued#############


system.time(
reflectance1values <- getValues(reflectance1)
)
reflectance1values <- as.data.frame(reflectance1values)

classesNo <- 9:10
system.time(
for (i in classesNo){
        predraster <- kmeanRaster(image = reflectance1, imagevalues = reflectance1values, i)
        writeRaster(predraster,
                    paste(savepath,i,"Classes-kmeanof1.tif", sep=""))
        print(paste(floor((i-classesNo[1]+1)*100/length(classesNo)), "%", sep = ""))
        
})