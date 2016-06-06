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

system.time(
reflectance1values <- getValues(reflectance1)
)
reflectance1values <- as.data.frame(reflectance1values)

classesNo <- 3:8
system.time(
for (i in classesNo){
        predraster <- kmeanRaster(image = image, imagevalues = reflectance1values, i)
        writeRaster(predraster,
                    paste(savepath,i,"kmeanof1.tif", sep=""))
        print(paste(floor((i-classesNo[1]+1)*100/length(classesNo)), "%", sep = ""))
        
})