#takes an image and set of training plygons. 
#Copmute texture for bands 2:4. Do the randomForest 
#classification on the image taking into account texture 
#analysis too
library(caret)
library(randomForest)
library(e1071) 
library(raster)
library(maptools)
library(RStoolbox)
library(rgeos)
#get the training polygon shapefile
print("enter the polygom trainig data")
xx <- readShapePoly( file.choose())#, proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
#get the image to classify
print("enter raster for classification")
image <- brick(file.choose())
projection(xx) <- projection(image)
library(glcm)
library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1
rasterlist <- list(raster(image, layer = 2), raster(image, layer = 3), raster(image, layer = 4))
# Initiate cluster
Sys.time()
cl <- makeCluster(no_cores)
clusterExport(cl,c("image", "texture"))
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(glcm))
Sys.time()
texlist <- parLapply(cl, rasterlist, glcm)
Sys.time()
stopCluster(cl)
toClass <- stack(c(rasterlist, texlist))
Sys.time()


train <- extract(toClass, xx, df = TRUE)
train2 <- train 
train <- cbind(train, xx@data$Class[train$ID])
train <- train[complete.cases(train),]
row.inf <- apply(train, 1, function(x) any(is.infinite(x)))
train <- train[!row.inf,]
Sys.time()
model <- randomForest(x = train[,names(train)[3:(dim(toClass)[3]+2)]],
                      y = as.factor(train[, ncol(train)]),
                      ntree=501, importance=TRUE)

library(snow)

#beginCluster(no_cores)
#classification <- clusterR(toClass, predict, args=list(model), filename = "D:/temp/classificationtexture.tif")
#endCluster()
Sys.time()
classification <- predict(toClass, model, filename = "D:/temp/classificationtexture1000trees.tif")
Sys.time()