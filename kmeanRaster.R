kmeanRaster <- function(image, imagevalues,no.classes){
        clus <- kmeans(na.omit(imagevalues), i, iter.max = 100, nstart = 10)
        rNA <- setValues(raster(image), 0)
        for (j in 1:nlayers(image)){
                rNA[is.na(image[[j]])] <- 1
        }
        rNA <- getValues(rNA)
        imagevalues$Class[rNA ==0] <- clus$cluster
        imagevalues$Class[rNA ==1] <- NA
        predraster <- raster(image)
        predraster <- setValues(predraster, MNFvalues$Class)
        predraster
}
