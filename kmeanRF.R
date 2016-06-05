kmeanRF <- function(image, class.no, values){
        values <- as.data.frame(values)
        #clustering wit kmean without normalization
        kclus <- kmeans(values, class.no) 
        cut <- kclus$cluster
        #train RF model
        train_control <- trainControl(method="cv", number=10)
        modelrf <- train(values, factor(cut), trControl=train_control, method = "rf")
        print(modelrf$results)
        predraster <- predict(image, modelrf, 
                              na.rm=T,inf.rm = TRUE)
        predraster <- calc(predraster, as.integer)
        predraster
}