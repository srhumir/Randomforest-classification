filNA <- function(train, Class, ID){
       means <- tapply(train, Class, colMeans, na.rm = TRUE)
       row.has.na <- apply(train, 1, function(x){any(is.na(x))})
       l <- apply(train[row.has.na,], 1, function(x) which(is.na(x)))
       
}