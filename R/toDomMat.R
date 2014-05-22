toDomMat <- function (intData){
    idError (intData); intMat <- toInterMat (intData)
    
    domMat <-intMat; domMat [,] <-NA
    
    com <- as.data.frame (t (combn (colnames (intMat), 2)))
    com [,3] <- apply (com, 1, function (x) Pij (x [1], x[2], intData))
    com [,4] <- apply (com, 1, function (x) Pij (x [2], x[1], intData))
    com <- subset (com, V3 + V4 != 0)
    
    for (i in 1:nrow (com)){
        if (com [i, 3] > com [i, 4]){
            
        }
    }
}