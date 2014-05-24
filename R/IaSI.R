IaSI <- function (domMat, max.iter = 1000){
    pairs <- t(combn (row.names (domMat), 2)); curMat <- domMat; t <- 0
    I <- inconsis (curMat); SI <- SI (curMat)
    
    for (i in 1:nrow (pairs)){
        pi <- pairs [i,1]; pj <- pairs [i,2]
        ri <- match (pi, colnames (curMat)); rj <- match (pj, colnames (curMat))
        srows <- sort (c(ri,rj))
        if (!is.na (curMat [srows [2], srows [1]]) & 
                curMat [srows [2], srows [1]] == 1){
            
        }
    }
} 