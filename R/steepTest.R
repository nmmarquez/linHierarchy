steepTest <- function (intData, iter = 2000, corrected = FALSE){
    iMat <- toInterMat (intData); numPlyrs <- 1:length (intData$players)
    Nij <- t(iMat) [upper.tri (t(iMat))] + iMat [upper.tri (iMat)]
    
    new.Rmat <- function (mat){
        new <- mat
        Rij <- sapply (Nij, function (x) sample (0:x,1))
        
        if (corrected) {repl <- Rij/Nij - (((Rij/Nij) - .5) / (Nij + 1))}
        else {repl <- Rij/Nij}
        
        new [upper.tri (new)] <- Rij/Nij
        new <- t(new)
        new [upper.tri (new)] <- (Nij - Rij)/Nij
        new [is.na (new)] <- 0
        new
    }
    
    DS <- function (mat){
        w <- rowSums (mat); l <- colSums (mat)
        return (sort (mat %*% w - t (t(l) %*% mat) + w - l))
    }
    
    emp.mat.list <- replicate (iter, iMat, FALSE)
    mat.list <- lapply (emp.mat.list, new.Rmat)
    ds.list <- lapply (mat.list, DS)
    coeffs <- unlist (lapply (ds.list,
                              function (x) lm (x ~ numPlyrs)$coefficients [2]))
    sum (coeffs > )
}