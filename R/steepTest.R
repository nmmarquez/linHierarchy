steepTest <- function (intData, iter = 2000, corrected = FALSE){
    iMat <- toInterMat (intData); numPlyrs <- 1:nrow(iMat); np <- nrow(iMat)
    Nij <- t(iMat) [upper.tri (t(iMat))] + iMat [upper.tri (iMat)]
    maxDS <- np * ((np - 1)/2)
    
    if (corrected){
        f1 <- Dij; f2 <- function (Rij){Rij/Nij - (((Rij/Nij) - .5)/(Nij + 1))}
    }
    else{
        f1 <- Pij; f2 <- function (Rij) {Rij/Nij}
    }
    
    new.Rmat <- function (mat){
        new <- mat
        Rij <- sapply (Nij, function (x) sample (0:x,1))
        repl <- f2 (Rij)
        new [upper.tri (new)] <- repl
        new <- t(new)
        new [upper.tri (new)] <- 1 - repl
        new [is.na (new)] <- 0
        t (new)
    }
    
    DS <- function (mat){
        w <- rowSums (mat); l <- colSums (mat)
        return (sort (mat %*% w - t (t(l) %*% mat) + w - l))
    }
    
    steep <- lm ((DS (f1 (iMat)) + maxDS)/np ~ numPlyrs)$coefficients [2]
    emp.mat.list <- replicate (iter, iMat, FALSE)
    mat.list <- lapply (emp.mat.list, new.Rmat)
    ds.list <- lapply (mat.list, function (x) (DS (x) + maxDS)/np)
    coeffs <- unlist (lapply (ds.list,
                              function (x) lm (x ~ numPlyrs)$coefficients [2]))
    final <- c (steep, sum (coeffs > steep)/length (coeffs))
    names (final) <- c('steepness', 'p-value')
    final
}