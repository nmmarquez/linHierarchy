#' Calculate the Steepness of Davids Score Ranking
#' 
#' Using the methods outlined in de Vries et al 2006, calculates the steepness
#' of a David's Score rating as well as uses simulation in order to estimate a
#' 'p-value' for the probablility of the group being organized in a linear 
#' organization (alternate hypothesis). 
#' @param intData Object of class int Data from which to derive and test the 
#' hierarchy.
#' @param iter The number of simulations to run in order to estimate a p-value.
#' @param corrected Wether to use the corrected method for Davids score.
#' @details The steep test creates a test for steepness by simuating possible
#' outcomes between dyads given their set number of interactions. This process 
#' is outlined in detail in de Vries et al 2006. 
#' @references de Vries et al (2006). Measuring and testing the steepness of 
#' dominance hierarchies. Animal Behaviour.
#' @return A numeric vector of length two indicating the steepness and the 
#' p-value found from simulation for the steepness.
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # calculate David's Score
#' davidScore (id1)
#' # calculate steepness and p-value
#' steepTest (id1)
#' @export
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
    
    steep <- lm ((DS (f1 (intData)) + maxDS)/np ~ numPlyrs)$coefficients [2]
    emp.mat.list <- replicate (iter, iMat, FALSE)
    mat.list <- lapply (emp.mat.list, new.Rmat)
    ds.list <- lapply (mat.list, function (x) (DS (x) + maxDS)/np)
    coeffs <- unlist (lapply (ds.list,
                              function (x) lm (x ~ numPlyrs)$coefficients [2]))
    final <- c (steep, sum (coeffs > steep)/length (coeffs))
    names (final) <- c('steepness', 'p-value')
    final
}