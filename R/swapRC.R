#' Swap the rows and columns of a dominance matrix
#' 
#' Rearranges the position of individuals in a dominance matrix in an attempt to
#' minimize eithier inconsistancies or iconsistancy stregnth as per the methods 
#' outlined in Han de Vries 1998.
#' @param domMat matrix populated by methods described in Hans de Vries 1998.
#' @param rc1 a row/column in domMat to be swapped
#' @param rc2 a row/column in domMat to be swapped
#' @details When the individuals of the matrix are swapped the names of the
#' matrix are also swapped in order to ensure consistancy of naming.
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # calculate dominance matrix using all players
#' dm1 <- toDomMat (id1)
#' # original dominance matrix
#' dm1
#' # swap individual 1 and 2
#' swapRC (dm1, 1, 2)
#' @references Han de Vries (1998) Finding a Dominance Order Most Consistant
#' with a Linear Hierarchy.
#' @export

swapRC <- function (domMat, rc1, rc2){
    new <- domMat
    
    new [c(rc2, rc1),] <- domMat [c(rc1, rc2),]
    new [,c(rc2, rc1)] <- new [,c(rc1, rc2)]
    
    row.names (new) [c(rc2, rc1)] <- row.names (domMat) [c(rc1, rc2)]
    colnames (new) [c(rc2, rc1)] <- colnames (domMat) [c(rc1, rc2)]
    
    new
}