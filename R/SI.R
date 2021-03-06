#' Totals the strength of inconsitancies of a Dominance Matrix
#' 
#' Takes the total number of inconsistancies of a Matrix andd sums thier 
#' 'stregnths'.
#' @param domMat matrix populated by methods described in Hans de Vries 1998.
#' @return numeric of length 1 totaling the strength of inconsistancies in a 
#' matrix.
#' @details Calculates the strength of inconistancies in a dominance matrix
#' using the methods outlined in Han de Vries 1998.
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
#' # get inconsitancy number
#' inconsis (dm1)
#' # get strength of inconsistancies
#' SI (dm1)
#' @references Han de Vries (1998) Finding a Dominance Order Most Consistant
#' with a Linear Hierarchy.
#' @export

SI <- function (domMat){
    idError (domMat, 'matrix')
    
    idf <- which (lower.tri (domMat) & domMat == 1, arr.ind = T)
    
    sum (idf [,'row'] - idf [,'col'])
}