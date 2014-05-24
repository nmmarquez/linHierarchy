#' Counts the inconsitancies of a Dominance Matrix
#' 
#' Adds the total number of inconsistancies of a dominance matrix ignoring any 
#' dyads where there is a tie.
#' @param domMat matrix populated by methods described in Hans de Vries 1998.
#' @return numeric of length 1 totaling the number of inconsistancies in matrix.
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
#' @references Han de Vries (1998) Finding a Dominance Order Most Consistant
#' with a Linear Hierarchy.
#' @export


inconsis <- function (domMat){
    idError (domMat, 'matrix')
    
    sum (lower.tri (domMat) & domMat == 1, na.rm = T)
}