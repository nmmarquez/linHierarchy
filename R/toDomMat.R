#' Convert interData object to dominance matrix
#' 
#' Creates a matrix cataloging 'dominance' between players in
#' an interData object.
#' @param intData object of class "interData" to build the matrix
#' @return a matrix indicating player wins
#' @details toDomMat creates a matrix which uses players from
#' intData as row and column names. A cell is given a value of 1 if the player
#' in the row is dominant to the player in the column, a value of .5 if there 
#' was an equal number of win-loss interactions and NA for all other instances. 
#' Because this matrix is used in I&SI calulations as outlined by Han de Vries
#' 1998 draws are ignored.
#' @return a dominance matrix
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # calculate dominance matrix using all players
#' toDomMat (id1)
#' @references Han de Vries (1998) Finding a Dominance Order Most Consistant
#' with a Linear Hierarchy. 
#' @export

toDomMat <- function (intData){
    idError (intData)
    
    Pmat <- Pij (toInterMat (intData))
    
    Pmat [Pmat != .5] <- round (Pmat [Pmat != .5])
    
    Pmat
}