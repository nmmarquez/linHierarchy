#' Convert interData object to interaction matrix
#' 
#' Creates a matrix cataloging the wins between players in
#' an interData object.
#' @param intData object of class "interData" to build the matrix
#' @details toInterMat creates a matrix which uses players from
#' intData as row and column names. Each cell is given an integer value
#' indicating the number of times the player indicated in the row won a dyadic
#' interaction against the player indicted in the column. Ties are ignored for
#' this calculation.
#' @return a matrix of player wins
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # calculate using all players
#' toInterMat (id1)
#' @export

toInterMat <- function (intData){
    idError (intData)
    
    players <- intData$players; wdf <- intData$interactions
    x <- matrix (0, length (players), length (players), 
                 dimnames = list (players, players))
    
    for (i in 1:nrow (wdf)){
        p1 <- wdf [i,'player.1']; p2 <- wdf [i,'player.2']
        
        if (wdf [i,'outcome'] == 1){
            x [p1,p2] <- x[p1,p2] + 1
        }
        
        else if (wdf [i,'outcome'] == -1){
            x [p2,p1] <- x[p2,p1] + 1
        }
    }
    return (x)
}