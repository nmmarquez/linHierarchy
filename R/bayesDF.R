#' Convert interData object to data frame of wins for individuals
#' 
#' Creates a data frame which has a row for each unique pairing of indvividuals
#' in a group and a column specifying the number of first player wins as well as
#' a column for the number of second player wins for that dyad.
#' @param intData object of class "interData" to build the matrix
#' @return a data frame with arow for each dyad in the group indicating number
#' of wins.
#' @details Due to the nature of the construction of the matrix ties amongst
#' dyads are ignored. 
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # calculate using all players
#' toBayesDF (id1)
#' @export

toBayesDF <- function (intData){
    idError (intData)
    
    interMat <- toInterMat (intData)
    bayesDF <- as.data.frame (t (combn (id1$players, 2)))
    
    bayesDF [,3] <- apply (bayesDF, 1, function(x) interMat[x [1], x[2]])
    bayesDF [,4] <- apply (bayesDF, 1, function(x) interMat[x [2], x[1]])
    names (bayesDF) <- c('p1', 'p2', 'win1', 'win2')
    
    bayesDF
}