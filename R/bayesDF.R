#' Convert interData object to an expanded data frame
#' 
#' Creates a data frame which has a column for each player in a group 
#' @param intData object of class "interData" to build the matrix
#' @return a data frame with a row for each observation in the group and column
#' for each player where values of 1 and -1 indicate player 1 and player 2 
#' respectively. The outcome variable is the lat column of the data frame where
#' 1 and 0 indicate where player 1 or player 2 won respectively.
#' @details Due to the nature of construction of the data frame ties are 
#' ignored. 
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # calculate using only players who had at least 1 win or loss interaction
#' toBayesDF (id1)
#' @export

toBayesDF <- function (intData){
    idError (intData); noTies <- subset (intData, ties = FALSE)
    df <- noTies$interactions; players <- noTies$players
    
    bayesDF <- as.data.frame (t (sapply (1:nrow (df), function (x)
        (df [x,1] == players) - (df [x,2] == players))))
    names (bayesDF) <- players
    
    bayesDF [,'outcome'] <- df$outcome
    bayesDF [bayesDF$outcome == -1, 'outcome'] <- 0

    bayesDF
}