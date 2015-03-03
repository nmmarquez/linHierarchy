#' Convert a standard data frame to an interData object
#' 
#' The function takes a dataframe with either 4 or 5 columns and converts it to
#' an interData object to be used for analysis in functions in this package.
#' @param df data frame with 4 columns as described in the details.
#' @param ... arguments to be passed to the as.POSIXct function for
#' converting time data
#' @details The columns of the data frame being passed to this function should
#' be as follows:
#' 
#' - column 1: a vector with the 1st participant in a dyadic interaction/game
#' 
#' - column 2: a vector with the 2nd participant in a dyadic interaction/game
#' 
#' - column 3: the outcome of the interaction coded as either 
#' 
#'     >  1 if player 1 was the winner
#'     
#'     > -1 if player 2 was the winner
#'     
#'     >  0 if the outcome was a tie
#'     
#' - column 4: a vector to be coerced to the class POSIXct by the as.POSIXct function
#'  denoting the time of the interaction/game
#' @return a list like object of class interData with three elements
#' - element 1: character of players in the interactions  
#' - element 2: range of time that the data frame encompasses  
#' - element 3: data frame with the players interactions, outcome, and date
#' @note Interactions where player.1 is equal to player.2 are automatically
#' removed as no algorithm takes into account what to do when a player plays
#' against themself.
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' intTableConv (interactions)
#' @export

intTableConv <- function (df, ...){
    df[,1] <- as.character(df[,1]); df[,2] <- as.character(df[,2])
    df.int <- df [df [,1] != df [,2],1:4]
    names (df.int) <- c("player.1", "player.2", "outcome", "datetime")
    df.int [,1] <- as.character (df.int [,1])
    df.int [,2] <- as.character (df.int [,2])
    df.int [,4] <- as.POSIXct (df.int [,4], ...)
    df.int <- na.omit (df.int); N <- nrow (df.int)
    
    if (!all (sapply (df.int [,3], function (x) abs (x) == 1 | x == 0))){
        warning ("Outcomes are not formatted properly")
    }
    
    df.int <- df.int [order (df.int [,4], runif (N)),]
    if (N > 0) {row.names (df.int) <- 1:N}
    players <- unique (c(df.int$player.1, df.int$player.2))
    intList <- suppressWarnings (list (players = as.character (players), 
                                       datetime = c(sort(df.int$datetime) [1],
                                                    sort(df.int$datetime,T)[1]), 
                                       interactions = df.int))
    names (intList [['datetime']]) <- c('start', 'end')
    class (intList) <- "interData"
    return (intList)
}