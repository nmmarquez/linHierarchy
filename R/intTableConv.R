#' Convert a standard data frame to an interData object
#' 
#' The function takes a dataframe with either 4 or 5 columns and converts it to
#' an interData object to be used for analysis in functions in this package.
#' @param df data frame with 4 columns as described in the details.
#' @param ... further arguments to be passed to or from other methods.
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
#' - column 4: a vector to be coerced to the class POSIXct denoting the time of
#' the interaction/game
#' @return an object of class interData
#' @export

intTableConv <- function (df, ...){
    df.int <- df [,1:4]
    names (df.int) <- c("player.1", "player.2", "outcome", "datetime")
    df.int [,1] <- as.character (df.int [,1])
    df.int [,2] <- as.character (df.int [,2])
    df.int [,4] <- as.POSIXct (df.int [,4], ...)
    if (!all (sapply (df.int [,3], function (x) abs (x) == 1 | x == 0))){
        warning ("Outcomes are not formatted properly")
    }
    df.int <- df.int [order (df.int [,4]),]
    players <- unique (c(df.int$player.1, df.int$player.2))
    intList <- list (players = players, interactions = df.int)
    class (intList) <- "interData"
    intList
}