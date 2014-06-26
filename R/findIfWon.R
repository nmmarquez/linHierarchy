#' Find if player won in interaction
#' 
#' Given a row of an interData object determines if the player in question was
#' the winner.
#' 
#' @param player a player from the row in the interData object
#' @param intData an object of class "interData" from which an interaction is
#' to be evaluated
#' @param row the row of the interData data frame to evaluate
#' @details The function looks at an individual row of a data frame from an
#' interData object and returns a logical of wether or not the player was
#' victorious. Will return NA if player is not in row
#' @return logical indicating the wether the player won or not
#' @export

findIfWon <- function (player, intData, rows = 1:nrow (intData$interactions)){
    idError (intData); plyrError (player, intData)
    inter <- intData$interactions; final <- vector()
    
    for (r in rows){
        if (player == inter [r,1]){final <- c(final, inter [r,3] == 1)}
        else if (player == inter [r,2]){final <- c(final, inter [r,3] == -1)}
        else {final <- c(final, NA)}
    }
    
    final
}