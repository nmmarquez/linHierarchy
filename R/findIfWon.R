#' Find if subject won in interaction
#' 
#' Given a row of an interData object determines if the subject in question was
#' the winner.
#' 
#' @param subject a player from the row in the interData object
#' @param intData an object of class "interData" from which an interaction is
#' to be evaluated
#' @param row the row of the interData data frame to evaluate
#' @details The function looks at an individual row of a data frame from an
#' interData object and returns a logical of wether or not the subject was
#' victorious.
#' @return logical indicating the wether the player won or not
#' @export

findIfWon <- function (subject, intData, row){
    intDF <- intData [['interactions']]
    if (subject == intDF [row, 1]){
        return (intDF [row, 3] == 1)
    }
    else if (subject == intDF [row, 2]){
        return (intDF [row, 3] == -1)
    }
    else{
        return (NA)
    }
}