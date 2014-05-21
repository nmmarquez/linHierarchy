#' Count the number of interactions
#' 
#' Find the number of interactions occuring between two players in an interData
#' object
#' @param p1 a player in the interData object
#' @param p2 a player in the interData object
#' @param intData an object of the class "interData" with players p1 and p2
#' @param includeDraws logical indicating wether to include draws or not
#' @param showInteractions logical indicating wether to return a data frame
#' showing the interactions. Defaults to showing the number of interactions.
#' @return Either the number of interactions between p1 and p2 in intData or a 
#' dataframe displaying those interactions
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # calculate using all players
#' numInt (id1$players [1], id1$players [2], id1)
#' @export

numInt <- function (p1, p2, intData, includeDraws = F, showInteractions = F){
    idError (intData); plyrError (c(p1, p2), intData)
    ints <- subset (intData, players = c(p1, p2))$interactions
    
    if (includeDraws){
        if (showInteractions) {return (ints)}
        else {return (nrow (ints))}
    }
    
    else if (!includeDraws){
        if (showInteractions) {return (ints [ints$outcome != 0,])}
        else {return (nrow (ints [ints$outcome != 0,]))}
    }
}