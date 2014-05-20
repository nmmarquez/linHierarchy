#' Evaluates the probability of Winning
#' 
#' Given two players in interaction data finds the percentage for
#' winning of the first player over of the seconed.
#' @param pi player to find win percentage of
#' @param pj opposng player
#' @param intData object of class "interData" where pi and pj are players
#' @return Numeric value reflecting the percentage of time pi beat pj where
#' there was a clear winner. (i.e. no tie)
#' @description Calculates the percentage of games player i (pi) won over 
#' player j (pj) across the interData object. Ties are disregarded in this 
#' calculation as per the methods described in Gammel et al. 2003
#' @references Gammel et al. (2003) David's Score. Animal Behaviour
#' @examples 
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # find Pij betweeen two players in group
#' Pij (id1$players [1], id1$players [2], id1)
#' @export

Pij <- function (pi, pj, intData){
    if (!(pi %in% intData [[1]]) | !(pj %in% intData [[1]])){
        stop ('Either Player 1 or 2 not in interData Object')
    }
    if (class (intData) != 'interData'){
        stop ('intData argument must be of the class "interData"')
    }
    intDF <- intData [[2]]
    subRows <- row.names (subset (intDF, (player.1 == pi | player.2 == pi) &
                         (player.1 == pj | player.2 == pj)))
    if (nrow (subset (intDF [subRows,], outcome != 0)) == 0){
        return (0)
    }
    else {
        alpha = sum (sapply (subRows, function(x) findIfWon(pi, intData, x)),
                     na.rm=T)
        n = nrow (subset (intDF [subRows,], outcome != 0))
    }
    alpha/n
}