#' Evaluates the probability of Winning
#' 
#' Given two players in interaction data finds the percentage for
#' winning of the first player over of the seconed.
#' @param pi player to find win percentage of
#' @param pj opposng player
#' @param intData object of class "interData" where pi and pj are players
#' @return Numeric value reflecting the percentage of time pi beat pj where
#' there was a clear winner. (i.e. no tie)
#' @export

Pij <- function (pi, pj, intData){
    if (!(pi %in% intData [[1]]) | !(pj %in% intData [[1]])){
        stop ('Either Player 1 or 2 not in interData Object')
    }
    if (pi == pj){
        stop ('Player 1 can not be the same as Player 2')
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