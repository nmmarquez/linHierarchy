#' Evaluates David adjusted the probability of Winning
#' 
#' Given two players in interaction data finds the David adjusted percentage for
#' winning of the first player over of the seconed as per Gammel et al. 2003.
#' @param pi player to find win percentage of
#' @param pj opposng player
#' @param intData object of class "interData" where pi and pj are players
#' @return Numeric value reflecting the percentage of time pi beat pj where
#' there was a clear winner (i.e. no tie) adjusted for chance.
#' @description Calculates the percentage of games player i (pi) won over 
#' player j (pj) across the interData object adjusted fpr chance. Ties are
#' disregarded in this calculation as per the methods described in Gammel et al.
#' 2003.
#' @references Gammel et al. (2003) David's Score. Animal Behaviour
#' @examples 
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # find Dij betweeen two players in group
#' Dij (id1$players [1], id1$players [2], id1)
#' @export

Dij <- function (pi, pj, intData){
    idError (intData); plyrError (c(pi, pj), intData)
    
    intDij <- subset (intData, players = c(pi,pj))
    
    if (nrow (subset (intDij$interactions, outcome != 0)) == 0){
        return (0)
    }
    
    else {
        alpha <- sum (sapply (1:nrow (intDij$interactions), 
                             function(x) findIfWon(pi, intDij, x)), na.rm=T)
        num.int <- nrow (subset (intDij$interactions, outcome != 0))
    }
    alpha/num.int - (((alpha/num.int) - .5) / (num.int + 1))
}