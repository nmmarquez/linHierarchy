#' Evaluates David adjusted the probability of Winning
#' 
#' Given two players in interaction data finds the David adjusted percentage for
#' winning of the first player over of the seconed as per Gammel et al. 2003.
#' @param intData an object of the class "interData"
#' @return Numeric value reflecting the percentage of time pi beat pj where
#' there was a clear winner (i.e. no tie) adjusted for chance.
#' @description Calculates the percentage of games player i (pi) won over 
#' player j (pj) across the interMat object adjusted fpr chance. Ties are
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
#' # find Dij betweeen players in group
#' Dij (toInterMat (id1))
#' @export

Dij <- function (intData){
    idError (intData)
    intMat <- toInterMat (intData)
    Nij <- t(intMat) + intMat
    pMat <- intMat/Nij - ((intMat/Nij - .5)/ (Nij + 1))
    pMat [is.na (pMat)] <- 0
    pMat
}