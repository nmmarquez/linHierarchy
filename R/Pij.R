#' Evaluates the probability of Winning
#' 
#' Given two players in interaction data finds the percentage for
#' winning of the first player over of the seconed.
#' @param intData an object of the class "interData"
#' @return Numeric value reflecting the percentage of time pi beat pj where
#' there was a clear winner. (i.e. no tie)
#' @description Calculates the percentage of games player i (pi) won over 
#' player j (pj) across the interData object. Ties are disregarded in this 
#' calculation as per the methods described in Gammel et al. 2003 and de Vries
#' et al. 2006.
#' @references Gammel et al. (2003) David's Score. Animal Behaviour.  
#' de Vries et al (2006). Measuring and testing the steepness of 
#' dominance hierarchies. Animal Behaviour.
#' @examples 
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # find Pij betweeen players in group
#' Pij (toInterMat (id1))
#' @export

Pij <- function (intData){
    idError (intData)
    intMat <- toInterMat (intData)
    Nij <- t(intMat) + intMat
    pMat <- intMat/Nij
    pMat [is.na (pMat)] <- 0
    pMat
}