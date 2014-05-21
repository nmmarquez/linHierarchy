#' Calculate David's 'l' score
#' 
#' Find the 'l' score of the David's score algorithim for the associated player
#' defined in the player argument.
#' @param player in the "interData" object of which to find the 'l' score
#' @param intData an object of class "interData" which contains the players 
#' interactions.
#' @description Calculates the l score of the Davids score algorithim.
#' l is calculated as the sum of all individuals (1- Pij) percentages where i is
#' the individual for which l is associated and and j is all other players in
#' the interData object. Ties are disregarded in this calculation as per the
#' methods described in Gammel et al. 2003
#' @references Gammel et al. (2003) David's Score. Animal Behaviour
#' @return Numeric value l.
#' @examples 
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # find l of player in group
#' calcL (id1$players [1], id1)
#' @export
#' 
calcL <- function (player, intData){
    idError (intData); plyrError (player, intData)
    
    opp <- intData [['players']] [intData [['players']] != player]
    
    sum (sapply (opp, function (x) Pij (x, player, intData)))
}