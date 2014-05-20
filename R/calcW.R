#' Calculate David's score 'w'
#' 
#' Find the 'w' score of the David's score algorithim for the associated player
#' defined in the player argument.
#' @param player in the "interData" object of which to find the 'w' score.
#' @param intData an object of class "interData" which contains the players 
#' interactions.
#' @description Calculates the w score of the Davids score algorithim.
#' w is calculated as the sum of all individuals Pij percentages where i is the
#' individual for which w is associated and and j is all other players in the 
#' interData object.  Ties are disregarded in 
#' this calculation as per the methods described in Gammel et al. 2003
#' @references Gammel et al. (2003) David's Score. Animal Behaviour
#' @return Numeric value w.
#' @examples 
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # find w of player in group
#' calcWL (id1$players [1], id1)
#' @export

calcW <- function (player, intData){
    if (!(player %in% intData [['players']])){
        stop ('Player not in interData object.')
    }
    else if (class (intData) != 'interData'){
        stop ('intData argument must be of the class "interData"')
    }
    
    opp <- intData [['players']] [intData [['players']] != player]
    sum (sapply (opp, function (x) Pij (player, x, intData)))
}