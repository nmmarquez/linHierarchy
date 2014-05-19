#' Calculate David's score 'w' paramter
#' 
#' Find the 'w' paramter of the David's score algorithim for the associated
#' player defined in the player argument
#' @param player in the "interData" object of which to find the 'w' parameter
#' @param intData an object of class "interData" which contains the players 
#' interactions.
#' @description Calculates the w paramter of the Davids score algorithim. W is
#' calculated as the sum of all individuals Pij percentages where i is the
#' individual for which w is associated and and j is all other players in the 
#' interData object. Ties are disregarded in this 
#' calculation as per the methods described in Gammel et al. 2003
#' @references Gammel et al. (2003) David's Score. Animal Behaviour
#' @return w a sum of individuals Pij percentages.
#' @examples 
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # find W of player in group
#' calcW (id1$players [1], id1)
#' @export

calcW <- function (player, intData){
    if (!(player %in% intData [['players']])){
        stop ('Player not in interData object.')
    }
    if (class (intData) != 'interData'){
        stop ('intData argument must be of the class "interData"')
    }
    opp <- intData [['players']] [intData [['players']] != player]
    sum (sapply (opp, function (x) Pij (player, x, intData)))
}