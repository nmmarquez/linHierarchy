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