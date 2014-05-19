#' Calculate David's score 'w' paramter
#' 
#' Find the 'w' paramter of the David's score algorithim for the associated
#' player defined in the player argument
#' @param player in the "interData" object of which to find the 'w' parameter
#' @param intData an object of class "interData" which contains the players 
#' interactions.
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