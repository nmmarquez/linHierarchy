#' Calculate David's score 'w2' or 'l2' score
#' 
#' Find the 'w2' or 'l2' score of the David's score algorithim for the
#' associated player defined in the player argument.
#' @param player in the "interData" object of which to find the 'w' parameter
#' @param intData an object of class "interData" which contains the players 
#' interactions.
#' @param param either the single character w or l to calculate the
#' w2 or l2 parmeter respectively.
#' @description Calculates the w2 or l2 score of the Davids score
#' algorithim. w2 is calculated as the sum of all individuals Pij percentages
#' where i is the individual for which w is associated and and j is all other
#' players in the interData object weighted by player j's w score. l2 is the
#' sum of all (1 - Pij) weighted by player j's l score. Ties are disregarded
#' in this calculation as per the methods described in Gammel et al. 2003
#' @references Gammel et al. (2003) David's Score. Animal Behaviour
#' @return Either w2 or l2 as described above. 
#' @examples 
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # find w2 of player in group
#' calcWL2 (id1$players [1], id1)
#' # find l2 of player in group
#' calcWL2 (id1$players [1], id1, 'l')
#' @export

calcWL2 <- function (player, intData, param = 'w'){
    if (!(player %in% intData [['players']])){
        stop ('Player not in interData object.')
    }
    else if (class (intData) != 'interData'){
        stop ('intData argument must be of the class "interData"')
    }
    else if (!(param == 'w' | param == 'l')){
        stop ('The param argument must either be "w" or "l"')
    }
    
    opp <- intData [['players']] [intData [['players']] != player]
    
    if (param == 'w'){
        sum (sapply (opp, function (x) Pij (player, x, intData) * 
                         calcWL (x, intData)))
    }
    
    else if (param == 'l'){
        sum (sapply (opp, function (x) (1 - Pij (player, x, intData)) * 
                         calcWL (x, intData, 'l')))
    }
}