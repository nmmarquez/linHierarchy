#' Calculate the David's Score of players
#' 
#' Calculates the David's Score of players in an "interData" object.
#' @param intData object of class "interData" to calculate scores.
#' @param players the players in the interactions to use for calculation 
#' algorithm.
#' @details Using the methods outlined in Gamel et al. 2003 a David's score is 
#' calculated using interactions from intData. Adjusting the players parameter
#' will modify the algorithm so that only interactions that involve the players
#' specified will be used.
#' @return A 2 column data frame specifying the players used in the algorithm 
#' sorted by their correspong david's score.
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # calculate using all players
#' davidScore (id1)
#' # calculate using only the first five players scores
#' davidScore (id1, id1$players [1:5])
#' @references Gammel et al. (2003) David's Score. Animal Behaviour
#' @export

davidScore <- function (intData, players = intData$players){
    idError (intData); plyrError (players, intData)
    
    wdf <- subset (intData$interactions, player.1 %in% players &
                       player.2 %in% players)
    wiD <- intTableConv (wdf)
    
    DS <- data.frame (players = players, score = as.numeric (NA))
    DS$score <- sapply (players, function (x) calcW (x, wiD) + 
                calcWL2 (x, wiD) - calcL (x, wiD) - 
                calcWL2 (x, wiD, 'l'))
    DS <- DS [order (-DS$score),]; row.names (DS) <- 1:nrow(DS)
    DS
}