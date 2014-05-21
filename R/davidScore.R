#' Calculate the David's Score of players
#' 
#' Calculates the David's Score of players in an "interData" object.
#' @param intData object of class "interData" to calculate scores.
#' @details Using the methods outlined in Gamel et al. 2003 a David's score is 
#' calculated using interactions from intData. Adjusting the players parameter
#' will modify the algorithm so that only interactions that involve the players
#' specified will be used.
#' @return A 2 column data frame specifying the players used in the algorithm 
#' sorted by their corresponding david's score.
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
#' @references Gammel et al. (2003) David's Score. Animal Behaviour
#' @export

davidScore <- function (intData){
    idError (intData)
    
    DS <- data.frame (players = intData$players, score = as.numeric (NA),
                      stringsAsFactors = F)
    
    DS$score <- sapply (DS$players, function (x) calcW (x, intData) + 
                calcWL2 (x, intData) - calcL (x, intData) - 
                calcWL2 (x, intData, 'l'))
    
    DS <- DS [order (-DS$score),]; row.names (DS) <- 1:nrow(DS)
    
    DS
}