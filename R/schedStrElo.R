#' Get the schedule strength of players in an Elo Table
#' 
#' The function returns a data frame with the average eloRating of players an 
#' individual has competed against along with the number of interactions that
#' went into calculating the statistic.
#' @param eloTab an object of class "eloTable" from which ratings are pulled.
#' @param players Players for whom to return a schedule score.
#' @param time.range POSIXct of length two which describes the time range to 
#' look for scores in.
#' @param onlyW logical indicating wether to include only wins. Takes precednt
#' over onlyL.
#' @param onlyL logical indicating wether to include only loses.
#' @details Using an eloTable the function will calculate the schedule
#' strength of the players requested, with all players in an eloTable being the 
#' default. The schedule strength is calculated by taking the average value of
#' all the other players that an individual competed against. Using the onlyW 
#' and onlyL parameters the user can specify wether they would like to include
#' only wins or only losses repectively. 
#' @return A data frame sorted by greatest schedule strength.
#' @examples 
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # produce eloTable object
#' et1 <- eloTable (id1)
#' # get schedule strength
#' schedStrElo (et1)
#' @export

schedStrElo <- function (eloTab, players = eloTab$players,
                         time.range = eloTab$datetime, onlyW = F, onlyL = F){
    idError (eloTab, "eloTable")
    time.range <- sort (time.range)
    
    scores <- subset (eloTab$eloTable, 
                      datetime >= time.range [1] & datetime <= time.range [2])
    
    if (onlyW) {scores <- subset (scores, outcome == 1)}
    else if (onlyL) {scores <- subset (scores, outcome == -1)}
    
    pLis <- lapply (players, function (x) subset (scores, player == x))
    numInt <- sapply (pLis, function (x) nrow (x))
    SS <- sapply (pLis, function (x)  mean (x [,'opponentScore']))
    df <- data.frame (player = players, schedStr = SS, interactions = numInt)
    return (df [order (df$schedStr, decreasing = T),])
}