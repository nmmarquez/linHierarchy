#' Get the latest scores from an eloTable object until specified time
#' 
#' Pulls players latest scores from an eloTable object until specified time. If
#' no scores exist for an individual prior to that time in the table then the
#' initial value is returned.
#' 
#' @param eloTab an object of the class "eloTable" from which ratings are to be
#' extracted.
#' @param tObj an element of the class "POSIXct" for which
#' only scores occuring prior to within the eloTable will be used.
#' @param players A character list of players for which scores are obtained
#' @details Using the datetime parameter as a cieling for elo ratings this
#' function will return the latest rating for each player in the eloTable
#' object which has occured on or before that time. If no score has occured
#' prior to the specified time than the initial score is returned for that
#' player with a value of NA for the time.
#' @return A data frame containing the latest rating for each player up to the
#' datetime parameter as well as the time which that rating was last updated.
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
#' # get latest scores
#' extractScores (et1)
#' # only scores before median
#' extractScores (et1, tObj = median (et1$eloTable$datetime, na.rm = T))
#' @export

extractScores <- function (eloTab, tObj = eloTab$datetime ['end'],
                           players = eloTab [['players']]){
    idError (eloTab, "eloTable"); plyrError (players, eloTab);
    
    if (!any (grepl ("POSIX",  class (tObj)))){
        stop ("tObj must be of POSIX class")
    }
    
    until <- subset (eloTab [[3]], datetime <= tObj)
    nas <- eloTab [[3]] [!complete.cases (eloTab [[3]]),]
    wEloTab <- rbind (nas, until)
    latElo <- wEloTab [!duplicated (wEloTab$player, fromLast = T),]
    sLatElo <-latElo [latElo$player %in% players,]
    sLatElo [order (-sLatElo$score),]
}