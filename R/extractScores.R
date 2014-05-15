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
#' interactions <- data.frame (p1 = c('i', 'j', 'i'), p2 = c('j', 'h', 'h'),
#'                             o = 1, d = c ('1/1/89', '1/3/89', '1/2/89'))
#' # convert to interData object
#' id1 <- intTableConv (interactions, format = '%m/%d/%y')
#' # produce eloTable object
#' et1 <- eloTable (id1)
#' # get latest scores
#' extractScores (et1)
#' # only scores before '1/2/89'
#' extractScores (et1, tObj = as.POSIXct ('1/2/89', format = '%m/%d/%y'))
#' @export

extractScores <- function (eloTab, tObj = Sys.time(),
                           players = eloTab [['players']]){
    if (class (eloTab) != 'eloTable') {
        stop ('eloTab must be of class "eloTable"')
    }
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