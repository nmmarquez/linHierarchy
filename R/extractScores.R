#' Get the latest scores from an eloTable object until specified time
#' 
#' Pulls players latest scores from an eloTable object until specified time. If
#' no scores exist for an individual prior to that time in the table then the
#' initial value is returned.
#' 
#' @param eloTab an object of the class "eloTable" from which ratings are to be
#' extracted.
#' @param datetime an element to be coerced to the class "POSIXct" for which
#' only scores occuring prior to within the eloTable will be used.
#' @param ... further arguments to be passed to or from other methods.
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
#' extractScores (t1, '01/01/1988', format = "%m/%d/%Y")
#' @export

extractScores <- function (eloTab, datetime = Sys.time(), ...){
    if (class (eloTab) != 'eloTable') {
        warning ('Object not of class "eloTable"')
    }
    tObj <- as.POSIXct (datetime, ...)
    until <- subset (eloTab [[3]], datetime <= tObj)
    nas <- eloTab [[3]] [!complete.cases (eloTab [[3]]),]
    wEloTab <- rbind (nas, until)
    latElo <- wEloTab [!duplicated (wEloTab$player, fromLast = T),]
    latElo [sort (-latElo$score),]
}