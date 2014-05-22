#' Subsets an interData object by players and time
#' 
#' Takes an "interData" object and subsets it so that it only includes
#' interactions involving the specifed players within the specified time range.
#' @param intData an oject of class "interData" to subset.
#' @param players players which to restrict the interactions to.
#' @param time.range a length 2 sorted POSIXct vector with the first element 
#' specifying the earliest time of which to consider iteractions and the last
#' element specifying the latest time. 
#' @return a "subset" interData object.
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # subset by players
#' subset (id1, id1$players [-c(1,2)])
#' #subset by time
#' subset (id1, time.range = c(mean (id1$datetime), id1$datetime ['end']))
#' @export

subset.interData <- function (intData, players = intData$players, 
                              time.range = intData$datetime){
    
    tStart <- sort (time.range) [1]; tEnd <- tail (sort (time.range), 1)
    plyrs <- as.character (players); int.df <- intData$interactions
    
    newdf <- subset (intData$interactions, player.1 %in% plyrs & 
                     player.2 %in% plyrs & datetime >= tStart &
                         datetime <= tEnd)
    
    intTableConv (newdf)
}