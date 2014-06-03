#' Subsets an interData object by players and time
#' 
#' Takes an "interData" object and subsets it so that it only includes
#' interactions involving the specifed players within the specified time range.
#' @param intData an oject of class "interData" to subset.
#' @param players players which to restrict the interactions to.
#' @param time.range a length 2 sorted POSIXct vector with the first element 
#' specifying the earliest time of which to consider iteractions and the last
#' element specifying the latest time.
#' @param and logical specifying wether the players subsetted should occur in
#' the player.1 and player.2 position (and = TRUE) or just in a single position
#' (and = FALSE).
#' @param ties logical indicting wether to inculde ties. Default is TRUE.
#' @return a "subset" of the interData object.
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
#' # get any interaction with a single player
#' subset (id1, id1$players [1], and = FALSE)
#' #subset by time
#' subset (id1, time.range = c(mean (id1$datetime), id1$datetime ['end']))
#' @export

subset.interData <- function (intData, players = intData$players, 
                              time.range = intData$datetime, and = T, ties = T){
    
    tStart <- sort (time.range) [1]; tEnd <- tail (sort (time.range), 1)
    plyrs <- as.character (players); int.df <- intData$interactions
    
    if (and){
        newdf <- subset (intData$interactions, player.1 %in% plyrs & 
                             player.2 %in% plyrs & datetime >= tStart &
                             datetime <= tEnd)
    }
    else{
        newdf <- subset (intData$interactions, (player.1 %in% plyrs | 
                             player.2 %in% plyrs) & datetime >= tStart &
                             datetime <= tEnd)
    }
    
    if (!ties){
        newdf <- subset (newdf, outcome != 0)
    }
    
    intTableConv (newdf)
}