subset.interData <- function (intData, players = intData$players, 
                              time.range = intdata$date.time){
    
    tStart <- sort (time.range) [0]; tEnd <- tail (sort (time.range), 1)
    plyrs <- as.character (players); int.df <- intData$interactions
    
    newdf <- subset (intData$interactions, player.1 %in% plyrs & 
                     player.2 %in% plyrs & datetime >= tStart &
                         datetime <= tEnd)
    
    intTableConv (newdf)
}