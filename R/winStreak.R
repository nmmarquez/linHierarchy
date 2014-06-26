#' Find win streak of players in group
#' 
#' Given an interData object returns the longest win streaks for each player.
#' 
#' @param intData an object of class "interData" from which wins are evaluated
#' @param players teh players to get winstreaks of
#' @details The function will return a dataframe with specified players from
#' the interData object along with their longest win streak and the dates of the
#' first and last victory. 
#' @return data frame detailing the winstreaks of the specified players.
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # get the winstreaks of players
#' winStreak (id1)
#' @export

winStreak <- function (intData, players = intData$players){
    idError (intData); plyrError (players, intData); N <- length (players)
    
    subL <- lapply (players, function (x) subset (intData, player = x, and = F))
    winC <- lapply (1:N, function (x)
        cbind (subL [[x]] [[3]], findIfWon (players [x], subL [[x]])))
    streakL <- lapply (winC, function (x) 
        cbind (x, (x [,5]) * unlist(lapply(rle(x [,5])$lengths, seq_len))))
    
    df <- data.frame (player = as.character(NA), streak = as.numeric(NA),
                      start = as.POSIXct(NA), end = as.POSIXct(NA),
                      stringsAsFactors = FALSE)
    
    for (i in 1:length (players)){
        df [i,1] <- players [i];
        Row <- tail (which (streakL [[i]] [,6] == max (streakL [[i]] [,6])),1)
        df [i,2] <- streakL [[i]] [Row,6]
        df [i,3] <- streakL [[i]] [Row,4]
        df [i,4] <- streakL [[i]] [Row - streakL [[i]] [Row,6] + 1,4]
    }
    
    df <- df [order (df$streak, decreasing = T),]; row.names (df) <- 1:nrow (df)
    df
}