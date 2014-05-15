#' Plot and eloTable object
#' 
#' Using the player elo ratings of an "eloTable" object, plots a time series
#' graph showing changes in individuals scores. Specified players must have
#' at least one score in the specified time range.
#' @param eloTab object of the class "eloTable"
#' @export

plot.eloTable <- function (eloTab, start.time = eloTab [[2]] [1],
                           end.time = eloTab [[2]] [2], 
                           players = eloTab [[1]], 
                           pcol = 1:length (eloTab [[1]]), ...){
    if (!any (grepl ("POSIX",  class (start.time))) | 
        !any (grepl ("POSIX",  class (end.time)))){
        stop ("Start and end time must be of POSIX class")
    }
    dEloTab <- subset(eloTab[[3]], 
                      datetime >= start.time & datetime <= end.time)
    wEloTab <- dEloTab [dEloTab$player %in% players,]
    if (!any (players %in% wEloTab$player)){
        stop ("The following players have no data during this time period",
              players [!(players %in% wEloTab$player)])
    }
    plot (wEloTab$datetime, wEloTab$score, xlim = range (wEloTab$datetime),
          ylim = range (wEloTab$score), xlab = "Time", ylab = "Rating",
          type = 'n', main = 'Elo Rating Across Time', ...)
    for (i in 1:length (players)){
        temp = subset (wEloTab, player == players [i])
        lines (temp$datetime, temp$score, type = 'l', col = pcol [i], ...)
    }
    legend ('topleft', players, col = pcol, lty = 1, ...)
}