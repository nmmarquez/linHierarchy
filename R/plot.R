#' Plot and eloTable object
#' 
#' Using the player elo ratings of an "eloTable" object, plots a time series
#' graph showing changes in individuals scores. Specified players must have
#' at least one score in the specified time range.
#' @param eloTab object of the class "eloTable" to build a plot with.
#' @param start.time object of the class "POSIXct" for whcih only scores
#' occuring after this time are graphed.
#' @param end.time object of the class "POSIXct" for whcih only scores
#' occuring before this time are graphed.
#' @param players character indicating the players in eloTab to graph
#' @param pcol vector of length equal to the players argument that
#' correspond to the colors to be used in the visualization for each player.
#' @param ... additional parameters to be passed to plotting functions
#' @details Plotting an eloTable object builds a time series line graph for 
#' each player across the range of dates in the eloTable. The players and
#' dates used may be adjusted as well as the colors to be used. Additional
#' paramters may be used to pass arguments to the plot.xy, lines and
#' legend functions.
#' @examples 
#' # generate generic data
#' interactions <- data.frame (p1 = c('i', 'j', 'i'), p2 = c('j', 'h', 'h'),
#'                             o = 1, d = c ('1/1/89', '1/3/89', '1/2/89'))
#' # convert to interData object
#' id1 <- intTableConv (interactions, format = '%m/%d/%y')
#' # produce eloTable object
#' et1 <- eloTable (id1)
#' # plot all scores
#' plot (et1)
#' # plot only one player
#' plot (et1, players = "j", pcol = "green")
#' # plot only scores before '1/2/89'
#' plot (et1, end.time = as.POSIXct ('1/2/89', format = '%m/%d/%y'))
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