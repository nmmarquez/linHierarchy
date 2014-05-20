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
#' @param players character indicating the players in eloTab to graph.
#' @param col vector of length equal to the players argument that
#' correspond to the colors to be used in the visualization for each player.
#' @param xlab a label for the x axis, defaults to "Time".
#' @param ylab a label for the y axis, defaults to "Rating".
#' @param main a main title for the plot.
#' @param xlim the x limits (x1, x2) of the plot. Note that x1 > x2 is allowed
#' and leads to a ‘reversed axis’.
#' @param ylim the y limits of the plot.
#' @param lty,lwd the line types and widths for lines appearing in the legend.
#' @param legend.pos the position of the legend on the graph
#' @param bty the type of box to be drawn around the legend. The allowed values
#' are "o" (the default) and "n".
#' @param ... additional parameters to be passed to the base plot function.
#' @details Plotting an eloTable object builds a time series line graph for 
#' each player across the range of dates in the eloTable. The players and
#' dates used may be adjusted as well as the colors to be used. Additional
#' paramters may be used to pass arguments to the plot.xy, lines and
#' legend functions.
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
#' # plot all scores
#' plot (et1)
#' # plot only three players
#' plot (et1, players = et1$players [1:3], lty = 3, lwd = 5)
#' # plot only scores before median
#' plot (et1, end.time = median (et1$eloTable$datetime, na.rm = T), lty = 6,
#'       main = 'Elo Rating up to Median Date', legend.pos = "bottomleft")
#' @export

plot.eloTable <- function (eloTab, start.time = eloTab [[2]] [1],
                           end.time = eloTab [[2]] [2], 
                           players = eloTab [[1]], 
                           col = 1:length (eloTab [[1]]),
                           xlab = "Time", ylab = "Rating",
                           main = 'Elo Rating Across Time',
                           xlim = range (wEloTab$datetime),
                           ylim = range (wEloTab$score), lty = 1, lwd = 1, 
                           legend.pos = 'topleft', bty = 'o', ...){
    if (!any (grepl ("POSIX",  class (start.time))) | 
        !any (grepl ("POSIX",  class (end.time)))){
        stop ("Start and end time must be of POSIX class")
    }
    dEloTab <- subset(eloTab[[3]], 
                      datetime >= start.time & datetime <= end.time)
    wEloTab <- dEloTab [dEloTab$player %in% players,]
    if (!all (players %in% wEloTab$player)){
        warning ("The following players have no data during this time period",
              players [!(players %in% wEloTab$player)])
    }
    plot (wEloTab$datetime, wEloTab$score, type = 'n', xlab= xlab, ylab = ylab,
          xlim = xlim, ylim = ylim, main = main, ...)
    for (i in 1:length (players)){
        temp = subset (wEloTab, player == players [i])
        lines (temp$datetime, temp$score, col = col [i], lty = lty, lwd = lwd)
    }
    legend (legend.pos, players, col = col, lty = lty, lwd = lwd, bty = bty)
}