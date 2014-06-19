#' Get the top scores achieved in an Elo table
#' 
#' The function returns a data frame with the top (or bottom) N ratings achieved 
#' by unique players.
#' @param eloTab an object of class "eloTable" from which ratings are pulled.
#' @param N the number of ratings to return.
#' @param time.range POSIXct of length two which describes the time range to 
#' look for scores in.
#' @param low logical stating wether to get the lowest lowest scores as opposed
#' to the highest.
#' @details Using an eloTable the function will look for the top or bottom
#' ratings achieved for each unique player and returns the N highest scores in a 
#' data frame. The data frame is of the same structure as the dataf frame in an 
#' eloTable object.
#' @return A data frame sorted by highest or lowest scores achieved
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
#' # get top 5 highest scores attained in the table
#' topElo (et1, 5)
#' # get top 5 lowest scores attained in the table
#' topElo (et1, 5, low = TRUE)
#' @export

topElo <- function (eloTab, N = 10, time.range = eloTab$datetime, low = FALSE){
    idError (eloTab, "eloTable")
    time.range <- sort (time.range)
    
    scores <- subset (eloTab$eloTable, 
                      datetime >= time.range [1] & datetime <= time.range [2])
    
    topSc <- scores [order (scores$score, decreasing = !low),]
    unSc <- topSc [!duplicated (topSc$player),]
    ret <- min (c(N, nrow (unSc)))
    unSc [1:ret,]
}