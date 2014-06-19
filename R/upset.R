#' Get the biggest upsets from an Elo table
#' 
#' The function returns a data frame with the N biggest upsets in an eloTable
#' object.
#' @param eloTab an object of class "eloTable" from which ratings are pulled.
#' @param N the number of ratings to return.
#' @param time.range POSIXct of length two which describes the time range to 
#' look for scores in.
#' @details Using an eloTable the function will look for the largest upset 
#' between two players as recorded by their difference in initial score. Only 
#' victories where the unlikely player wins will be considered.
#' @return A data frame sorted by greatest upset
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
#' upsetElo (et1, 5)
#' @export

upsetElo <- function (eloTab, N = 10, time.range = eloTab$datetime){
    idError (eloTab, "eloTable")
    time.range <- sort (time.range)
    
    scores <- subset (eloTab$eloTable, 
                      datetime >= time.range [1] & datetime <= time.range [2])
    
    upsets <- subset (scores [order (scores$margin),], outcome == 1)
    ret <- min (N, nrow (upsets))
    upsets [1:ret,]
}