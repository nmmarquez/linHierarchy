#' Print call for eloTable object
#' 
#' Prints each item in EloTable list and head of the elo update table.
#' @param eloTab an object of the class "eloTable"
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
#' print (et1)
#' @details Prints only the head of the eloTable data.frame omitting initial
#' scores
#' @export

print.eloTable <- function (eloTab){
    print (eloTab [1]);
    print (eloTab [2]);
    print (head (na.omit (eloTab [[3]])))
}