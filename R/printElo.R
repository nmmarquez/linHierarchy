#' Print call for eloTable object
#' 
#' Prints each item in EloTable list and head of the elo update table.
#' @param eloTab an object of the class "eloTable"
#' @examples 
#' # generate generic data
#' interactions <- data.frame (p1 = c('i', 'j', 'i'), p2 = c('j', 'h', 'h'),
#'                             o = 1, d = c ('1/1/89', '1/3/89', '1/2/89'))
#' # convert to interData object
#' id1 <- intTableConv (interactions, format = '%m/%d/%y')
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