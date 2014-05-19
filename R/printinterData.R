#' Print call for interData object
#' 
#' Prints each item in interData list and head of the interData data frame.
#' @param interData an object of the class "interData"
#' @examples 
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # print 
#' print (id1)
#' @details Prints only the head of the interData data.frame
#' @export

print.interData <- function (interData){
    print (interData [1]);
    print (head (na.omit (interData [[2]])))
}