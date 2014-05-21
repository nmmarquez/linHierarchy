#' Calls error if player not in interData object
#' 
#' Error if any player in players not in "interData" object
#' @param players proposed players to use for function
#' @param intData an object of class "interData"
#' @export

plyrError <- function (players, intData){
    plyrs <- as.character (players)
    if (!all (plyrs %in% intData$players)){
        stop ('The following players are not in the argumnet ', 
              deparse (substitute (intData)), ': ',
              deparse (plyrs [!(plyrs %in% intData$players)]))
    }
}