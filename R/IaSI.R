#' Find player rankings with I&SI method
#'
#' Implements I&SI method for finding a "linear hierarchy" among players
#' @param intData object of class "interData" to find player rankings
#' @param max.iter the maximum number of iterations for the algorithm to run
#' @details Applies the methods described in Han de Vries 1998 to find a linear 
#' hierarchy amongst interacting members of a group. The algorithm attempts to
#' minimize the number of occurences where a player of low ranking wins a
#' majority of interactions over a higher ranking player. If the algorithm finds
#' an ordering where this even never occurs it will immeadiately exit and return
#' the result.
#' @return a list with
#' - a data frame with all players sorted by their ranking
#' - a list detailing the number of inconsistancies and strength of
#' inconstancies
#' - the dominance matrix reflecting the ranking order
#' @references Han de Vries (1998) Finding a Dominance Order Most Consistant
#' with a Linear Hierarchy.
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # apply I&SI method for finding a linear hierarchy
#' IaSI (id1)
#' @export

IaSI <- function (intData, max.iter = 100){
    bestMat <- toDomMat (intData); pairs <- t(combn (intData$players, 2))
    I <- inconsis (bestMat); SI <- SI (bestMat); tempMat <- bestMat; t1 <- 0
    
    while (t1 < max.iter){
        stopIt2 <- F
        while (stopIt2 == F){
            stopIt2 <- T 
            for (i in 1:nrow (pairs)){
                pi <- pairs [i,1]; pj <- pairs [i,2]
                ri <- match (pi, colnames (tempMat))
                rj <- match (pj, colnames (tempMat))
                srows <- sort (c(ri,rj))
                if (tempMat [srows [2], srows [1]] == 1){
                    netInc <- 0
                    for (k in srows [1]:(srows [2] - 1)){
                        netInc <- netInc + 
                            sign (tempMat [srows [2],k] - tempMat [k,srows [2]])
                    }
                    if (netInc > 0){
                        tempMat <- swapRC (tempMat, srows [2], srows [1])
                        stopIt2 <- F
                    }
                }
            }
        }
        
        if ((inconsis (tempMat) < I) | 
                (inconsis (tempMat) == I & SI (tempMat) < SI)){
            bestMat <- tempMat; I <- inconsis (bestMat); SI <- SI (bestMat)
        }
        
        else{
            t1 <- t1 + 1
            if (I > 0){
                tempMat2 <- tempMat
                for (pj in  colnames (tempMat)){
                    rjn <- match (pj, colnames (tempMat))
                    if (sum (tempMat [rj, 1:rj]) > 0){
                        temp_i <- sample (1:rj, 1)
                        tempMat2 <- swapRC (tempMat2, rj, temp_i)
                    }
                }
                tempMat <- tempMat2
            }
            else {
                break
            }
        }
    }
    IandSI <- c(I,SI); names (IandSI) <- c('I', 'SI')
    
    list (rankings = data.frame (ranking = 1:nrow (bestMat),
                                 players = row.names (bestMat)),
          IandSI = IandSI, dom.Matrix = bestMat)
}