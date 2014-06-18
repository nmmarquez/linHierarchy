#' Generate a series of elo ratings from interaction data
#' 
#' Given an object of the class "interData" will produce a series of elo
#' ratings up until the latest interaction in the interData object.
#' @param intData An onject of class "interData".
#' @param initial The starting score for all participants in the group.
#' @param kScaleBounds The player score bins to associate with K factors. See
#' ?findK for more details
#' @param kFactors The possible K factors to be used. See ?findK for more 
#' details.
#' @param minThresh The minimum value of any players score. See 
#' ?eloSingleOutcome for more details
#' @details Using the methods described in Neumann et al 2011. a series of Elo
#' ratings are generated from interaction data. Initial scores may either be a
#' single numeric value or a vector of integers correspong to the players
#' element of intData. Further arguments are passed to the findK and 
#' singleOutcome functions and include the paramters; kScaleBounds, kFactors,
#' and minTresh.
#' @return An object of the class eloTable which contains a vector of players
#' in the graoup, the time range of interactions, and a data frame sorted by
#' time containing any changes in an individuals elo rating.
#' @examples 
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # produce eloTable object
#' eloTable (id1)
#' @references Neumann et al (2011) Assessing Dominance Hierarchies. 
#' Animal Behaviour
#' @export

eloTable <- function (intData, initial = 1000, kScaleBounds = c(-Inf, Inf), 
                      kFactors = 100, minThresh = 100){
    idError (intData)
    df.int <- intData [[3]]; players <- intData [[1]]
    elo <- data.frame (player = players, score = initial, 
                       datetime = as.POSIXct (NA), opponent = as.character (NA),
                       opponentScore = as.numeric(NA), outcome = as.numeric(NA),
                       margin = as.numeric(NA), stringsAsFactors = F)
    for (i in 1:nrow (df.int)){
        n <- nrow (elo); p1 <- df.int [i,1]; p2 <- df.int [i,2] 
        out <- df.int [i, 3]; time <- df.int [i, 4]
        sc1 <- tail (elo$score [elo$player == p1], 1) # player 1's old score
        sc2 <- tail (elo$score [elo$player == p2], 1) # player 2's old score
        k1 <- findK (sc1, kScaleBounds, kFactors) # get k factor for player.1
        k2 <- findK (sc2, kScaleBounds, kFactors) # get k factor for player.2
        
        elo [n+1,'player'] <- p1; elo [n+1, 'datetime'] <- time
        elo [n+1,'score'] <- singleOutcome (sc1, sc2, out, k1, k2, minThresh)[1]
        elo [n+1,'opponent'] <- p2; elo [n+1,'outcome'] <- out
        elo [n+1,'opponentScore'] <- sc2; elo [n+1,'margin'] <- sc1 - sc2
        elo [n+2,'player'] <- p2; elo [n+2, 'datetime'] <- time
        elo [n+2,'score'] <- singleOutcome (sc1, sc2, out, k1, k2, minThresh)[2]
        elo [n+2,'opponent'] <- p1; elo [n+2,'outcome'] <- out * -1
        elo [n+2,'opponentScore'] <- sc1; elo [n+2,'margin'] <- sc2 - sc1
    }
    elo.f <- list (players = players, datetime = range(elo$datetime,na.rm = T),
                   eloTable = elo)
    names (elo.f [[2]]) <- c('start', 'end'); class (elo.f) <- 'eloTable'
    return (elo.f)
}