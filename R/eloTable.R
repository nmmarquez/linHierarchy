#' Generate a series of elo ratings from interaction data
#' 
#' Given an object of the class "interData" will produce a series of elo
#' ratings up until the latest interaction in the interData object.
#' @param interData an onject of class "interData".
#' @param initial the starting score for all participants in the group.
#' @param ... further arguments to be passed to or from other methods.
#' @details Using the methods described in Neumann et al 2011. a series of Elo
#' ratings are generated from interaction data. Initial scores may either be a
#' single numeric value or a vector of integers correspong to the players
#' element of interData. Further arguments are passed to the findK and 
#' singleOutcome functions and include the paramters; kScaleBounds, kFactors,
#' and minTresh.
#' @return An object of the class eloTable which contains a list of players
#' in the graoup, the date range of interactions and a data frame sorted by
#' date containing any changes in an individuals elo rating.
#' @examples 
#' # generate generic data
#' interactions <- data.frame (p1 = c('i', 'j', 'i'), p2 = c('j', 'h', 'h'),
#'                             o = 1, d = c ('1/1/89', '1/3/89', '1/2/89'))
#' # convert to interData object
#' id1 <- intTableConv (interactions, format = '%m/%d/%y')
#' # produce eloTable object
#' eloTable (id1)
#' @references Neumann et al (2011) Assessing Dominance Hierarchies. 
#' Animal Behaviour
#' @export

eloTable <- function (interData, initial = 1000, ...){
    if (class (interData) != 'interData') {
        warning ('object not of class "interData"')
    }
    df.int <- interData [[2]]; players <- interData [[1]]
    elo <- data.frame (player = players, score = initial, 
                       date = as.POSIXct (NA), stringsAsFactors = F)
    for (i in 1:nrow (df.int)){
        n <- nrow (elo); p1 <- df.int [i,1]; p2 <- df.int [i,2] 
        out <- df.int [i, 3]; time <- df.int [i, 4]
        sc1 <- tail (elo$score [elo$player == p1], 1) # player 1's old score
        sc2 <- tail (elo$score [elo$player == p2], 1) # player 2's old score
        k1 <- findK (sc1, ...); k2 <- findK (sc2, ...) # get k factors
        
        if (sc1 >= sc2){
            elo [n+1,'player'] <- p1; elo [n+1, 'date'] <- time
            elo [n+1,'score'] <- singleOutcome (sc1, sc2, out, k1, k2, ...) [1]
            elo [n+2,'player'] <- p2; elo [n+2, 'date'] <- time
            elo [n+2,'score'] <- singleOutcome (sc1, sc2, out, k1, k2, ...) [2]
        }
        else {
            elo [n+1,'player'] <- p1; elo [n+1,'date'] <- time
            elo [n+1,'score'] <- singleOutcome(sc2, sc1, -out, k1, k2, ...) [2]
            elo [n+2,'player'] <- p2; elo [n+2,'date'] <- time
            elo [n+2,'score'] <- singleOutcome(sc2, sc1, -out, k1, k2, ...) [1]
        }
    }
    elo.f <- list (players = players, dates = range (elo$date, na.rm = T),
                   eloTable = elo)
    names (elo.f [[2]]) <- c('start', 'end'); class (elo.f) <- 'eloTable'
    return (elo.f)
}