#' Calculate the David's Score of players
#' 
#' Calculates the David's Score of players in an "interData" object.
#' @param intData object of class "interData" to calculate scores.
#' @param corrected specify wether to use David's adjustment for chance.
#' @param normalized specify wether to use a normalizing factor detailed in 
#' de Vries et al (2006).
#' @details Using the methods outlined in Gamel et al. 2003 and de Vries et al.
#' 2006 a David's score is calculated using interactions from intData. Adjusting 
#' the corrected parameter will modify the algorithm to use David's adjustment
#' for chance. 
#' @return A 2 column data frame specifying the players used in the algorithm 
#' sorted by their corresponding david's score.
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # calculate David's Score
#' davidScore (id1)
#' # with David's adjustment for chance
#' davidScore (id1, corrected = TRUE)
#' @references Gammel et al. (2003) David's Score. Animal Behaviour.  
#' de Vries et al (2006). Measuring and testing the steepness of 
#' dominance hierarchies. Animal Behaviour.
#' @export

davidScore <- function (intData, corrected = FALSE, normalize = FALSE){
    idError (intData); plyrs <- intData$players
    if (corrected){
        Pmat <- Dij (intData)
    }
    else{
        Pmat <- Pij (intData)
    }
    
    w <- rowSums (Pmat); l <- colSums (Pmat)
    w2 <- Pmat %*% w; l2 <- t (t(l) %*% Pmat)
    
    DS <- data.frame (players = plyrs, score = w + w2 - l - l2)
    DS <- DS [order (-DS$score),]; row.names (DS) <- 1:nrow(DS)
    
    if (normalize){
        DS$score <- (DS$score + nrow (Pmat) * ((nrow (Pmat) - 1)/2))/nrow (Pmat)
    }
    
    DS
}