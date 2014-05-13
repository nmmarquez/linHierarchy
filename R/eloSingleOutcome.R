#' Generate New Elo Scores from a Single Dyadic Interaction
#' 
#' Takes the elo score of two individuals as well as their rating adjustments
#' and generates new scores based on the methods of Neumann et al 2011
#' @param RA Score of the higher rated player (player A)
#' @param RB score of the lower rated player (player B)
#' @param outcome either 1 player A wins, -1 player B wins, or 0 tie
#' @param kA the k factor for player A
#' @param kB the k factor for player B
#' @param minThresh the minimum value of any players score
#' @param ... further arguments to be passed to or from other methods.
#' @references Neumann et al (2011) Assessing Dominance Hierarchies. 
#' Animal Behaviour
#' @return a vector with player A's and B's new scores
#' @export

singleOutcome <- function (RA, RB, outcome, kA, kB, minThresh = 100, ...){
    # calculate the probability of A winning
    QA <- 10**(RA/400)
    QB <- 10**(RB/400)
    p <- QA / (QA + QB)
    
    # calculate new scores
    if (outcome >= 0){
        newA <- ((RA + (1-p)*kA)**(outcome))*((RA - (1-p)*kA)**(1 - outcome))
        newB <- ((RB - (1-p)*kB)**(outcome))*((RB + (1-p)*kB)**(1 - outcome))
    }
    else{
        newA <- RA - p*kA
        newB <- RB + p*kB
    }
    
    # create player score floor so that scores never go below a min threshold
    if (newA < minThresh) {newA <- minThresh}
    if (newB < minThresh) {newB <- minThresh}
    
    return (round (c(newA, newB)))
}