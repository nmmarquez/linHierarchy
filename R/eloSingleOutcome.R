#' Generate New Elo Scores from a Single Dyadic Interaction
#' 
#' Takes the elo score of two individuals as well as their rating adjustments
#' and generates new scores based on the methods of Neumann et al 2011
#' @param R1 Current score of player.1.
#' @param R2 Current score of player.2.
#' @param out either 1 player 1 wins, -1 player 2 wins, or 0 tie
#' @param kA the k factor for player 1
#' @param kB the k factor for player 2
#' @param minThresh the minimum value of any players score
#' @references Neumann et al (2011) Assessing Dominance Hierarchies. 
#' Animal Behaviour
#' @return a vector with player 1's and 2's new scores
#' @export

singleOutcome <- function (R1, R2, out, k1, k2, minThresh = 100, ...){
    if (R1 >= R2) {RA <- R1; RB <- R2; kA <- k1; kB <- k2; flip = FALSE}
    else {RA <- R2; RB <- R1; kA <- k2; kB <- k1; flip = TRUE; out <- out * -1}
    
    # calculate the probability of A winning
    QA <- 10**(RA/400); QB <- 10**(RB/400); p <- QA / (QA + QB)
    
    # calculate new scores
    if (out >= 0){
        newA <- ((RA + (1-p)*kA)**(out)) * ((RA + (.5-p)*kA)**(1 - out))
        newB <- ((RB - (1-p)*kB)**(out)) * ((RB + (-.5+p)*kB)**(1 - out))
    }
    else{
        newA <- RA - p*kA; newB <- RB + p*kB
    }
    
    # create player score floor so that scores never go below a min threshold
    if (newA < minThresh) {newA <- minThresh}
    if (newB < minThresh) {newB <- minThresh}
    
    if (flip) {return (round (c(newB, newA)))}
    else {return (round (c(newA, newB)))}
}