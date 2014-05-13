#' Generate the appropriate K factor given a players score
#' 
#' Given a players score, ranges of scores to associate with a K factor and the
#' K values to use for the given ranges the function will find the appropriate 
#' K score to use for an elo rtaing update based on Neumann et al 2011.
#' @param playerScore the score of the player in question
#' @param kScaleBounds the player score bins to associate with K factors
#' @param kFactors the possible K factors to be used
#' @param ... further arguments to be passed to or from other methods.
#' @references Neumann et al (2011) Assessing Dominance Hierarchies. 
#' Animal Behaviour
#' @return the players associated K factor
#' @details Given a parameterization of kScaleBounds = c(-Inf, 2100, 2400, Inf)
#' and kFactors = c(32,24,16): 
#'  
#' - If an individuals score is less than 2100 the kFactor assigned is 32
#' 
#' - Between 2100 and 2400 the kFactor assigned is 24
#' 
#' - A score above 2400 the the kFactor assigned is 16
#' @note In order to ensure a proper function call the kScaleBounds
#' parameter must be sorted from least to greatest and must be of a length
#' 1 greater than the kFactors parameter. In addition assigning the first
#' element of kScaleBounds to -Inf and the last to Inf will allow the
#' players score to always remain within an acceptable range.
#' @examples
#' # generic function calls
#' findK (1000)
#' findK (1000, kScaleBounds = c(-Inf, 1100, Inf), kFactors = c(32, 16))
#' # may cause errors in elo algorithim if range of kscale bounds isnt all real
#' # numbers but you can still call it this way
#' findK (1000, kScaleBounds = c(0, 500, 100, Inf), kFactors = c(32, 16, 8))
#' @export

findK <- function (playerScore,kScaleBounds =c(-Inf, Inf),kFactors = 100, ...){
    
    bounds <- sort (kScaleBounds)
    
    if (length (kScaleBounds) != length (kFactors) + 1){
        stop ("The number of kFactors must be one less than the length of ",
              "the paramter kScaleBounds")
    }
    
    if (playerScore < bounds [1] | playerScore > bounds [length (bounds)]){
        stop ("Player score does not fall in kScaleBounds range.")
    }
    
    else{
        for (i in 1:length (bounds) - 1){
            if (playerScore > bounds [i] && playerScore <= bounds [i + 1]) {
                assignedK = kFactors [i]
                break
            }
        }
    }
    
    return (assignedK)
}