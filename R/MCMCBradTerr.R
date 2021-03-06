#' Bayesian Approximation of Bradley Terry Pairwise Comparisons
#' 
#' The function generates a posterior of player ratings using the methods
#' detailed in E.S. Adams 2005 with the MCMCpack package. 
#' @param intData An object of class "interData" which provides the data frame
#' for the analysis
#' @param fixed.player either the position of the player in the intData object
#' to have their player rating fixed to zero or their name.
#' @param b0 The prior mean of players rating under a multivariate normal prior. 
#' This can either be a scalar or a column vector with dimension equal to the 
#' number of players (excluding those who only have tie interaction outcomes) - 
#' 1. If this takes a scalar value, then that value will serve as prior mean 
#' rating for all of the players.
#' @param B0 The prior precision of player rating under a multivariate normal 
#' prior. This can either be a scalar or a square matrix with dimensions equal
#' to the number of players (excluding those who only have tie interaction 
#' outcomes) - 1. If this takes a scalar value, then that value times an identity
#' matrix serves as the prior precision of player ratings.
#' @param beta.start Intial values of the players excluding the first player
#' which fixed to zero. Must be numeric vector of length equal to number of
#' players (excluding those who only have tie interaction outcomes) - 1. If this
#' takes a scalar value that value will be the start for all player ratings 
#' (excluding the first player who is fixed at zero).
#' @param MLE logical indicating to use MLE instead of bayesian methods. Default
#' is set to FALSE. 
#' @param ... Further arguments to be passed to the MCMClogit function
#' @return If MLE == FALSE An mcmc object that contains the posterior sample. 
#' This object can be summarized by functions provided by the coda package. Else
#' a glm pbject will be returned.
#' @examples
#' # generate generic data
#' interactions <- data.frame (a = sample (letters [1:10], 100, T),
#'                             b = sample (letters [1:10], 100, T),
#'                             o = sample (c(-1,-1,0,1,1), 100, T), 
#'                             d = Sys.time () + runif (100, 40, 160))
#' # convert to interData object
#' id1 <- intTableConv (interactions)
#' # remove ties
#' id1.noTies <- subset (id1, ties = FALSE)
#' # apply Bradley-Terry MCMC function
#' posterior <- MCMCBradTerr (id1.noTies)
#' # plot posterior distributions
#' plot (posterior)
#' # MLE used instead
#' summary (MCMCBradTerr (id1.noTies, MLE = TRUE))
#' 
#' @details MCMCBradTerr uses the MCMClogit function from MCMCpack with a 
#' specially formatted data frame in order to obtain a posterior distribution of
#' the player ratings of a group. A single player is fixed to a rating of zero
#' and may be specified via the fixed.player parameter. Per the methods of 
#' E.S. Adams 2005 the fixed player should be alternated over several analyses
#' in order to see if the model is sensitive to this specification. It is also
#' advised that prior to running the MCMCBradterr function that the user subsets
#' the "interData" object such that all ties are removed.
#' @references E.S. Adams (2005) Bayesian Analysis of Linear Dominance
#' Hierarchies. Animal Behaviour.
#' @export 

MCMCBradTerr <- function (intData, fixed.player = 1, b0 = 0, B0 = .001, 
                          beta.start = 0, MLE = FALSE, ...){
    df <- toBayesDF (intData); players <- names (df [, - ncol (df)])
    
    if (class (fixed.player) == 'character'){
        fixed.player <- match (fixed.player, players)
        if (is.na (fixed.player [1])){
            stop ('Specified fixed player is either not a valid player in ',
                  'intData or has no non-tied interactions')
        }
    }
    
    if (!MLE){
        MCMClogit (outcome ~ . - 1, data = df [,-fixed.player[1]], b0 = b0, 
                   B0 = B0, beta.start = beta.start, ...)
    }
    else{
        glm(outcome ~ . - 1, data = df [,-fixed.player[1]], family = "binomial")
    }
}
