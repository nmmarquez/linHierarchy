upsetElo <- function (eloTab, N = 10, time.range = eloTab$datetime){
    idError (eloTab, "eloTable")
    time.range <- sort (time.range)
    
    scores <- subset (eloTab$eloTable, 
                      datetime >= time.range [1] & datetime <= time.range [2])
    
    upsets <- subset (scores [order (scores$margin),], outcome == 1)
    ret <- min (N, nrow (upsets))
    upsets [1:ret,]
}