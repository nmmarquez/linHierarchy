davidScoreLA <- function (intData){
    idError (intData); plyrs <- intData$players
    Pmat <- sapply (plyrs, function (x) 
        sapply (plyrs, function (y) Pij (y, x, intData)))
    
    w <- rowSums (Pmat); l <- colSums (Pmat)
    w2 <- Pmat %*% w; l2 <- t (t(l) %*% Pmat)
    
    DS <- data.frame (players = plyrs, score = w + w2 - l - l2)
    
    DS <- DS [order (-DS$score),]; row.names (DS) <- 1:nrow(DS)
    
    DS
}