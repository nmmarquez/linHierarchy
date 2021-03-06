% Generated by roxygen2 (4.0.0): do not edit by hand
\name{topElo}
\alias{topElo}
\title{Get the top scores achieved in an Elo table}
\usage{
topElo(eloTab, N = 10, time.range = eloTab$datetime, low = FALSE)
}
\arguments{
\item{eloTab}{an object of class "eloTable" from which ratings are pulled.}

\item{N}{the number of ratings to return.}

\item{time.range}{POSIXct of length two which describes the time range to
look for scores in.}

\item{low}{logical stating wether to get the lowest lowest scores as opposed
to the highest.}
}
\value{
A data frame sorted by highest or lowest scores achieved
}
\description{
The function returns a data frame with the top (or bottom) N ratings achieved
by unique players.
}
\details{
Using an eloTable the function will look for the top or bottom
ratings achieved for each unique player and returns the N highest scores in a
data frame. The data frame is of the same structure as the dataf frame in an
eloTable object.
}
\examples{
# generate generic data
interactions <- data.frame (a = sample (letters [1:10], 100, T),
                            b = sample (letters [1:10], 100, T),
                            o = sample (c(-1,-1,0,1,1), 100, T),
                            d = Sys.time () + runif (100, 40, 160))
# convert to interData object
id1 <- intTableConv (interactions)
# produce eloTable object
et1 <- eloTable (id1)
# get top 5 highest scores attained in the table
topElo (et1, 5)
# get top 5 lowest scores attained in the table
topElo (et1, 5, low = TRUE)
}

