% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReproStatus.R
\name{ReproStatus}
\alias{ReproStatus}
\title{Function to append Reproductive Status of an animal to at each fix of GPS data}
\usage{
ReproStatus(gps, startdates, enddates, neodat, capdat, subspp, subsex,
  subset)
}
\arguments{
\item{gps}{data.frame of gps data}

\item{startdates}{vector (date format) of the start dates to subset GPS data}

\item{enddates}{vector (date format) of the end dates to subset GPS data}

\item{neodat}{path to neonate database}

\item{capdat}{path to capture database}

\item{subspp}{what species to subset}

\item{subsex}{what sex to subset}

\item{subset}{TRUE/FALSE. Whether or not function should remove non-repro animals}
}
\value{
Returns a data.frame with all gps data, AnimalID, Sex, and Species, and Reproductive Status (Repro or Non-Repro)
}
\description{
Merge cleaned GPS data with the reproductive status of animals at each fix
}
\examples{
\donttest{ReproData<-ReproStatus(gps = yourgpsdata, startdates = c('2017-05-01', '2018-05-01'), enddates = c('2017-09-01', '2018-09-01'), neodat = yourpath, subspp = "MD", subsex = "F")}
NEO DATA 
=============
}
\keyword{gps,}
\keyword{neonate,}
\keyword{reproductive}
