% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LionData.R
\name{LionData}
\alias{LionData}
\title{Append AID to GPS data and subset to desired time period}
\usage{
LionData(filepath, startdates, enddates)
}
\arguments{
\item{filepath}{path to all lion movement file}

\item{startdates}{vector of start dates to subset data}

\item{enddates}{vector of end dates to subset data}
}
\value{
Returns a data.frame with all gps data, AnimalID, Sex, and Movement Rates
}
\description{
Merge raw GPS Data with Animal IDs, and Sex
}
\examples{
\donttest{Lion<-LionData(filepath = yourfile startdates = '2017-05-01', enddates = '2017-09-01')}
}
\keyword{animal}
\keyword{id,}
\keyword{lion,}
\keyword{mountain}
\keyword{movement}
\keyword{rate}
