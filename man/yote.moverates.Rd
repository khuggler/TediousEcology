% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yote.moverate.R
\name{yote.moverates}
\alias{yote.moverates}
\title{Append AID to GPS data and subset to desired time period}
\usage{
yote.moverates(username, password, dirdown, cType = "ATS/IRID", yotedat)
}
\arguments{
\item{username}{username of account where gps data stored}

\item{password}{password of account where gps data stored}

\item{dirdown}{temporary directory for data to be downloaded}

\item{cType}{"ATS/IRID". Won't change.}

\item{yotedat}{path to coyote capture database}
}
\value{
Returns a data.frame with all gps data, AnimalID, Sex,
}
\description{
Merge raw GPS Data with Animal IDs, and Sex
}
\examples{
\donttest{yote.gps<-yote.gps(username = yourusername, password = yourpassword, dirdown = tempdir, cType = "ATS/IRID", yotedat = path, startdates = '2017-05-01', enddates = '2017-09-01')}
}
\keyword{animal}
\keyword{coyote,}
\keyword{id,}
\keyword{movement}
\keyword{rate}
