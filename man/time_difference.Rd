% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bestBus.R
\name{time_difference}
\alias{time_difference}
\title{Time difference}
\usage{
time_difference(schedule1, schedule2)
}
\arguments{
\item{schedule1}{time in format "hh:mm:ss"}

\item{schedule2}{time in format "hh:mm:ss"}
}
\value{
the difference in minutes between 2 schedules
}
\description{
This function calculates the difference in minutes between 2 schedules
}
\examples{
time_difference("12:30:45", "12:50:10")
time_difference("13:30:00", "12:50:00")
}

