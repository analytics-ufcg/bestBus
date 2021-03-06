% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bestBus.R
\name{best_travel_deprecated}
\alias{best_travel_deprecated}
\title{The best travel function - deprecated}
\usage{
best_travel_deprecated(prev_travel, curr_travel, later_travel)
}
\arguments{
\item{prev_travel}{length of the previous travel in minutes}

\item{curr_travel}{length of the main travel in minutes}

\item{later_travel}{length of the next travel in minutes}
}
\value{
-1 or 0 or 1
}
\description{
This function calculates the travel index with the minimum length:
 0 if the current travel has the minimum length;
-1 if the previous travel has the minimum length;
 1 if the next travel has the minimum length;
}
\examples{
best_travel_deprecated(20,21,30)
best_travel_deprecated(NA,21,30)
best_travel_deprecated(20,21,NA)
}

