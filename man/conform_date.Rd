% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{conform_date}
\alias{conform_date}
\title{Conform Date Fields}
\usage{
conform_date(x)
}
\arguments{
\item{x}{date vector (numeric or date type)}
}
\value{
a POSICXct date format vector
}
\description{
Conform Date Fields
}
\examples{
conform_date(30050)
conform_date("01/05/1983")
conform_date("05/28/1983")

}
