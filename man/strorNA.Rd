% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{strorNA}
\alias{strorNA}
\title{Return a str or NA}
\usage{
strorNA(x)
}
\arguments{
\item{x}{The string to check}
}
\value{
Returns x or NA
}
\description{
Converts empty strings to NA otherwise return string as is
}
\examples{
strorNA('')
strorNA('lorem')
}
