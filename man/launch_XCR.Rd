% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app.R
\name{launch_XCR}
\alias{launch_XCR}
\title{Launch local XCR instance}
\usage{
launch_XCR(host = "0.0.0.0", port = 8000, launch.browser = TRUE)
}
\arguments{
\item{host}{The IP address. default is '0.0.0.0'}

\item{port}{The port to host app from}

\item{launch.browser}{Logical, indicate whether or not the default browser
should open}
}
\value{
Runs the XCR shiny applet.
}
\description{
Launch local XCR instance
}
\examples{
\dontrun{
launch_XCR()
}
}
