% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export.R
\name{exportCSV}
\alias{exportCSV}
\title{Export a csv}
\usage{
exportCSV(x, file, ...)
}
\arguments{
\item{x}{a data frame or tibble to export}

\item{file}{file or connection to write to}

\item{...}{arguments to pass to \code{readr::write_csv()}}
}
\value{
The file path of the exported csv.
}
\description{
Export a csv and return the file path of the exported csv.
}
\details{
If the file path directory does not exist, the directory is created prior to export.
}
\examples{
exportCSV(iris, "iris.csv")
}
