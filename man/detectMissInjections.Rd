% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect.R
\name{detectMissInjections}
\alias{detectMissInjections}
\alias{detectMissInjections,Binalysis-method}
\alias{detectMissInjections,MetaboProfile-method}
\title{Detect miss injections}
\usage{
detectMissInjections(x, idx = "fileOrder")

\S4method{detectMissInjections}{Binalysis}(x, idx = "fileOrder")

\S4method{detectMissInjections}{MetaboProfile}(x, idx = "fileOrder")
}
\arguments{
\item{x}{object of class \code{Binalysis} or \code{MetaboProfile}}

\item{idx}{sample information column to use for sample indexes}
}
\value{
A list containing the name of sample information column used to index the miss injections and a vector of miss injection indexes.
}
\description{
Detect miss injected samples.
}
\details{
Samples with a total ion count (TIC) below 1.5 times the inter-quartile range are detected as miss injections.
}
\examples{
## Retrieve file paths and sample information for example data
files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1:2]

info <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')[1:2,]

## Perform spectral binning
analysis <- binneR::binneRlyse(files, 
                               info, 
                               parameters = binneR::detectParameters(files))

## Detect miss injections
miss_injections <- detectMissInjections(analysis)

## Display detected miss injections
miss_injections$missInjections
}
