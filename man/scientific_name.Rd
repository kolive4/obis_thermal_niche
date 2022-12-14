% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{scientific_name}
\alias{scientific_name}
\title{Convert from file name to scientific_name}
\usage{
scientific_name(
  x = "carcharodon_carcharias.csv.gz",
  ext = ".csv.gz",
  sep = "_"
)
}
\arguments{
\item{x}{chr, name of file}

\item{ext}{chr, file extension to be removed}

\item{sep}{chr, separator of file name}
}
\value{
character vector of scientific names
}
\description{
Convert from file name to scientific_name
}
