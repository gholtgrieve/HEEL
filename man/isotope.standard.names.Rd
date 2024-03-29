% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isotope.standard.names.R
\docType{data}
\name{isotope.standard.names}
\alias{isotope.standard.names}
\title{Data set containing official names and synonyms of isotope standards}
\format{
A list with 8 elements
}
\usage{
data(isotope.standard.names)
}
\description{
Data set, as a list, of the official names for HEEL isotope standards plus a bunch of
possible synonyms.  Each element of the list contains possible names for one standard,
with the name of that element being the "official" name.  Intended to be used with the
check.isotope.standard.names() function, which checks a given string against the
list and, if there, returns the official name.
}
\keyword{datasets}
