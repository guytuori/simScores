% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/miscellaneous.R
\name{j_display_name}
\alias{j_display_name}
\title{Go from Baseball-Reference Name to Display Name}
\usage{
j_display_name(name)
}
\arguments{
\item{name}{the name to change.  Must be in the style "First Last"}
}
\value{
the name in the format "Last; First"
}
\description{
Takes a name in the Baseball-Reference style and turns it into a name for the
apps
}
\examples{
j_display_name("Mike Schmidt")
}

