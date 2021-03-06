% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ldcp.R
\name{ldcp}
\alias{ldcp}
\title{Define the LDCP system}
\usage{
ldcp(data, glmp, report)
}
\arguments{
\item{data}{is the \code{\link{data_structure}} object.}

\item{glmp}{is the \code{\link{glmp}} object.}

\item{report}{is the \code{\link{report_template}} object.}
}
\value{
The generated system \code{ldcp = list(data, glmp, report)}
}
\description{
Linguistic Descriptions of Complex Phenomena (LDCP) is a technology focused on
modeling complex phenomena, interpreting input data and generating automatic
text reports customized to the user needs. #'
The \code{ldcp} constructor receive as arguments: the \code{data_structure},
the \code{\link{glmp}} and the \code{\link{report_template}}.
}
\examples{
\dontrun{my_ldcp <- ldcp(my_data,my_glmp,my_report)}
}
\seealso{
\code{\link{cp}} and \code{\link{pm}}
}
