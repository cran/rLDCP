% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuzzy-rules.R
\name{fuzzy_rule}
\alias{fuzzy_rule}
\title{Define the fuzzy rule}
\usage{
fuzzy_rule(...)
}
\arguments{
\item{...}{the 0 and 1 that compose the fuzzy rule.}
}
\value{
the \code{fuzzy_rule <- c(...)}
}
\description{
We define a fuzzy rule using the numbers 1 and 0.
rule(0,0,1,0,0, 0,0,1,0,0, 0,0,1,0,0, 0,0,1)

This is an example of fuzzy_rule(0,0,1,0,0,1). In the fuzzy rule the number 1 means that the linguistic
expression is included and the number 0 means that the linguistic expression is not included.
}
\examples{
# For example, the rule "IF CPtemp IS warm THEN CPcomfort IS very comfortable"
#is coded as:

 fuzzy_rule(0,1,0,0,0,1)

# Where, the first three values (0,1,0) correspond with the linguistic
# expressions Atemp=(cold, warm, hot) that define the room temperature (CPtemp).
# The last three values (0,0,1) are related to the linguistic expressions
# Acomfort=(uncomfortable, comfortable and very comfortable) that define
# the room comfort (CPcomfort).
#
}
