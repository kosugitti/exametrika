% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_TestItemFunctions.R
\name{crr}
\alias{crr}
\alias{crr.default}
\alias{crr.binary}
\title{Correct Response Rate}
\usage{
crr(U, na = NULL, Z = NULL, w = NULL)

\method{crr}{default}(U, na = NULL, Z = NULL, w = NULL)

\method{crr}{binary}(U, na = NULL, Z = NULL, w = NULL)
}
\arguments{
\item{U}{Either an object of class "exametrika" or raw data. When raw data is given,
it is converted to the exametrika class with the \code{\link{dataFormat}} function.}

\item{na}{Values to be treated as missing values.}

\item{Z}{Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
observed responses, while 0 indicates missing data.}

\item{w}{Item weight vector specifying the relative importance of each item.}
}
\value{
A numeric vector of weighted correct response rates for each item.
Values range from 0 to 1, where higher values indicate easier items
(more students answered correctly).
}
\description{
The correct response rate (CRR) is one of the most basic and important
statistics for item analysis. This is an index of item difficulty and
a measure of how many students out of those who tried an item correctly
responded to it. This function is applicable only to binary response data.

The CRR for each item is calculated as:
\deqn{p_j = \frac{\sum_{i=1}^n z_{ij}u_{ij}}{\sum_{i=1}^n z_{ij}}}
where \eqn{z_{ij}} is the missing indicator and \eqn{u_{ij}} is the response.
}
\note{
This function is implemented using a binary data compatibility wrapper and
will raise an error if used with polytomous data.
}
\examples{
# Simple binary data
U <- matrix(c(1, 0, 1, 1, 0, 1), ncol = 2)
crr(U)

# using sample datasaet
crr(J15S500)
}
