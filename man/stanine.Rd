% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_TestItemFunctions.R
\name{stanine}
\alias{stanine}
\alias{stanine.default}
\alias{stanine.binary}
\title{Stanine Scores}
\usage{
stanine(U, na = NULL, Z = NULL, w = NULL)

\method{stanine}{default}(U, na = NULL, Z = NULL, w = NULL)

\method{stanine}{binary}(U, na = NULL, Z = NULL, w = NULL)
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
A list containing two elements:
\describe{
\item{stanine}{The score boundaries for each stanine level}
\item{stanineScore}{The stanine score (1-9) for each student}
}
}
\description{
The Stanine (Standard Nine) scoring system divides students into nine groups
based on a normalized distribution. This function is applicable only to
binary response data.

These groups correspond to the following percentile ranges:
\itemize{
\item Stanine 1: lowest 4\% (percentiles 1-4)
\item Stanine 2: next 7\% (percentiles 5-11)
\item Stanine 3: next 12\% (percentiles 12-23)
\item Stanine 4: next 17\% (percentiles 24-40)
\item Stanine 5: middle 20\% (percentiles 41-60)
\item Stanine 6: next 17\% (percentiles 61-77)
\item Stanine 7: next 12\% (percentiles 78-89)
\item Stanine 8: next 7\% (percentiles 90-96)
\item Stanine 9: highest 4\% (percentiles 97-100)
}
}
\note{
This function is implemented using a binary data compatibility wrapper and
will raise an error if used with polytomous data.

Stanine scores provide a normalized scale with:
\itemize{
\item Mean = 5
\item Standard deviation = 2
\item Scores range from 1 to 9
\item Score of 5 represents average performance
}
}
\examples{
result <- stanine(J15S500)
# View score boundaries
result$stanine
# View individual scores
result$stanineScore

}
\references{
Angoff, W. H. (1984). Scales, norms, and equivalent scores. Educational Testing Service.
(Reprint of chapter in R. L. Thorndike (Ed.) (1971) Educational Measurement (2nd Ed.).
American Council on Education.
}
