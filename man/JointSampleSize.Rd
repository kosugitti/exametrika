% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_TestItemFunctions.R
\name{JointSampleSize}
\alias{JointSampleSize}
\title{Joint Sample Size}
\usage{
JointSampleSize(U, na = NULL, Z = NULL, w = NULL)
}
\arguments{
\item{U}{U is a data matrix of the type matrix or data.frame.}

\item{na}{na argument specifies the numbers or characters to be treated as missing values.}

\item{Z}{Z is a missing indicator matrix of the type matrix or data.frame}

\item{w}{w is item weight vector}
}
\value{
Returns a matrix of class c("exametrika", "matrix") where each element (i,j)
represents the number of students who responded to both item i and item j. The
diagonal elements represent the total number of responses for each item.
}
\description{
The joint sample size is a matrix whose elements are the number of
individuals who responded to each pair of items.
}
\note{
This function is implemented using a binary data compatibility wrapper and
will raise an error if used with polytomous data.
}
