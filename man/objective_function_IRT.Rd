% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04C_ParameterEstimation.R
\name{objective_function_IRT}
\alias{objective_function_IRT}
\title{Log-likelihood function used in the Maximization Step (M-Step).}
\usage{
objective_function_IRT(lambda, model, qjtrue, qjfalse, quadrature)
}
\arguments{
\item{lambda}{item parameter vector}

\item{model}{2,3,or 4 PL}

\item{qjtrue}{correct resp pattern}

\item{qjfalse}{incorrect resp pattern}

\item{quadrature}{Pattern of a segmented normal distribution.}
}
\description{
Log-likelihood function used in the Maximization Step (M-Step).
}
