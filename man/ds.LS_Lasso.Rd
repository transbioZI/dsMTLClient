% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.Lasso.R
\name{ds.LS_Lasso}
\alias{ds.LS_Lasso}
\title{Solver of Lasso regression}
\usage{
ds.LS_Lasso(X, Y, lam, C, opts, datasources, nDigits)
}
\arguments{
\item{X}{The design matrices of multiple cohorts}

\item{Y}{Label vectors of multiple cohorts}

\item{lam}{lambda value}

\item{C}{The hyper-parameter associated with L2 term}

\item{opts}{Options controlling the optimization procedure}

\item{datasources}{The connections of servers}

\item{nDigits}{The number of digits rounded for each number prepared for network transmission}

\item{W}{The current estimate of the variables}
}
\value{
The converged result of optimization

The proximal point of W

non-smooth part of objective
}
\description{
Solver of Lasso regression
}
\details{
Solver of Lasso regression
}
\author{
Han Cao
}
\keyword{internal}
