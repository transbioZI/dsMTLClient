% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.MTL_Trace.R
\name{ds.LR_MTL_Trace}
\alias{ds.LR_MTL_Trace}
\title{Solver of FeMTL with logistic loss and low-rank structure}
\usage{
ds.LR_MTL_Trace(X, Y, lam, C, opts, datasources = NULL, nDigits)
}
\arguments{
\item{X}{The design matrices of multiple cohorts}

\item{Y}{Label vectors of multiple cohorts}

\item{lam}{The hyper-parameter controlling the sparsity}

\item{C}{The hyper-parameter associated with L2 term}

\item{opts}{Options controlling the optimization procedure}

\item{datasources}{The connections of servers}

\item{nDigits}{The number of digits rounded for each number prepared for network transmission}
}
\value{
The converged result of optimization
}
\description{
Solver of FeMTL with logistic loss and low-rank structure
}
\details{
Solver of FeMTL with logistic loss and low-rank structure
}
\author{
Han Cao
}
