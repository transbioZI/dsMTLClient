% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.MTL_iNMF.R
\name{ds.solveINMF}
\alias{ds.solveINMF}
\title{Solver of Federated integrative NMF}
\usage{
ds.solveINMF(
  datasources,
  Xs,
  newH,
  rank = 2,
  lam = 1,
  Sp = 1,
  opts = list(maxIter = 40, tol = 0.01, ter = 2),
  nDigits = 10
)
}
\arguments{
\item{datasources}{The connections of servers}

\item{Xs}{The data matrices of multiple cohorts}

\item{newH}{The starting point shared component matrix H}

\item{rank}{The hyper-parameter indicate the number of  latent variables}

\item{lam}{The hyper-parameter associated with L2 term}

\item{Sp}{The hyper-parameter controlling the sparsity}

\item{opts}{Options controlling the optimization procedure}

\item{nDigits}{The number of digits rounded for each number prepared for network transmission}
}
\value{
The converged result of optimization
}
\description{
Solver of Federated NMF
}
\details{
Solver of Federated NMF
}
\author{
Han Cao
}
