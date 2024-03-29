% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.LRBetas.R
\name{ds.LRBetas}
\alias{ds.LRBetas}
\title{Fit a logistic regression model on selected covariates}
\usage{
ds.LRBetas(X, Y, covar, datasources)
}
\arguments{
\item{X}{Predictors}

\item{Y}{Binary-Outcome}

\item{covar}{Positions of adjusting covariates in the X dataset}

\item{datasources}{The connections of servers}
}
\value{
Estimated beta coefficients for covariates

Estimated beta coefficients for covariates
}
\description{
Fit a linear logistic regression model with only selected adjusting covariates
}
\details{
Beta coefficients are employed for the estimation of lambda max

Beta coefficients are employed for the estimation of lambda max
}
\author{
Han Cao & Augusto Anguita-Ruiz
}
