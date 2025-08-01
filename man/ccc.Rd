% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccc.R
\name{ccc}
\alias{ccc}
\title{Concordance Correlation Coefficient (CCC)}
\usage{
ccc(x, y, bias_correction = TRUE)
}
\arguments{
\item{x}{A numeric vector of predicted or measured values.}

\item{y}{A numeric vector of observed or reference values. Must be the same
length as \code{x}.}

\item{bias_correction}{Logical. If \code{TRUE}, applies a bias correction by
using sample variance and covariance estimators (i.e., dividing by
\eqn{n - 1} instead of \eqn{n}).}
}
\value{
A numeric value between -1 and 1 indicating the level of concordance.
A value of 1 indicates perfect agreement.
}
\description{
Computes the Concordance Correlation Coefficient (CCC) between two numeric
vectors.
The CCC assesses the agreement between two sets of measurements by combining
measures of both precision (correlation) and accuracy (closeness to the
identity line).
}
\examples{
x <- c(1, 2, 3, 4, 5)
y <- c(1.1, 1.9, 3.2, 4.1, 4.8)
ccc(x, y)                          # Unbiased sample-based method
ccc(x, y, bias_correction = FALSE) # Lin's original population method
}
\references{
Lawrence I-Kuei Lin. (1989). A Concordance Correlation Coefficient to Evaluate Reproducibility. \emph{Biometrics}, \emph{45}(1), 255–268. \url{https://doi.org/10.2307/2532051}
}
