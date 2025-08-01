% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s_plus_tr_p.R
\name{s_plus_tr_p}
\alias{s_plus_tr_p}
\title{Terminal Reinforcement Probability of the S+ Terminal Link}
\usage{
s_plus_tr_p(sig, tl_dur1, tl_dur2, tr_p1, tr_p2)
}
\arguments{
\item{sig}{Logical. Indicates whether the terminal link stimuli are assumed
to be discriminable. See \code{\link[=sig_check]{sig_check()}}.}

\item{tl_dur1}{Numeric. Duration of the first terminal link.}

\item{tl_dur2}{Numeric. Duration of the second terminal link.}

\item{tr_p1}{Numeric. Probability of terminal (primary/unconditional)
reinforcement following the first terminal link.}

\item{tr_p2}{Numeric. Probability of terminal (primary/unconditional)
reinforcement following the second terminal link.}
}
\value{
A numeric vector containing the terminal reinforcement probability
of the identified \eqn{S^{+}} stimulus, or \code{NA} if undefined.
}
\description{
Given two terminal link stimuli, identifies the one with
the greater reinforcement probability per unit of time (i.e., higher
\eqn{p/t} ratio), and returns its its terminal reinforcement probability.
This is interpreted as the \eqn{S^{+}} reinforcement probability within a
signalled alternative.
}
\details{
If both terminal reinforcement probabilities are 0, or if either
terminal link duration is 0, the function returns \code{NA}. If the choice
alternative is not signalled (\code{sig == FALSE}), \code{NA} is also returned.
}
\examples{
s_plus_tr_p(TRUE, 10, 20, 0.5, 0.8)
s_plus_tr_p(TRUE, 10, 20, 0, 0) # Returns NA
s_plus_tr_p(FALSE, 10, 20, 0.5, 0.8) # Returns NA
}
\seealso{
\code{\link[=s_delta]{s_delta()}}, \code{\link[=s_plus_dur]{s_plus_dur()}}
}
