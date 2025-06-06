% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find.prob.hyp.r
\name{find.prob.hyp}
\alias{find.prob.hyp}
\title{Finding probabilities for a given hypothesis}
\usage{
find.prob.hyp(hyp, diel.setup = NULL, fast = TRUE)
}
\arguments{
\item{hyp}{hypothesis name, for example, D.max}

\item{diel.setup}{A list of created by diel.ineq(). Contains matrices A and vector b, or matrices A and C, and vectors b and d.}

\item{fast}{Default is TRUE, which uses p.options2 instead of p.options3. Does not apply to equality hypotheses.}
}
\value{
A matrix of probabilities that match hypothesis specified by hyp
}
\description{
Function that inputs a given hypothesis and outputs as many possible
probability sets that match the diel hypothesis (i.e., satisfies the inequality constrints). Allows for inequalities, equalities, and non-linear inputs.
}
\examples{
find.prob.hyp(hyp ="D.max")
}
