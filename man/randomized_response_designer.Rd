% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/randomized_response_designer.R
\name{randomized_response_designer}
\alias{randomized_response_designer}
\title{Create a randomized response design}
\usage{
randomized_response_designer(
  N = 1000,
  prob_forced_yes = 0.6,
  prevalence_rate = 0.1,
  withholding_rate = 0.5,
  args_to_fix = NULL
)
}
\arguments{
\item{N}{An integer. Size of sample.}

\item{prob_forced_yes}{A number in [0,1]. Probability of a forced yes.}

\item{prevalence_rate}{A number in [0,1]. Probability that individual has the sensitive trait.}

\item{withholding_rate}{A number in [0,1]. Probability that an individual with the sensitive trait hides it.}

\item{args_to_fix}{A character vector. Names of arguments to be args_to_fix in design.}
}
\value{
A randomized response design.
}
\description{
Produces a (forced) randomized response design that measures the share of individuals with a given trait \code{prevalence_trait} in a population of size \code{N}. Probability of forced response ("Yes") is given by \code{prob_forced_yes}, and rate at which individuals with trait lie is given by \code{withholding_rate}.
}
\details{
\code{randomized_response_designer} employs a specific variation of randomized response designs in which respondents are required to report a args_to_fix answer to the sensitive question with a given probability (see Blair, Imai, and Zhou (2015) for alternative applications and estimation strategies).

See \href{https://declaredesign.org/r/designlibrary/articles/randomized_response.html}{vignette online}.
}
\examples{
# Generate a randomized response design using default arguments:
randomized_response_design <- randomized_response_designer()
}
\author{
\href{https://declaredesign.org/}{DeclareDesign Team}
}
\concept{descriptive}
\concept{experiment}
