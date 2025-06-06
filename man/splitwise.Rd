% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitwise.R
\name{splitwise}
\alias{splitwise}
\alias{print.splitwise_lm}
\alias{summary.splitwise_lm}
\title{SplitWise Regression}
\usage{
splitwise(
  formula,
  data,
  transformation_mode = c("iterative", "univariate"),
  direction = c("backward", "forward", "both"),
  minsplit = 5,
  criterion = c("AIC", "BIC"),
  exclude_vars = NULL,
  verbose = FALSE,
  trace = 1,
  steps = 1000,
  k = 2,
  ...
)

\method{print}{splitwise_lm}(x, ...)

\method{summary}{splitwise_lm}(object, ...)
}
\arguments{
\item{formula}{A formula specifying the response and (initial) predictors, e.g. \code{mpg ~ .}.}

\item{data}{A data frame containing the variables used in \code{formula}.}

\item{transformation_mode}{Either \code{"iterative"} or \code{"univariate"}. Default = \code{"iterative"}.}

\item{direction}{Stepwise direction: \code{"backward"}, \code{"forward"}, or \code{"both"}.}

\item{minsplit}{Minimum number of observations in a node to consider splitting. Default = 5.}

\item{criterion}{Either \code{"AIC"} or \code{"BIC"}. Default = \code{"AIC"}.
\strong{Note}: If you choose \code{"BIC"}, you typically want \code{k = log(nrow(data))} in stepwise.}

\item{exclude_vars}{A character vector naming variables that should be forced to remain linear
(i.e., no dummy splits allowed). Default = \code{NULL}.}

\item{verbose}{Logical; if \code{TRUE}, prints debug info in transformation steps. Default = \code{FALSE}.}

\item{trace}{If positive, \code{step()} prints info at each step. Default = 1.}

\item{steps}{Maximum number of steps for \code{step()}. Default = 1000.}

\item{k}{Penalty multiple for the number of degrees of freedom (used by \code{step()}).
E.g. 2 for AIC, \code{log(n)} for BIC. Default = 2.}

\item{...}{Additional arguments passed to \code{summary.lm}.}

\item{x}{A \code{"splitwise_lm"} object returned by \code{splitwise}.}

\item{object}{A \code{"splitwise_lm"} object returned by \code{splitwise}.}
}
\value{
An S3 object of class \code{c("splitwise_lm", "lm")}, storing:
\item{splitwise_info}{List containing transformation decisions, final data, and call.}
}
\description{
Transforms each numeric variable into either a single-split dummy or keeps it linear,
then runs \code{stats::step()} for stepwise selection. The user can choose a
simpler univariate transformation or an iterative approach.
}
\section{Functions}{
\itemize{
\item \code{print(splitwise_lm)}: Prints a summary of the splitwise_lm object.

\item \code{summary(splitwise_lm)}: Provides a detailed summary, including how dummies were created.

}}
\examples{
# Load the mtcars dataset
data(mtcars)

# Univariate transformations (AIC-based, backward stepwise)
model_uni <- splitwise(
  mpg ~ .,
  data               = mtcars,
  transformation_mode = "univariate",
  direction           = "backward",
  trace               = 0
)
summary(model_uni)

# Iterative approach (BIC-based, forward stepwise)
# Note: typically set k = log(nrow(mtcars)) for BIC in step().
model_iter <- splitwise(
  mpg ~ .,
  data               = mtcars,
  transformation_mode = "iterative",
  direction           = "forward",
  criterion           = "BIC",
  k                   = log(nrow(mtcars)),
  trace               = 0
)
summary(model_iter)

}
