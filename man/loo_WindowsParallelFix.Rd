% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayes_functions.R
\name{loo_WindowsParallelFix}
\alias{loo_WindowsParallelFix}
\title{Workaround fix for a broken loo package command}
\usage{
loo_WindowsParallelFix(model)
}
\arguments{
\item{model}{An rstanarm model object.}
}
\description{
The standard loo command doesn't work on Windows because of a bug. This command gets around that bug by extracting the log likelihood first, and then running the loo command on that extracted object
}
