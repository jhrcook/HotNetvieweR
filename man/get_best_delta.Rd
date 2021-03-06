% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_best_delta.R
\name{get_best_delta}
\alias{get_best_delta}
\title{Finds the best delta for a the HotNet2 results}
\usage{
get_best_delta(delta_df, pval_cutoff = 0.05)
}
\arguments{
\item{pval_cutoff}{The maximum p-value to use for statistical significance.}

\item{delta_tib}{A data frame (or tibble) with the columns ... for all of the delta values.}
}
\value{
The best delta value to use or \code{NA} if none can be used.
}
\description{
Briefly, ... (explain how the best delta is chosen). More is explained in the
vignette "...".
}
