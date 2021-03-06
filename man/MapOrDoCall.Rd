% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MapOrDoCall.R
\name{MapOrDoCall}
\alias{MapOrDoCall}
\title{\code{Map}/\code{lapply} all in one}
\usage{
MapOrDoCall(fn, multiple, single, useCache, cl = NULL)
}
\arguments{
\item{fn}{The function that will be run via \code{Map}/\code{clusterMap}.}

\item{multiple}{This a list the arguments that Map will cycle over.}

\item{single}{Passed to \code{MoreArgs} in the \code{mapply} function.}

\item{useCache}{Logical indicating whether to use the cache.}

\item{cl}{A cluster object or \code{NULL}.}
}
\description{
Usually run after \code{identifyVectorArgs} which will separate the arguments
into vectors of values for a call to \code{Map}, and arguments that have
only one value (passed to \code{MoreArgs} in \code{Map}). If all are single
length arguments, then it will pass to \code{lapply}. If a \code{cl} is provided
and is non-\code{NULL}, then it will pass all arguments to \code{clusterMap} or
\code{clusterApply}.
}
\seealso{
\code{identifyVectorArgs}
}
