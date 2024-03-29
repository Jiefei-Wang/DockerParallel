% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AAA.R
\docType{class}
\name{CloudRuntime-class}
\alias{CloudRuntime-class}
\alias{.CloudRuntime}
\title{The cloud runtime}
\description{
The cloud runtime. It is a class purely for storing the runtime information
for the cloud.
The values in \code{CloudRuntime} in a cluster can be accessed by the getter function
which starts with the prefix \code{.get}(e.g. \code{.getServerPublicIp(cluster)}).
}
\section{Fields}{

\describe{
\item{\code{serverFromOtherSource}}{Logical(1), whether the server is provided outside of
cluster. If \code{TRUE}, the cluster will not try to stop the server when it is stopped.}

\item{\code{serverPublicIp}}{Character(1) or character(0), the server public IP.}

\item{\code{serverPublicPort}}{Integer(1) or integer(0), the server public port.}

\item{\code{serverPrivateIp}}{Character(1) or character(0), the server private IP.}

\item{\code{serverPrivatePort}}{Integer(1) or integer(0), the server private port.}

\item{\code{runningWorkerNumber}}{Integer(1), the current initializing workers.}

\item{\code{runningWorkerNumber}}{Integer(1), the current running workers.}
}}


