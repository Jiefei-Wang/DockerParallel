% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all-generics-CloudProvider.R,
%   R/method-default.R
\name{reconnectDockerCluster}
\alias{reconnectDockerCluster}
\alias{reconnectDockerCluster,ANY-method}
\title{Reconnect the cluster}
\usage{
reconnectDockerCluster(provider, cluster, verbose)

\S4method{reconnectDockerCluster}{ANY}(provider, cluster, verbose)
}
\arguments{
\item{provider}{S4 \code{CloudProvider} object. The service provider.}

\item{cluster}{S4 \code{DockerCluster} object.}

\item{verbose}{Integer. The verbose level, default 1.}
}
\value{
No return value
}
\description{
Reconnect the cluster if the cluster has been running. It is provider's
responsibility to recover every information in the cluster, especially the the
slots in \code{cloudConfg} and \code{cloudRuntime}. The developer should call setters with the
prefix \code{.set} to set the values in the cluster.
The default method does nothing.
}
\section{Functions}{
\itemize{
\item \code{reconnectDockerCluster,ANY-method}: The default method, do nothing.
}}

