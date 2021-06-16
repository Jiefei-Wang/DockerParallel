###############container###############
#' Configurate the server container environment
#'
#' Configurate the server container environment. Developers can use this function
#' to set the server password, port number and etc. via the container environment variable.
#' The server info can be found by the getter function with the prefix `.getServer`
#' (e.g. `.getServerPassword(cluster)`).
#' The developer *must* calls `container$copy()` before
#' setting the server environment. The user provided environment variables should
#' be respected and overwritten only when necessary.
#' There is no default method for this generic.
#'
#' @inheritParams generics-commonParams
#' @param container Reference Container Object. The server container.
#'
#' @return An object which has the same class as `container`
#' @export
setGeneric("configServerContainerEnv", function(container, cluster, verbose){
    standardGeneric("configServerContainerEnv")
}, signature = "container")

#' Configurate the worker container environment
#'
#' Configurate the worker container environment. Developers can use this function
#' to set the server Ip, password and etc. via the container environment variable.
#' The server info can be found by the getter function with the prefix `.getServer`
#' (e.g. `.getServerPassword(cluster)`). Depending on the network status, the worker
#' can use the server private IP to connect with the server.
#' The developer *must* calls `container$copy()` before
#' setting the server environment. The user provided environment variables should
#' be respected and overwritten only when necessary.
#' There is no default method for this generic.
#'
#' @inheritParams generics-commonParams
#' @param container Reference Container Object. The worker container.
#' @param workerNumber Integer. The number of workers in a container.
#'
#' @return An object which has the same class as `container`
#' @export
setGeneric("configWorkerContainerEnv", function(container, cluster, workerNumber, verbose){
    standardGeneric("configWorkerContainerEnv")
}, signature = "container")

#' Register/deregister the parallel backend
#'
#' Register/deregister the parallel backend. These methods will be dispatched based on
#' the *worker* container. The parallel framework depends on the container image.
#' If the container uses the `foreach` framework, there is no need
#' to define `deregisterParallelBackend` as its default method will deregister the
#' foreach backend. There is no default method defined for `registerParallelBackend`.
#'
#' @inheritParams generics-commonParams
#' @param container The worker container.
#' @param ... The additional parameter that will be passed to the registration function
#'
#' @rdname containerParallelBackend
#' @return No return value
#' @export
setGeneric("registerParallelBackend", function(container, cluster, verbose, ...){
    standardGeneric("registerParallelBackend")
}, signature = "container")

#' @rdname containerParallelBackend
#' @export
setGeneric("deregisterParallelBackend", function(container, cluster, verbose, ...){
    standardGeneric("deregisterParallelBackend")
}, signature = "container")

#' Get the server container from the worker container
#'
#' Get the server container from the worker container. This function will
#' be called by the `DockerCluster` object when the user only provides a worker container
#' to its constructor.
#'  There is no default method defined for this generic.
#'
#' @param workerContainer The worker container.
#' @return A server container
#' @export
setGeneric("getServerContainer", function(workerContainer){
    standardGeneric("getServerContainer")
}, signature = "workerContainer")
