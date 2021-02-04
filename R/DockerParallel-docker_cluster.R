#' Create a cluster with remote docker workers
#'
#' Create a cluster with remote docker workers. This is an S4 generic.
#' The method will be dispatched based on the `config` variable
#'
#' @param config An S4 object, the configuration of the worker nodes
#' @param workers The number of workers in the cluster, additional meaning may
#' exist for a specific implementation
#' @param ... Additional arguments that will passed to the specific implementation
#' @returns
#' The return value is implementation dependent
#' @export
setGeneric("docker_cluster", function(config, workers, ...) {
  standardGeneric("docker_cluster")
})

