###############cloud provider###############
setGeneric("initialProvider", function(provider, cluster, verbose = FALSE, ...){
    standardGeneric("initialProvider")
})

#' Run the containers
#'
#' Run the containers and return a list of instance handles
#'
#' @return InstanceInfo object
setGeneric("runServer", function(provider, cluster, container, hardware, verbose = FALSE, ...){
    standardGeneric("runServer")
})


setGeneric("runWorkers", function(provider, cluster, container, hardware, containerNumber, verbose = FALSE, ...){
    standardGeneric("runWorkers")
})


#' Get the cluster IP
#'
#' Get the cluster IP, the IP can be used for the worker to connect with
#' the server. The IP can be anything and does not have to have any meaning
#' to the cluster.
#'
#' @param serverHandle the server handles
#'
#' @return InstanceInfo object
# setGeneric("getInstanceIps", function(provider, instanceHandles, verbose = FALSE, ...){
#     standardGeneric("getInstanceIps")
# })
setGeneric("getClusterIp", function(provider, serverHandle, verbose = FALSE, ...){
    standardGeneric("getClusterIp")
})


#' @returns
#' A character vector with each element corresponding to an instance in `instanceHandles`.
#' Each element must be one of three possible characters
#' `initializing`, `running` and `stopped`
setGeneric("getInstanceStatus", function(provider, instanceHandles, verbose = FALSE, ...){
    standardGeneric("getInstanceStatus")
})

#' @returns
#' A logical vector
setGeneric("IsInstanceInitializing", function(provider, instanceHandles, verbose = FALSE, ...){
    standardGeneric("IsInstanceInitializing")
})

#' @returns
#' A logical vector
setGeneric("IsInstanceRunning", function(provider, instanceHandles, verbose = FALSE, ...){
    standardGeneric("IsInstanceRunning")
})

#' @returns
#' A logical vector
setGeneric("IsInstanceStopped", function(provider, instanceHandles, verbose = FALSE, ...){
    standardGeneric("IsInstanceStopped")
})

#' @return A logical vector
setGeneric("killInstances", function(provider, instanceHandles, verbose = FALSE, ...){
    standardGeneric("killInstances")
})





###############container###############
#' @returns
#' `container` object
setGeneric("configServerContainer", function(container, cluster, verbose = FALSE, ...){
    standardGeneric("configServerContainer")
})

#' @returns
#' `container` object
setGeneric("configWorkerContainer", function(container, cluster, verbose = FALSE, ...){
    standardGeneric("configWorkerContainer")
})

#' @returns
#' `container` object
setGeneric("configWorkerNumber", function(container, workerNumber, verbose = FALSE, ...){
    standardGeneric("configWorkerNumber")
})

setGeneric("registerParallelBackend", function(container, cluster, verbose = FALSE, ...){
    standardGeneric("registerParallelBackend")
})
setGeneric("deregisterParallelBackend", function(container, cluster, verbose = FALSE, ...){
    standardGeneric("deregisterParallelBackend")
})


