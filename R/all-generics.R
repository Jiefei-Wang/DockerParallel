###############cloud provider###############
#'
#' Only the user and cloud provider knows the network relationship between
#' the server and workers, so provider can set the flag `serverWorkerSameNAT` in
#' `cloudConfig` in `cluster`. Although it is possible to change any settings
#' in `cluster` in this fuction, the best practice is to only initialize `provider`
#' and change the slot `serverWorkerSameNAT`.
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
#' @return A data.frame with `publicIp` and `privateIp` columns
setGeneric("getInstanceIps", function(provider, instanceHandles, verbose = FALSE, ...){
    standardGeneric("getInstanceIps")
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
setGeneric("configServerContainerEnv", function(container, cluster, verbose = FALSE, ...){
    standardGeneric("configServerContainerEnv")
})

#' @returns
#' `container` object
setGeneric("configWorkerContainerEnv", function(container, cluster, workerNumber, verbose = FALSE, ...){
    standardGeneric("configWorkerContainerEnv")
})


setGeneric("registerParallelBackend", function(container, cluster, verbose = FALSE, ...){
    standardGeneric("registerParallelBackend")
})
setGeneric("deregisterParallelBackend", function(container, cluster, verbose = FALSE, ...){
    standardGeneric("deregisterParallelBackend")
})


