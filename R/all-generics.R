#' commom params
#'
#'
#' @param verbose Integer. The verbose level, default 1.
#' @param provider S4 CloudProvider object. The service provider.
#' @param cluster S4 DockerCluster object.
#' @param container S4 Container Object.
#' @param hardware S4 CloudHardware Object.
#' @rdname commonParams
#' @name commonParams
NULL



###############cloud provider###############
#' Initialize the service provider
#'
#' Initialize the service provider. This function will be called prior
#' to `runServer` and `runWorkers`. It is used to initialize the cloud-specific
#' settings(e.g. Initialize the cloud network). The function might be called many
#' times so developers can cache the service status and speed up the initialization
#' process.
#'
#' @inheritParams commonParams
#'
#' @details
#' Basic on the cloud nature, certain initialization process might be required
#' before deploying the container on the cloud. This function will be called by
#' the `DockerCluster` object before running the server and workers.
#'
#' Besides initializing the cloud settings, if the parallel server is provided
#' by the cloud. The function might needs to change the value in
#' `cluster@cloudConfig$serverWorkerSameNAT` to inform `DockerCluster` whether the
#' server and the workers behind the same router. If
#' `cluster@cloudConfig$serverWorkerSameNAT` is `TRUE`(default), the worker will
#' connect with the server using the server's private IP. Otherwise, the server's
#' public IP will be used.
#'
#' Although it is possible to change any settings in `cluster` in this fuction,
#' the best practice is to only initialize `provider` and
#' the slot `cluster@cloudConfig$serverWorkerSameNAT`.
#' @return NULL
setGeneric("initializeProvider", function(provider, cluster, verbose){
    standardGeneric("initializeProvider")
})

#' Run the server
#'
#' Run the server and return the server's instance handle.
#'
#' @inheritParams commonParams
#' @param container S4 Container Object. The server container.
#' @param hardware S4 CloudHardware Object. The server hardware.
#'
#' @section Instance Handle:
#' Any data type can be used as the instance handle as long as it supports
#' `identical` and `unique` functions. The `DockerParallel` object will use the
#' handle to check the instance status or kill the instance. It is recommended
#' to use `numeric` or `character` as the instance handle.
#'
#' @returns
#' Any object that can be used by the cluster to identify the server instance.
setGeneric("runServer", function(provider, cluster, container, hardware, verbose){
    standardGeneric("runServer")
})

#' Run the workers
#'
#' Run the workers and return a list of worker's instance handles.
#'
#' @inheritParams commonParams
#' @param workerNumber Integer. The number of workers need to be run.
#' @param container S4 Container Object. The worker container.
#' @param hardware S4 CloudHardware Object. The worker hardware.
#'
#'
#' @inheritSection runServer Instance Handle
#' @returns
#' A list of object that can be used by the cluster to identify the worker instances.
setGeneric("runWorkers",
           function(provider, cluster, container, hardware, workerNumber, verbose){
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
setGeneric("getInstanceIps", function(provider, instanceHandles, verbose){
    standardGeneric("getInstanceIps")
})


#' @returns
#' A character vector with each element corresponding to an instance in `instanceHandles`.
#' Each element must be one of three possible characters
#' `initializing`, `running` and `stopped`
setGeneric("getInstanceStatus", function(provider, instanceHandles, verbose){
    standardGeneric("getInstanceStatus")
})

#' @returns
#' A logical vector
setGeneric("IsInstanceInitializing", function(provider, instanceHandles, verbose){
    standardGeneric("IsInstanceInitializing")
})

#' @returns
#' A logical vector
setGeneric("IsInstanceRunning", function(provider, instanceHandles, verbose){
    standardGeneric("IsInstanceRunning")
})

#' @returns
#' A logical vector
setGeneric("IsInstanceStopped", function(provider, instanceHandles, verbose){
    standardGeneric("IsInstanceStopped")
})

#' @return A logical vector
setGeneric("killInstances", function(provider, instanceHandles, verbose){
    standardGeneric("killInstances")
})

setGeneric("validateContainer", function(provider, container){
    standardGeneric("validateContainer")
})




###############container###############
#' @returns
#' `container` object
setGeneric("configServerContainerEnv", function(container, cluster, verbose){
    standardGeneric("configServerContainerEnv")
})

#' @returns
#' `container` object
setGeneric("configWorkerContainerEnv", function(container, cluster, workerNumber, verbose){
    standardGeneric("configWorkerContainerEnv")
})


setGeneric("registerParallelBackend", function(container, cluster, verbose, ...){
    standardGeneric("registerParallelBackend")
})
setGeneric("deregisterParallelBackend", function(container, cluster, verbose, ...){
    standardGeneric("deregisterParallelBackend")
})


