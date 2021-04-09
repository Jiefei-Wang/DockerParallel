#' commom params
#'
#'
#' @param verbose Integer. The verbose level, default 1.
#' @param provider S4 `CloudProvider` object. The service provider.
#' @param cluster S4 `DockerCluster` object.
#' @param container S4 `DockerContainer` Object.
#' @param hardware S4 `CloudHardware` Object.
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


#' Get the instance public/private IPs
#'
#' Get the instance public/private IPs. The IPs will be used by the cluster to
#' make connections between server and worker, server and client. If the instance
#' does not have the public or private IP, the return value can be the character
#' "".
#'
#' @inheritParams commonParams
#' @param instanceHandles List. A list of instance handles.
#'
#' @return A data.frame with `publicIp` and `privateIp` columns
setGeneric("getInstanceIps", function(provider, instanceHandles, verbose){
    standardGeneric("getInstanceIps")
})


#' Get the instance status
#'
#' Get the instance status. Unless you have a special requirement, you only need to
#' define `getInstanceStatus`. The default `getInstanceStatus` do nothing but return
#' a vector of "running" with the same lenght of the input instance handles.
#'
#' @inheritParams getInstanceIps
#'
#' @rdname instanceStatus
#' @returns
#' `getInstanceStatus` : A character vector with each element corresponding
#' to an instance in `instanceHandles`. Each element must be one of three possible characters
#' `"initializing"`, `"running"` or `"stopped"`
#'
#' `IsInstanceInitializing`, `IsInstanceRunning`, `IsInstanceStopped`:
#' A logical vector with each element corresponding to the status of each instance
#' @export
setGeneric("getInstanceStatus", function(provider, instanceHandles, verbose){
    standardGeneric("getInstanceStatus")
})


#' @rdname instanceStatus
#' @export
setGeneric("IsInstanceInitializing", function(provider, instanceHandles, verbose){
    standardGeneric("IsInstanceInitializing")
})


#' @rdname instanceStatus
#' @export
setGeneric("IsInstanceRunning", function(provider, instanceHandles, verbose){
    standardGeneric("IsInstanceRunning")
})


#' @rdname instanceStatus
#' @export
setGeneric("IsInstanceStopped", function(provider, instanceHandles, verbose){
    standardGeneric("IsInstanceStopped")
})

#' Kill the instances
#'
#' Kill the instances.
#'
#' @inheritParams getInstanceIps
#'
#' @returns
#' A logical vector indicating whether the killing operation is success for each instance
setGeneric("killInstances", function(provider, instanceHandles, verbose){
    standardGeneric("killInstances")
})



###############container###############
#' Configurate the server container environment
#'
#' Configurate the server container environment. Developers can use this function
#' to set the server password, port number and etc. via the container environment variable.
#'
#' @inheritParams commonParams
#' @param container Reference Container Object. The server container.
#'
#' @return An object which has the same class as `container`
#' @export
setGeneric("configServerContainerEnv", function(container, cluster, verbose){
    standardGeneric("configServerContainerEnv")
})

#' Configurate the worker container environment
#'
#' Configurate the worker container environment. Developers can use this function
#' to set the server Ip, password and etc. via the container environment variable.
#'
#' @inheritParams commonParams
#' @param container Reference Container Object. The worker container.
#' @param workerNumber Integer. The number of workers in a container.
#'
#' @return An object which has the same class as `container`
#' @export
setGeneric("configWorkerContainerEnv", function(container, cluster, workerNumber, verbose){
    standardGeneric("configWorkerContainerEnv")
})

#' Register/deregister the parallel backend
#'
#' Register/deregister the parallel backend. These methods will be dispatched based on
#' the worker container. The parallel framework depends on the container image.
#' If the container uses the `foreach` framework, there is no need
#' to define `deregisterParallelBackend` as its default method will deregister the
#' foreach backend.
#'
#' @inheritParams commonParams
#' @param container The worker container.
#'
#' @rdname containerParallelBackend
#' @return NULL
setGeneric("registerParallelBackend", function(container, cluster, verbose, ...){
    standardGeneric("registerParallelBackend")
})
#' @rdname containerParallelBackend
setGeneric("deregisterParallelBackend", function(container, cluster, verbose, ...){
    standardGeneric("deregisterParallelBackend")
})

#' Get the server container from the worker container
#'
#' Get the server container from the worker container
#'
#' @param container The worker container.
#' @export
setGeneric("getServerContainer", function(container, ...){
    standardGeneric("getServerContainer")
})


###############provider and container###############
#' Get the exported method and variable from the provider or container
#'
#' Get the exported method and variable from the provider or container. These
#' methods should be used by the developer to export their APIs to the user. The
#' `DockerCluster` object will call `getExportedNames` and `getExportedObject` to
#' export them to the user.
#'
#' @param x A cloud provider or container object
#'
#' @details
#' If the exported object is a function, the function can find the `DockerCluster` cluster
#' which contains the cloud provider or the container by the variable `cluster`. This
#' can be useful if the developer needs to change anything in the cluster besides the provider
#' or container itself. If the argument `cluster` is provided as a function argument,
#' it will be removed when it is exported to the user. Therefore, you can explicitly
#' define the argument `cluster` and use it in your function. Users would not be bothered
#' with the redundant `cluster` argument.
#'
#' @returns
#' getExportedNames: The names of the exported methods or variables
#' getExportedObject: The exported method or variable
#' @rdname exported-apis
#' @export
setGeneric("getExportedNames", function(x){
    standardGeneric("getExportedNames")
})
#' @rdname exported-apis
#' @export
setGeneric("getExportedObject", function(x, name){
    standardGeneric("getExportedObject")
})

