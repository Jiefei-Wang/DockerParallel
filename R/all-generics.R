#' commom params
#'
#'
#' @param verbose Integer. The verbose level, default 1.
#' @param provider S4 `CloudProvider` object. The service provider.
#' @param cluster S4 `DockerCluster` object.
#' @param container S4 `DockerContainer` Object.
#' @param hardware S4 `DockerHardware` Object.
#' @rdname generics-commonParams
#' @name generics-commonParams
#' @return No return value
NULL



###############cloud provider###############
#' Initialize the service provider
#'
#' Initialize the service provider. This function will be called prior
#' to `runDockerServer` and `runDockerWorkers`. It is used to initialize the cloud-specific
#' settings(e.g. Initialize the cloud network). The function might be called many
#' times. Developers can cache the cloud status and speed up the initialization
#' process.
#'
#' @inheritParams generics-commonParams
#'
#' @details
#' Based on the cloud nature, an initialization process might be required
#' before deploying the container on the cloud. This function will be called by
#' the `DockerCluster` object before running the server and workers. The default
#' method will do nothing.
#'
#' Besides initializing the cloud settings, if the server container will be deployed
#' by the cloud provider. The function should call `.setServerWorkerSameLAN()` to inform
#' the `DockerCluster` object whether the server and the workers are under the same router.
#' If `.getServerWorkerSameLAN()` is `TRUE`(default), the worker will connect to the
#' server using the server's private IP. Otherwise, the server's
#' public IP will be used.
#'
#' Although it is possible to change any settings in `cluster` in this function,
#' the best practice is to only initialize `provider` and
#' the value `serverWorkerSameLAN`.
#' @return No return value
#' @export
setGeneric("initializeProvider", function(provider, cluster, verbose){
    standardGeneric("initializeProvider")
})

#' Run the server container
#'
#' Run the server and return the server instance handle. There is no default method
#' for this generic.
#'
#' @inheritParams generics-commonParams
#' @param container S4 `DockerContainer` Object. The server container.
#' @param hardware S4 `DockerHardware` Object. The server hardware.
#'
#' @section Instance Handle:
#' The instance handle is nothing but any data type that can be used by the
#' cloud provider to identify the running container. The data type should
#' supports `identical` and `unique` functions. The `DockerParallel` object will use the
#' handle to check the instance status or kill the instance. Though it is not required,
#' but we recommend to use `character` as the instance handle.
#'
#' @returns
#' Any object that can be used by the cluster to identify the server instance.
#' @export
setGeneric("runDockerServer", function(provider, cluster, container, hardware, verbose){
    standardGeneric("runDockerServer")
})

#' Run the worker container
#'
#' Run the workers and return a list of worker instance handles. The instance handles
#' can be duplicated if multiple workers share the same instance. There is no default method
#' for this generic.
#'
#'
#' @inheritParams generics-commonParams
#' @param workerNumber Integer. The number of workers needs to be run.
#' @param container S4 `DockerContainer` Object. The worker container.
#' @param hardware S4 `DockerHardware` Object. The worker hardware.
#'
#'
#' @inheritSection runDockerServer Instance Handle
#' @returns
#' A list of object that can be used by the cluster to identify the worker instances.
#' The lenght must be equal to `workerNumber`
#' @export
setGeneric("runDockerWorkers",
           function(provider, cluster, container, hardware, workerNumber, verbose){
    standardGeneric("runDockerWorkers")
})


#' Get the instance public/private IPs
#'
#' Get the instance public/private IPs. The IPs will be used by the cluster to
#' make connections between server and worker, server and client. If the instance
#' does not have the public or private IP, its value can be set to the character
#' "". There is no default method for this generic.
#'
#' @inheritParams generics-commonParams
#' @param instanceHandles List. A list of instance handles.
#'
#' @return A data.frame with `publicIp` and `privateIp` columns and each row corresponds
#' to an element in `instanceHandles`.
#' @export
setGeneric("getDockerInstanceIps", function(provider, instanceHandles, verbose){
    standardGeneric("getDockerInstanceIps")
})


#' Get the instance status
#'
#' Get the instance status. Unless you have a faster implementation, you only need to
#' define `getDockerInstanceStatus`. The function `getDockerInstanceStatus` should return
#' a character vector with each element corresponding to an instance in `instanceHandles`.
#' Each element must be one of three possible characters `"initializing"`, `"running"` or
#' `"stopped"`.
#' The default `getDockerInstanceStatus` returns
#' a vector of character "running" with the same length of the input instance handles.
#'
#'
#' @inheritParams getDockerInstanceIps
#'
#' @rdname instanceStatus
#' @returns
#' `getDockerInstanceStatus` : A character vector with each element corresponding
#' to an instance in `instanceHandles`. Each element must be one of three possible characters
#' `"initializing"`, `"running"` or `"stopped"`
#'
#' `IsDockerInstanceInitializing`, `IsDockerInstanceRunning`, `IsDockerInstanceStopped`:
#' A logical vector with each element corresponding to the status of each instance
#' @export
setGeneric("getDockerInstanceStatus", function(provider, instanceHandles, verbose){
    standardGeneric("getDockerInstanceStatus")
})


#' @rdname instanceStatus
#' @export
setGeneric("IsDockerInstanceInitializing", function(provider, instanceHandles, verbose){
    standardGeneric("IsDockerInstanceInitializing")
})


#' @rdname instanceStatus
#' @export
setGeneric("IsDockerInstanceRunning", function(provider, instanceHandles, verbose){
    standardGeneric("IsDockerInstanceRunning")
})


#' @rdname instanceStatus
#' @export
setGeneric("IsDockerInstanceStopped", function(provider, instanceHandles, verbose){
    standardGeneric("IsDockerInstanceStopped")
})

#' Kill the instances
#'
#' Kill the instances. The instance handles are unique.
#' If multiple workers share the same instance, all workers in the
#' same instance should be killed.
#' There is no default method for this generic.
#'
#' @inheritParams getDockerInstanceIps
#'
#' @returns
#' A logical vector indicating whether the killing operation is success for each instance
#' @export
setGeneric("killDockerInstances", function(provider, instanceHandles, verbose){
    standardGeneric("killDockerInstances")
})



#' Whether the cluster is running on the cloud?
#'
#' The function checks whether the cluster is running on the cloud. It returns
#' `TRUE` if the cluster specific to the value from `.getJobQueueName(cluster)` exists.
#' The default method always returns `FALSE`
#'
#' @inheritParams generics-commonParams
#' @return A logical value
#' @export
setGeneric("dockerClusterExists", function(provider, cluster, verbose){
    standardGeneric("dockerClusterExists")
})


#' Reconnect the cluster
#'
#' Reconnect the cluster if the cluster has been running. It is provider's
#' responsibility to recover every information in the cluster, especially the the
#' slots in `cloudConfg` and `cloudRuntime`. The developer should call setters with the
#' prefix `.set` to set the values in the cluster.
#' The default method does nothing.
#'
#' @inheritParams generics-commonParams
#' @return No return value
#' @export
setGeneric("reconnectDockerCluster", function(provider, cluster, verbose){
    standardGeneric("reconnectDockerCluster")
})


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
})

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
})

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
})
#' @rdname containerParallelBackend
#' @export
setGeneric("deregisterParallelBackend", function(container, cluster, verbose, ...){
    standardGeneric("deregisterParallelBackend")
})

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
})


###############provider and container###############
#' Get the exported method and variable from the provider or container
#'
#' Get the exported method and variable from the provider or container. These
#' methods should be used by the developer to export their APIs to the user. The
#' `DockerCluster` object will call `getExportedNames` and `getExportedObject` and
#' export them to the user.
#'
#' @param x A cloud provider or container object
#'
#' @details
#' If the exported object is a function, the exported function will be defined in
#' an environment such that the `DockerCluster` object is assigned to the variable `cluster`.
#' In other words, the exported function can use the variable `cluster` without define it.
#' This can be useful if the developer needs to change anything in the cluster without
#' asking the user to provide the `DockerCluster` object. The best practice is to define
#' `cluster` as the function argument, the argument will be removed when the function is
#' exported to the user. The user would not be bothered with the redundant `cluster` argument.
#'
#' @returns
#' getExportedNames: The names of the exported functions or variables
#' getExportedObject: The exported functions or variable
#' @rdname exported-apis
#' @export
setGeneric("getExportedNames", function(x){
    standardGeneric("getExportedNames")
})
#' @param name The name of the exported object
#' @rdname exported-apis
#' @export
setGeneric("getExportedObject", function(x, name){
    standardGeneric("getExportedObject")
})

