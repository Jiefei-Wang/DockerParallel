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
#' to `runDockerServer` and `runDockerWorkers`. It is used to initialize the cloud-specific
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
#' `cluster@cloudConfig$serverWorkerSameLAN` to inform `DockerCluster` whether the
#' server and the workers behind the same router. If
#' `cluster@cloudConfig$serverWorkerSameLAN` is `TRUE`(default), the worker will
#' connect with the server using the server's private IP. Otherwise, the server's
#' public IP will be used.
#'
#' Although it is possible to change any settings in `cluster` in this fuction,
#' the best practice is to only initialize `provider` and
#' the slot `cluster@cloudConfig$serverWorkerSameLAN`.
#' @return NULL
setGeneric("initializeProvider", function(provider, cluster, verbose){
    standardGeneric("initializeProvider")
})

#' Run the server
#'
#' Run the server and return the server's instance handle.
#'
#' @inheritParams commonParams
#' @param container S4 `DockerContainer` Object. The server container.
#' @param hardware S4 `CloudHardware` Object. The server hardware.
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
setGeneric("runDockerServer", function(provider, cluster, container, hardware, verbose){
    standardGeneric("runDockerServer")
})

#' Run the workers
#'
#' Run the workers and return a list of worker's instance handles. The instance handles
#' can be duplicated if multiple workers share the same instance.
#'
#'
#' @inheritParams commonParams
#' @param workerNumber Integer. The number of workers need to be run.
#' @param container S4 `DockerContainer` Object. The worker container.
#' @param hardware S4 `CloudHardware` Object. The worker hardware.
#'
#'
#' @inheritSection runDockerServer Instance Handle
#' @returns
#' A list of object that can be used by the cluster to identify the worker instances.
setGeneric("runDockerWorkers",
           function(provider, cluster, container, hardware, workerNumber, verbose){
    standardGeneric("runDockerWorkers")
})


#' Get the instance public/private IPs
#'
#' Get the instance public/private IPs. The IPs will be used by the cluster to
#' make connections between server and worker, server and client. If the instance
#' does not have the public or private IP, its value can be set to the character
#' "".
#'
#' @inheritParams commonParams
#' @param instanceHandles List. A list of instance handles.
#'
#' @return A data.frame with `publicIp` and `privateIp` columns
setGeneric("getDockerInstanceIps", function(provider, instanceHandles, verbose){
    standardGeneric("getDockerInstanceIps")
})


#' Get the instance status
#'
#' Get the instance status. Unless you have a special requirement, you only need to
#' define `getDockerInstanceStatus`. The default `getDockerInstanceStatus` do nothing but return
#' a vector of "running" with the same lenght of the input instance handles.
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
#' Kill the instances. Multiple workers may share the same instance, in this case, all
#' workers in the same instance should be killed.
#'
#' @inheritParams getDockerInstanceIps
#'
#' @returns
#' A logical vector indicating whether the killing operation is success for each instance
setGeneric("killDockerInstances", function(provider, instanceHandles, verbose){
    standardGeneric("killDockerInstances")
})



#' Whether the cluster is running on the cloud?
#'
#' The function checks whether the cluster is running on the cloud. It returns
#' true if the cluster specific to the `jobQueueName` exists.
#'
#' @inheritParams commonParams
#' @export
setGeneric("dockerClusterExists", function(provider, cluster, verbose){
    standardGeneric("dockerClusterExists")
})


#' Reconnect the cluster
#'
#' Reconnect the cluster if the cluster has been running. It is provider's
#' responsibility to recover every information in the cluster, especially the the
#' slots in `cloudConfg` and `cloudRuntime`.
#'
#' @inheritParams commonParams
#' @export
setGeneric("reconnectDockerCluster", function(provider, cluster, verbose){
    standardGeneric("reconnectDockerCluster")
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

