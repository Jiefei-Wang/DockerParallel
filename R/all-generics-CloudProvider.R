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
#' by the cloud provider. The function should call `.setServerWorkerSameLAN` to inform
#' the `DockerCluster` object whether the server and the workers are under the same router.
#' If `.getServerWorkerSameLAN` returns `TRUE`(default), the worker will connect to the
#' server using the server's private IP. Otherwise, the server's
#' public IP will be used.
#'
#' Although it is possible to change any settings in the cluster object in this function,
#' the best practice is to only initialize `provider` and
#' the value `serverWorkerSameLAN`.
#' @return No return value
#' @export
setGeneric("initializeCloudProvider", function(provider, cluster, verbose){
    standardGeneric("initializeCloudProvider")
})

#' Run or stop the server container
#'
#' Run or stop the server. These functions will not be called if the server is
#' not managed by the provider. There is no default method for these generics.
#'
#' @inheritParams generics-commonParams
#' @param container S4 `DockerContainer` Object. The server container.
#' @param hardware S4 `DockerHardware` Object. The server hardware.
#'
#' @rdname DockerServerManagement
#' @returns
#' No return value, if error occurs, the function can throw an error.
#'
#' @export
setGeneric("runDockerServer", function(provider, cluster, container, hardware, verbose){
    standardGeneric("runDockerServer")
})

#' @rdname DockerServerManagement
#' @export
setGeneric("stopDockerServer", function(provider, cluster, verbose){
    standardGeneric("stopDockerServer")
})

#' Get the server status
#'
#' Get the server status, return a character value which must be in one of three values
#' `"initializing"`, `"running"` or `"stopped"`. The default method always returns `"running"`
#'
#' @inheritParams generics-commonParams
#' @returns Character(1)
#' @export
setGeneric("getServerStatus", function(provider, cluster, verbose){
    standardGeneric("getServerStatus")
})

#' Get the server IP and port
#'
#' Get the server public/private IPs. The IPs will be used by the cluster to
#' make connections between server and worker, server and client. If the server
#' does not have the public or private IP, its value can be set to character(0) and port
#' can be set to integer(0).
#' If the IP has not been assigned yet, this function should wait until the IP is
#' available.
#' If the server is not provided by the cloud provider, this function will not be called.
#' There is no default method for this generic. The return value should be a name list
#' with four elements `publicIp`, `publicPort`, `privateIp` and `privatePort`.
#' If the server does not have the public endpoint, public IP and port can be `NULL`.
#'
#' @inheritParams generics-commonParams
#'
#' @return a name list with four elements `publicIp`, `publicPort`, `privateIp`
#' and `privatePort`.
#' @export
setGeneric("getDockerServerIp", function(provider, cluster, verbose){
    standardGeneric("getDockerServerIp")
})

#' Set the worker number on the cloud. There is no default method for this generic.
#'
#' Set the worker number on the cloud. The provider needs to scale the worker
#' number up and down accordingly.
#'
#' @inheritParams generics-commonParams
#' @return No return value
#' @export
setGeneric("setDockerWorkerNumber", function(provider, cluster, container, hardware, workerNumber, verbose){
    standardGeneric("setDockerWorkerNumber")
})

#' Get the worker number on the cloud
#'
#' Get the worker number on the cloud. Return a list with two elements,
#' which are the number of initializing and running workers. The names must
#' be `"initializing"` and `"running"`. The default method will
#' return `list(initializing = 0L, running = .getExpectedWorkerNumber(cluster))`
#'
#' @inheritParams generics-commonParams
#' @param status Character(1), the status of the container.
#' @return `list(initializing = ?, running = ?)`.
#' @export
setGeneric("getDockerWorkerNumbers", function(provider, cluster, verbose){
    standardGeneric("getDockerWorkerNumbers")
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

#' Cleanup the resources after the cluster has been stopped
#'
#' Cleanup the resources after the cluster has been stopped. After this
#' function is called, all the non-free resources should be stopped.
#' The cloud provider can still preserve some resources if they
#' are free. This generic might be called multiple times.
#' The default method does nothing.
#'
#' @inheritParams generics-commonParams
#' @param deep Logical(1), wheter all the associated resources should be removed
#' @return No return value
#' @export
setGeneric("cleanupDockerCluster", function(provider, cluster, deep, verbose){
    standardGeneric("cleanupDockerCluster")
})




