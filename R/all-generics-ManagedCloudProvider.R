##################################################
##     Worker management method 2:
##    DockerCluster controls the lifecycle of the workers
##################################################
#' Run the worker container
#'
#' Run the workers and return a character vector of the container handles
#' which the workers live in.
#' The handle must be recognized by the cloud provider to manage the containers.
#' The instance handles can be duplicated if multiple workers share the same container instance.
#' There is no default method for this generic.
#'
#'
#' @inheritParams generics-commonParams
#' @param workerNumber Integer. The number of workers needs to be run.
#' @param container S4 `DockerContainer` Object. The worker container.
#' @param hardware S4 `DockerHardware` Object. The worker hardware.
#'
#'
#' @returns
#' A character vector with each element corresponding to a worker container.
#' The length must be equal to `workerNumber`
#' @export
setGeneric("runDockerWorkers",
           function(provider, cluster, container, hardware, workerNumber, verbose){
               standardGeneric("runDockerWorkers")
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
#' @inheritParams generics-commonParams
#' @param instanceHandles Character(n). A list of instance handles.
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
#' @inheritParams getDockerInstanceStatus
#'
#' @returns
#' A logical vector indicating whether the killing operation is successful for each instance
#' @export
setGeneric("killDockerInstances", function(provider, instanceHandles, verbose){
    standardGeneric("killDockerInstances")
})
