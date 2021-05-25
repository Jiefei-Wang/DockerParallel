##################################################
##     Worker management method 2:
##    DockerCluster controls the lifecycle of the workers
##################################################
#' Run the worker container
#'
#' Run the workers and return a character vector of the worker handles. Each handle must
#' correspond to a container. The handle can be duplicated if multiple workers share the
#' same container. There is no default method for this generic.
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
setGeneric("runDockerWorkerContainers",
           function(provider, cluster, container, hardware, workerNumber, verbose){
               standardGeneric("runDockerWorkerContainers")
           })


#' Get the worker status
#'
#' Get the worker status. Unless you have a faster implementation, you only need to
#' define `getDockerWorkerStatus`. The function should return a character vector with
#' each element corresponding to a worker in `workerHandles`.
#' Each element must be one of three possible characters `"initializing"`, `"running"` or
#' `"stopped"`. There is no default method for `getDockerWorkerStatus`.
#'
#'
#' @inheritParams generics-commonParams
#' @param workerHandles Character(n). A character vector of **unique** instance handles.
#'
#' @rdname workerStatus
#' @returns
#' `getDockerWorkerStatus` : A character vector with each element corresponding
#' to an instance in `workerHandles`. Each element must be one of three possible characters
#' `"initializing"`, `"running"` or `"stopped"`
#'
#' `IsDockerWorkerInitializing`, `IsDockerWorkerRunning`, `IsDockerWorkerStopped`:
#' A logical vector with each element corresponding to the status of each instance
#' @export
setGeneric("getDockerWorkerStatus", function(provider, cluster, workerHandles, verbose){
    standardGeneric("getDockerWorkerStatus")
})


#' @rdname workerStatus
#' @export
setGeneric("IsDockerWorkerInitializing", function(provider, cluster, workerHandles, verbose){
    standardGeneric("IsDockerWorkerInitializing")
})


#' @rdname workerStatus
#' @export
setGeneric("IsDockerWorkerRunning", function(provider, cluster, workerHandles, verbose){
    standardGeneric("IsDockerWorkerRunning")
})


#' @rdname workerStatus
#' @export
setGeneric("IsDockerWorkerStopped", function(provider, cluster, workerHandles, verbose){
    standardGeneric("IsDockerWorkerStopped")
})

#' Kill the worker container
#'
#' Kill the worker container. The worker handles are unique.
#' If multiple workers share the same instance, all workers in the
#' same container should be killed. There is no default method for this generic.
#'
#' @inheritParams getDockerWorkerStatus
#'
#' @returns
#' A logical vector indicating whether the killing operation is successful for each instance
#' @export
setGeneric("killDockerWorkerContainers", function(provider, cluster, workerHandles, verbose){
    standardGeneric("killDockerWorkerContainers")
})
