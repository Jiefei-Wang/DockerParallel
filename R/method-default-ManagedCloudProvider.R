#' @rdname workerStatus
#' @export
setMethod("IsDockerWorkerInitializing", "ANY", function(provider, cluster, workerHandles, verbose = 0L){
    status <- getDockerWorkerStatus(provider=provider,
                                    cluster = cluster,
                                    workerHandles=workerHandles,
                                    verbose = verbose)
    status == "initializing"
})

#' @rdname workerStatus
#' @export
setMethod("IsDockerWorkerRunning", "ANY", function(provider, cluster, workerHandles, verbose = 0L){
    status <- getDockerWorkerStatus(provider=provider,
                                    cluster = cluster,
                                    workerHandles=workerHandles,
                                    verbose = verbose)
    status == "running"
})

#' @rdname workerStatus
#' @export
setMethod("IsDockerWorkerStopped", "ANY", function(provider, cluster, workerHandles, verbose = 0L){
    status <- getDockerWorkerStatus(provider=provider,
                                    cluster = cluster,
                                    workerHandles=workerHandles,
                                    verbose = verbose)
    status == "stopped"
})
