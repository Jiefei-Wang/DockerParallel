
#' @rdname workerStatus
#' @export
setMethod("getDockerWorkerStatus", "ANY", function(provider, workerHandles, verbose = 0L){
    rep("running", length(workerHandles))
})

#' @rdname workerStatus
#' @export
setMethod("IsDockerWorkerInitializing", "ANY", function(provider, workerHandles, verbose = 0L){
    status <- getDockerWorkerStatus(provider=provider,
                                    workerHandles=workerHandles,
                                    verbose = verbose)
    status == "initializing"
})

#' @rdname workerStatus
#' @export
setMethod("IsDockerWorkerRunning", "ANY", function(provider, workerHandles, verbose = 0L){
    status <- getDockerWorkerStatus(provider=provider,
                                    workerHandles=workerHandles,
                                    verbose = verbose)
    status == "running"
})

#' @rdname workerStatus
#' @export
setMethod("IsDockerWorkerStopped", "ANY", function(provider, workerHandles, verbose = 0L){
    status <- getDockerWorkerStatus(provider=provider,
                                    workerHandles=workerHandles,
                                    verbose = verbose)
    status == "stopped"
})
