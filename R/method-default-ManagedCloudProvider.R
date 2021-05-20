
#' @rdname instanceStatus
#' @export
setMethod("getDockerInstanceStatus", "ANY", function(provider, instanceHandles, verbose = 0L){
    rep("running", length(instanceHandles))
})

#' @rdname instanceStatus
#' @export
setMethod("IsDockerInstanceInitializing", "ANY", function(provider, instanceHandles, verbose = 0L){
    status <- getDockerInstanceStatus(provider=provider,
                                      instanceHandles=instanceHandles,
                                      verbose = verbose)
    status == "initializing"
})

#' @rdname instanceStatus
#' @export
setMethod("IsDockerInstanceRunning", "ANY", function(provider, instanceHandles, verbose = 0L){
    status <- getDockerInstanceStatus(provider=provider,
                                      instanceHandles=instanceHandles,
                                      verbose = verbose)
    status == "running"
})

#' @rdname instanceStatus
#' @export
setMethod("IsDockerInstanceStopped", "ANY", function(provider, instanceHandles, verbose = 0L){
    status <- getDockerInstanceStatus(provider=provider,
                                      instanceHandles=instanceHandles,
                                      verbose = verbose)
    status == "stopped"
})
