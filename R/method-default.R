#' @describeIn initializeProvider The default cloud initialization method, do nothing.
#' @export
setMethod("initializeProvider", "ANY", function(provider, cluster, verbose = 0L){

})

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


#' @describeIn dockerClusterExists The default method, it always returns `FALSE`.
#' @export
setMethod("dockerClusterExists", "ANY",function(provider, cluster, verbose){
    FALSE
})


#' @describeIn reconnectDockerCluster The default method, do nothing.
#' @export
setMethod("reconnectDockerCluster", "ANY",function(provider, cluster, verbose){
    message("No reconnect method has been defined in the provider")
})





#' @rdname containerParallelBackend
#' @export
setMethod("deregisterParallelBackend", "ANY", function(container, cluster, verbose = 0L){
    verbosePrint(verbose, "deregistering foreach backend")
    foreach::registerDoSEQ()
})



#' @describeIn getServerContainer The default method, return `NULL` value
#' @export
setMethod("getServerContainer", "ANY",function(workerContainer){
    NULL
})



#' @rdname exported-apis
#' @export
setMethod("getExportedNames", "ANY", function(x){
    NULL
})

#' @rdname exported-apis
#' @export
setMethod("getExportedObject", "ANY", function(x, name){
    stop("Unable to find the exported object.")
})
