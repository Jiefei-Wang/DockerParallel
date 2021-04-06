#' @describeIn initializeProvider The default cloud initialization method, do nothing.
#' @export
setMethod("initializeProvider", "ANY", function(provider, cluster, verbose = FALSE){

})

#' @rdname instanceStatus
#' @export
setMethod("getInstanceStatus", "ANY", function(provider, instanceHandles, verbose = FALSE){
    rep("running", length(instanceHandles))
})

#' @rdname instanceStatus
#' @export
setMethod("IsInstanceInitializing", "ANY", function(provider, instanceHandles, verbose = FALSE){
    status <- getInstanceStatus(provider=provider,
                                instanceHandles=instanceHandles,
                                verbose = verbose)
    status == "initializing"
})

#' @rdname instanceStatus
#' @export
setMethod("IsInstanceRunning", "ANY", function(provider, instanceHandles, verbose = FALSE){
    status <- getInstanceStatus(provider=provider,
                                instanceHandles=instanceHandles,
                                verbose = verbose)
    status == "running"
})

#' @rdname instanceStatus
#' @export
setMethod("IsInstanceStopped", "ANY", function(provider, instanceHandles, verbose = FALSE){
    status <- getInstanceStatus(provider=provider,
                                instanceHandles=instanceHandles,
                                verbose = verbose)
    status == "stopped"
})

#' @rdname containerParallelBackend
#' @export
setMethod("deregisterParallelBackend", "ANY", function(container, cluster, verbose = FALSE){
    verbosePrint(verbose, "deregistering foreach backend")
    foreach::registerDoSEQ()
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
