setMethod("initialProvider", "ANY", function(provider, cluster, verbose = FALSE, ...){

})

setMethod("getInstanceStatus", "ANY", function(provider, instanceHandles, verbose = FALSE, ...){
    rep("running", length(instanceHandles))
})

setMethod("IsInstanceInitializing", "ANY", function(provider, instanceHandles, verbose = FALSE, ...){
    status <- getInstanceStatus(provider=provider,
                                instanceHandles=instanceHandles,
                                verbose = verbose, ...)
    status == "initializing"
})
setMethod("IsInstanceRunning", "ANY", function(provider, instanceHandles, verbose = FALSE, ...){
    status <- getInstanceStatus(provider=provider,
                                instanceHandles=instanceHandles,
                                verbose = verbose, ...)
    status == "running"
})
setMethod("IsInstanceStopped", "ANY", function(provider, instanceHandles, verbose = FALSE, ...){
    status <- getInstanceStatus(provider=provider,
                                instanceHandles=instanceHandles,
                                verbose = verbose, ...)
    status == "stopped"
})



setMethod("deregisterParallelBackend", "ANY", function(container, cluster, verbose = FALSE, ...){
    verbosePrint(verbose, "deregistering foreach backend")
    foreach::registerDoSEQ()
})

