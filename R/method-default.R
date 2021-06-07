#' @describeIn initializeCloudProvider The default cloud initialization method, do nothing.
#' @export
setMethod("initializeCloudProvider", "ANY", function(provider, cluster, verbose = 0L){

})

#' @describeIn getDockerWorkerNumbers The default getDockerWorkerNumbers method. Return `c(0L, .getExpectedWorkerNumber(cluster))`
#' @export
setMethod("getDockerWorkerNumbers", "ANY", function(provider, cluster, verbose = 0L){
    list(initializing = 0L, running = .getExpectedWorkerNumber(cluster))
})


#' @describeIn dockerClusterExists The default method, it always returns `FALSE`.
#' @export
setMethod("dockerClusterExists", "ANY",function(provider, cluster, verbose){
    FALSE
})

#' @describeIn reconnectDockerCluster The default method, do nothing.
#' @export
setMethod("reconnectDockerCluster", "ANY", function(provider, cluster, verbose){
    message("No reconnect method has been defined in the provider")
    invisible(NULL)
})
#' @describeIn cleanupDockerCluster The default method, do nothing.
#' @export
setMethod("cleanupDockerCluster", "ANY", function(provider, cluster, verbose){
    invisible(NULL)
})

##########################################################
#' @describeIn getServerContainer The default method throws an error
#' @export
setMethod("getServerContainer", "ANY",function(workerContainer){
    stop("No server container getter is defined for this container")
})


##########################################################
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






