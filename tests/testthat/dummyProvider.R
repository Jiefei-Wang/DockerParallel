.DummyProvider <- setRefClass("DummyProvider",
                           fields =
                               list(
                                   initialized = "logical",
                                   workerNumber = "integer",
                                   isServerRunning = "logical",
                                   cleanup = "logical"),
                           contains = "CloudProvider")

DummyProvider <- function(initialized = FALSE,
                          workerNumber = 0,
                          isServerRunning = FALSE,
                          cleanup = FALSE){
    .DummyProvider(initialized = initialized,
                   workerNumber = as.integer(workerNumber),
                   isServerRunning = isServerRunning,
                   cleanup = cleanup)
}

setMethod("initializeCloudProvider", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "logical"))
    stopifnot(!provider$initialized)

    provider$initialized <- TRUE
})

setMethod("runDockerServer", "DummyProvider", function(provider, cluster, container, hardware, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(container, "DockerContainer"))
    stopifnot(is(hardware, "DockerHardware"))
    stopifnot(is(verbose, "logical"))

    stopifnot(!provider$isServerRunning)
    provider$isServerRunning <- TRUE

    Sys.setenv(dummyProvider = "exist")
})

setMethod("stopDockerServer", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "logical"))

    stopifnot(provider$isServerRunning)
    provider$isServerRunning <- FALSE
    Sys.setenv(dummyProvider = "")
})

setMethod("getServerStatus", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "logical"))

    if(provider$isServerRunning){
        return("running")
    }else{
        return("stopped")
    }
})

setMethod("getDockerServerIp", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "logical"))
    stopifnot(provider$isServerRunning)

    list(publicIp = "192.168.1.1",
         publicPort = 123,
         privateIp = "192.168.1.2",
         privatePort = 456)
})

setMethod("setDockerWorkerNumber", "DummyProvider", function(provider, cluster, container, hardware, workerNumber, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(container, "DockerContainer"))
    stopifnot(is(hardware, "DockerHardware"))
    stopifnot(is(verbose, "logical"))
    stopifnot(cluster$isServerRunning())

    provider$workerNumber <- workerNumber
})



setMethod("getDockerWorkerNumber", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "logical"))

    provider$workerNumber
})


setMethod("dockerClusterExists", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "logical"))
    stopifnot(!provider$isServerRunning)

    Sys.getenv("dummyProvider") == "exist"
})

setMethod("reconnectDockerCluster", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "logical"))
    stopifnot(!provider$isServerRunning)
    stopifnot(Sys.getenv("dummyProvider") == "exist")


})
setMethod("cleanupDockerCluster", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "logical"))
    stopifnot(!provider$isServerRunning)
    stopifnot(!provider$cleanup)
    stopifnot(Sys.getenv("dummyProvider") != "exist")

    provider$cleanup <- TRUE
})
