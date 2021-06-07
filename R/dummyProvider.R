.DummyProvider <- setRefClass("DummyProvider",
                           fields =
                               list(
                                   initialized = "logical",
                                   isServerRunning = "logical",
                                   cleanup = "logical",
                                   serverContainer = "DockerContainer",
                                   workerContainer = "DockerContainer"),
                           contains = "CloudProvider")
#' reset the dummy provider
#'
#' reset the dummy provider and remove all the environment variables it
#' defined.
#' @examples
#' resetDummyProvider()
#' @return No return value
#' @export
resetDummyProvider <- function(){
    Sys.setenv(dummyProvider = "")
    Sys.setenv(dummyProviderClusterData = "")
    Sys.setenv(dummyProviderWorkerNumber = "")
    invisible()
}

#' Create a Dummy provider for testing the container
#'
#' Create a Dummy provider for testing the container
#'
#' @param initialized,isServerRunning,cleanup logical(1), the flags
#'
#' @examples DummyProvider()
#' @return A `DummyProvider` object
#' @export
DummyProvider <- function(initialized = FALSE,
                          isServerRunning = FALSE,
                          cleanup = FALSE){
    .DummyProvider(initialized = initialized,
                   isServerRunning = isServerRunning,
                   cleanup = cleanup)
}

#' Create a Dummy provider for testing the container
#'
#' This function will set the slot `initialized` to `TRUE`
#'
#' @inheritParams initializeCloudProvider
#'
#' @return No return value
#' @export
setMethod("initializeCloudProvider", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(!provider$initialized)

    provider$initialized <- TRUE
})

#' Create a Dummy provider for testing the container
#'
#' This function will set the slot `isServerRunning` to `TRUE` and `cleanup` to `FALSE`.
#' It also adds the environment variable `dummyProvider` and `dummyProviderClusterData`.
#'
#' @inheritParams runDockerServer
#'
#' @return No return value
#' @export
setMethod("runDockerServer", "DummyProvider", function(provider, cluster, container, hardware, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(container, "DockerContainer"))
    stopifnot(is(hardware, "DockerHardware"))
    stopifnot(is(verbose, "numeric"))

    stopifnot(!provider$isServerRunning)
    provider$isServerRunning <- TRUE
    provider$cleanup <- FALSE
    provider$serverContainer <- container
    Sys.setenv(dummyProvider = .getJobQueueName(cluster))

    clusterData <- serialize(getDockerStaticData(cluster), NULL)
    encodedValue <- jsonlite::base64_enc(clusterData)
    Sys.setenv(dummyProviderClusterData = encodedValue)
})

#' Create a Dummy provider for testing the container
#'
#' This function will set the slot `isServerRunning` to `FALSE`
#'
#' @inheritParams stopDockerServer
#'
#' @return No return value
#' @export
setMethod("stopDockerServer", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))

    stopifnot(provider$isServerRunning)
    provider$isServerRunning <- FALSE
    Sys.setenv(dummyProvider = "")
    Sys.setenv(dummyProviderClusterData = "")
})

#' Create a Dummy provider for testing the container
#'
#' This function will return either "running" or "stopped" depending on
#' the slot `isServerRunning`
#'
#' @inheritParams getServerStatus
#'
#' @return No return value
#' @export
setMethod("getServerStatus", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))

    if(provider$isServerRunning){
        return("running")
    }else{
        return("stopped")
    }
})

#' Create a Dummy provider for testing the container
#'
#' This function always returns
#' `list(publicIp = "8.8.8.8", publicPort = 123, privateIp = "192.168.1.1", privatePort = 456)`
#'
#' @inheritParams getDockerServerIp
#'
#' @return No return value
#' @export
setMethod("getDockerServerIp", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(provider$isServerRunning)

    list(publicIp = "8.8.8.8",
         publicPort = 123,
         privateIp = "192.168.1.1",
         privatePort = 456)
})

#' Create a Dummy provider for testing the container
#'
#' This function will set the environment variable `dummyProviderWorkerNumber` and stores its
#' container in the slot `workerContainer`.
#'
#' @inheritParams setDockerWorkerNumber
#'
#' @return No return value
#' @export
setMethod("setDockerWorkerNumber", "DummyProvider", function(provider, cluster, container, hardware, workerNumber, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(container, "DockerContainer"))
    stopifnot(is(hardware, "DockerHardware"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(cluster$isServerRunning())

    provider$workerContainer <- container
    Sys.setenv(dummyProviderWorkerNumber = workerNumber)
})

#' Create a Dummy provider for testing the container
#'
#' This function returns value defined by the environment variable `dummyProviderWorkerNumber`
#'
#' @inheritParams getDockerWorkerNumbers
#'
#' @return No return value
#' @export
setMethod("getDockerWorkerNumbers", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))

    value <- Sys.getenv("dummyProviderWorkerNumber")
    if(value != ""){
        list(initializing = 0L,
             running = as.integer(Sys.getenv("dummyProviderWorkerNumber")))
    }else{
        list(initializing = 0L,
             running = 0)
    }
})

#' Create a Dummy provider for testing the container
#'
#' This function returns `TRUE` only when the environment variable `dummyProvider` is
#' equal to the job queue name
#'
#' @inheritParams dockerClusterExists
#'
#' @return No return value
#' @export
setMethod("dockerClusterExists", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))

    Sys.getenv("dummyProvider") == .getJobQueueName(cluster)
})

#' Create a Dummy provider for testing the container
#'
#' This function will try to resume the cluster from the environment variable `dummyProviderClusterData`
#'
#' @inheritParams reconnectDockerCluster
#'
#' @return No return value
#' @export
setMethod("reconnectDockerCluster", "DummyProvider", function(provider, cluster, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(!provider$isServerRunning)
    stopifnot(Sys.getenv("dummyProvider") == .getJobQueueName(cluster))
    encodedValue <- Sys.getenv("dummyProviderClusterData")
    staticData <- unserialize(jsonlite::base64_dec(encodedValue))
    setDockerStaticData(cluster, staticData)
    provider$isServerRunning <- TRUE
})

#' Create a Dummy provider for testing the container
#'
#' This function will set the slot `cleanup` to `TRUE`
#'
#' @inheritParams cleanupDockerCluster
#'
#' @return No return value
#' @export
setMethod("cleanupDockerCluster", "DummyProvider", function(provider, cluster, verbose){
    # stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(!provider$isServerRunning)
    stopifnot(Sys.getenv("dummyProvider") == "")
    provider$cleanup <- TRUE
})
