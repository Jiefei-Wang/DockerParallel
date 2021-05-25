handlePrefix <- "DummyManagedProviderHandle_"
getHandleName <- function(i){
    paste0(handlePrefix, i)
}
getAllHandles <- function(){
    envs <- Sys.getenv()
    idx <- startsWith(names(envs), handlePrefix)
    handles <- envs[idx]
    handleNames <- gsub(handlePrefix, "", names(handles), fixed = TRUE)
    names(handles) <- handleNames
    for(i in seq_along(handles)){
        handles[i] <- as.integer(handles[i])
    }
    handles
}

#########################################################
.DummyManagedProvider <- setRefClass("DummyManagedProvider",
                              fields =
                                  list(
                                      initialized = "logical",
                                      isServerRunning = "logical",
                                      cleanup = "logical",
                                      handleIdx = "integer",
                                      serverContainer = "DockerContainer",
                                      workerContainer = "DockerContainer"),
                              contains = "ManagedCloudProvider")


DummyManagedProvider <- function(initialized = FALSE,
                          isServerRunning = FALSE,
                          cleanup = FALSE){
    .DummyManagedProvider(initialized = initialized,
                   isServerRunning = isServerRunning,
                   cleanup = cleanup,
                   workerHandles = character(0),
                   handleIdx = 0L)
}

setMethod("initializeCloudProvider", "DummyManagedProvider", function(provider, cluster, verbose){
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(!provider$initialized)

    provider$initialized <- TRUE
})


setMethod("runDockerServer", "DummyManagedProvider", function(provider, cluster, container, hardware, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(container, "DockerContainer"))
    stopifnot(is(hardware, "DockerHardware"))
    stopifnot(is(verbose, "numeric"))

    stopifnot(!provider$isServerRunning)
    provider$isServerRunning <- TRUE
    provider$cleanup <- FALSE
    provider$serverContainer <- container
    Sys.setenv(DummyManagedProvider = .getJobQueueName(cluster))

    clusterData <- serializeDockerClusterStaticData(cluster)
    encodedValue <- jsonlite::base64_enc(clusterData)
    Sys.setenv(dummyProviderClusterData = encodedValue)
})

setMethod("stopDockerServer", "DummyManagedProvider", function(provider, cluster, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))

    stopifnot(provider$isServerRunning)
    provider$isServerRunning <- FALSE
    Sys.setenv(DummyManagedProvider = "")
})

setMethod("getServerStatus", "DummyManagedProvider", function(provider, cluster, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))

    if(provider$isServerRunning){
        return("running")
    }else{
        return("stopped")
    }
})

setMethod("getDockerServerIp", "DummyManagedProvider", function(provider, cluster, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(provider$isServerRunning)

    list(publicIp = "8.8.8.8",
         publicPort = 123,
         privateIp = "192.168.1.1",
         privatePort = 456)
})


setMethod("runDockerWorkerContainers", "DummyManagedProvider", function(provider, cluster, container, hardware, workerNumber, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(container, "DockerContainer"))
    stopifnot(is(hardware, "DockerHardware"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(cluster$isServerRunning())

    workerPerContainer <- 4
    handleIdxBegin <- provider$handleIdx
    handleIdxEnd <- handleIdxBegin + ceiling(workerNumber/workerPerContainer) - 1
    provider$handleIdx <- as.integer(handleIdxEnd + 1)
    handles <- unlist(lapply(handleIdxBegin:handleIdxEnd, function(x) as.character(rep(x, workerPerContainer))))
    handles <- handles[seq_len(workerNumber)]
    provider$workerContainer <- container

    workerInContainer <- table(handles)
    for(i in seq_along(workerInContainer)){
        name <- getHandleName(names(workerInContainer)[i])
        value <- workerInContainer[i]
        arg <- list(value)
        names(arg) <- name
        do.call(Sys.setenv, arg)
    }
    handles
})


setMethod("getDockerWorkerStatus", "DummyManagedProvider", function(provider, cluster, workerHandles, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))

    allHandles <- getAllHandles()
    idx <- workerHandles%in% names(allHandles)
    status <- rep("running", length(workerHandles))
    status[!idx] <- "stopped"
    status
})

setMethod("killDockerWorkerContainers", "DummyManagedProvider", function(provider, cluster, workerHandles, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(length(workerHandles) == length(unique(workerHandles)))

    for(i in workerHandles){
        handleName <- getHandleName(i)
        Sys.unsetenv(handleName)
    }
    rep(TRUE, length(workerHandles))
})


setMethod("dockerClusterExists", "DummyManagedProvider", function(provider, cluster, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(!provider$isServerRunning)

    Sys.getenv("DummyManagedProvider") == .getJobQueueName(cluster)
})

setMethod("reconnectDockerCluster", "DummyManagedProvider", function(provider, cluster, verbose){
    stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(!provider$isServerRunning)
    stopifnot(Sys.getenv("DummyManagedProvider") == .getJobQueueName(cluster))
    encodedValue <- Sys.getenv("dummyProviderClusterData")
    clusterData <- jsonlite::base64_dec(encodedValue)
    unserializeDockerClusterStaticData(cluster, clusterData)
    allHandles <- getAllHandles()
    addManagedWorkerHandles(provider, names(allHandles))
    provider$isServerRunning <- TRUE
})


setMethod("cleanupDockerCluster", "DummyManagedProvider", function(provider, cluster, verbose){
    # stopifnot(provider$initialized)
    stopifnot(is(cluster, "DockerCluster"))
    stopifnot(is(verbose, "numeric"))
    stopifnot(!provider$isServerRunning)
    stopifnot(!provider$cleanup)
    stopifnot(Sys.getenv("DummyManagedProvider") == "")
    stopifnot(length(getAllHandles()) == 0)

    provider$cleanup <- TRUE

})
