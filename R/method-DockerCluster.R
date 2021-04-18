# cluster$startCluster()
# cluster$startServer()
# cluster$startWorkers(workerNum)
# cluster$stopCluster()
# cluster$stopServer()
# cluster$stopWorkers(workerNum)
# cluster$getWorkerNumber()
# cluster$status()



startCluster <- function(cluster, ...){
    verbose <- cluster$verbose

    ## Check if the cluster exists on the cloud
    ## and ask user what to do
    answer <- checkIfClusterExistAndAsk(cluster)
    if(!answer)
        return(invisible(NULL))

    ## Start the server
    startServer(cluster)

    ## Start the worker
    workerNumber <- cluster@cloudConfig$workerNumber
    verbosePrint(verbose>0, "The cluster has ",workerNumber," workers")
    setWorkerNumber(cluster, workerNumber)

    ## Register backend
    registerBackend(cluster, ...)
    invisible(NULL)
}

#' Run the server
#'
#' Run the server and set the cluster IP
startServer <- function(cluster){
    verbose <- cluster$verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    initializeProvider(provider = provider, cluster=cluster, verbose = verbose)

    removeDiedServer(cluster)
    if(!isServerRunning(cluster)){
        ## Run the server if it does not exist
        if(!is.null(cloudRuntime$serverHandle)){
            stop("Server handle exists but the cluster IP does not")
        }
        if(is.null(cluster@serverContainer)){
            stop("No server container can be found")
        }
        verbosePrint(verbose, "Launching server")
        serverContainer <-
            configServerContainerEnv(
                cluster@serverContainer,
                cluster = cluster,
                verbose = verbose
            )
        instanceHandle <- runDockerServer(provider = provider,
                                    cluster = cluster,
                                    container=serverContainer,
                                    hardware=cloudConfig$serverHardware,
                                    verbose = verbose)
        cloudRuntime$serverHandle <- instanceHandle
        serverIp <- getDockerInstanceIps(
            provider,
            instanceHandles = list(instanceHandle),
            verbose = verbose
        )
        if(serverIp$privateIp==""){
            cloudRuntime$serverPrivateIp <- NULL
        }else{
            cloudRuntime$serverPrivateIp <-serverIp$privateIp
        }
        if(serverIp$publicIp==""){
            cloudRuntime$serverPublicIp <- NULL
        }else{
            cloudRuntime$serverPublicIp <-serverIp$publicIp
        }
    }
    invisible(NULL)
}

setWorkerNumber <- function(cluster, workerNumber){
    stopifnot(workerNumber>=0)
    workerNumber <- as.integer(workerNumber)
    cloudConfig <- cluster@cloudConfig

    cloudConfig$workerNumber <- workerNumber
    workerOffset <- cloudConfig$workerNumber - getWorkerNumber(cluster)
    if(workerOffset > 0){
        addWorkersInternal(cluster, workerOffset)
    }
    if(workerOffset < 0){
        removeWorkersInternal(cluster, abs(workerOffset))
    }
    invisible(NULL)
}


addWorkers <- function(cluster, workerNumber){
    workerNumber <- as.integer(workerNumber)

    verbose <- cluster$verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    initializeProvider(provider = provider, cluster=cluster, verbose = verbose)
    ## get the expected worker number
    cloudConfig$workerNumber <- cloudConfig$workerNumber + workerNumber

    ## get the number of workers that will be added
    requiredAddedNumber <- cloudConfig$workerNumber - getWorkerNumber(cluster)
    if(requiredAddedNumber<=0){
        return(invisible(NULL))
    }
    addWorkersInternal(cluster, requiredAddedNumber)
    invisible(NULL)
}


removeWorkers<- function(cluster, workerNumber){
    workerNumber <- as.integer(workerNumber)
    cloudRuntime <- cluster@cloudRuntime
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    verbose <- cluster$verbose

    stopifnot(cloudConfig$workerNumber >= workerNumber)
    ## set the expected worker number
    cloudConfig$workerNumber <- cloudConfig$workerNumber - workerNumber

    ## get the number of workers that will be killed
    requiredKilledNumber <- getWorkerNumber(cluster) - cloudConfig$workerNumber
    if(requiredKilledNumber <= 0){
        return(invisible(NULL))
    }
    removeWorkersInternal(cluster, requiredKilledNumber)
}



stopCluster <- function(cluster){
    verbosePrint(cluster$verbose, "Stopping cluster")
    deregisterBackend(cluster)
    setWorkerNumber(cluster, 0)
    stopServer(cluster)
    invisible(NULL)
}

stopServer<- function(cluster){
    verbose <- cluster$verbose
    provider <- cluster@cloudProvider
    cloudRuntime <- cluster@cloudRuntime
    if(!is.null(cloudRuntime$serverHandle)){
        result <- killDockerInstances(provider,
                                instanceHandles = list(cloudRuntime$serverHandle),
                                verbose = verbose)
        if(result){
            resetRuntimeServer(cloudRuntime)
        }
    }
    invisible(NULL)
}

reconnect <- function(cluster, ...){
    verbose <- cluster$verbose
    provider <- cluster@cloudProvider
    exist <- dockerClusterExists(provider=provider, cluster=cluster, verbose=verbose)
    if(!exist)
        stop("The cluster with the job queue name <",
             .getJobQueueName(cluster),
             "> is not running!")

    if(cluster$stopClusterOnExit){
        verbosePrint(verbose>0, "<stopClusterOnExit> will be set to FALSE")
        cluster$stopClusterOnExit <- FALSE
    }
    reconnectDockerCluster(provider = provider,
                           cluster = cluster,
                           verbose = verbose)
    cluster$registerBackend(...)
}


getWorkerNumber <- function(cluster){
    removeDiedWorkers(cluster)
    sum(cluster@cloudRuntime$workerPerHandle)
}

getExpectedWorkerNumber <- function(cluster){
    cluster@cloudConfig$workerNumber
}

registerBackend <- function(cluster, ...){
    verbose <- cluster$verbose
    verbosePrint(verbose, "Registering foreach redis backend, it might take a few minutes")
    if(!is.null(cluster@cloudRuntime$serverHandle)){
        success <- waitInstanceUntilRunning(
            cluster@cloudProvider,
            list(cluster@cloudRuntime$serverHandle)
        )
        if(!success){
            stop("The server is not running!")
        }
    }
    registerParallelBackend(container = cluster@workerContainer,
                            cluster = cluster,
                            verbose = cluster$verbose, ...)
    cluster@settings$parallelBackendRegistered <- TRUE
    invisible(NULL)
}


deregisterBackend <- function(cluster){
    if(cluster@settings$parallelBackendRegistered){
        deregisterParallelBackend(container = cluster@workerContainer,
                                  cluster = cluster,
                                  verbose = cluster$verbose)
        cluster@settings$parallelBackendRegistered <- FALSE
    }
    invisible(NULL)
}

isServerRunning <- function(cluster){
    ipExist <- !all(
        is.null(cluster@cloudRuntime$serverPrivateIp),
        is.null(cluster@cloudRuntime$serverPublicIp)
    )
    if(!ipExist){
        if(is.null(.getServerHandle(cluster))){
            FALSE
        }else{
            TRUE
        }
    }else{
        TRUE
    }
}

update <- function(cluster){
    removeDiedServer(cluster)
    removeDiedWorkers(cluster)
    cluster
}
