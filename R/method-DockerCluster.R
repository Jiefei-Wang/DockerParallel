# cluster$startCluster()
# cluster$startServer()
# cluster$startWorkers(workerNum)
# cluster$stopCluster()
# cluster$stopServer()
# cluster$stopWorkers(workerNum)
# cluster$getWorkerNumber()
# cluster$status()
startCluster <- function(cluster){
    verbose <- cluster$verbose

    ## Start the server
    startServer(cluster)

    ## Start the worker
    workerNumber <- cluster@cloudConfig$workerNumber
    verbosePrint(verbose>0, "The cluster has ",workerNumber," workers")
    setWorkerNumber(cluster, workerNumber)

    ## Register backend
    registerBackend(cluster)
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
        if(is.null(cloudConfig$serverContainer)){
            stop("No server container can be found")
        }
        verbosePrint(verbose, "Launching server")
        serverContainer <-
            configServerContainerEnv(
                cloudConfig$serverContainer,
                cluster = cluster,
                verbose = verbose
            )
        instanceHandle <- runServer(provider,
                                    container=serverContainer,
                                    hardware=cloudConfig$serverHardware,
                                    verbose = verbose)
        cloudRuntime$serverHandle <- instanceHandle
        serverIp <- getInstanceIps(
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
    addWorkersInternl(cluster, requiredAddedNumber)
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
    removeWorkersInternl(cluster, requiredKilledNumber)
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
        result <- killInstances(provider,
                                instanceHandles = list(cloudRuntime$serverHandle),
                                verbose = verbose)
        if(result){
            resetRuntimeServer(cloudRuntime)
        }
    }
    invisible(NULL)
}

getWorkerNumber <- function(cluster){
    removeDiedWorkers(cluster)
    sum(cluster@cloudRuntime$workerPerHandle)
}
getExpectedWorkerNumber <- function(cluster){
    cluster@cloudConfig$workerNumber
}

## TODO: add ... to customize foreach
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
    registerParallelBackend(container = cluster@cloudConfig$workerContainer,
                            cluster = cluster,
                            verbose = cluster$verbose, ...)
    cluster@settings$parallelBackendRegistered <- TRUE
    invisible(NULL)
}


deregisterBackend <- function(cluster){
    if(cluster@settings$parallelBackendRegistered){
        deregisterParallelBackend(container = cluster@cloudConfig$workerContainer,
                                  cluster = cluster,
                                  verbose = cluster$verbose)
        cluster@settings$parallelBackendRegistered <- FALSE
    }
    invisible(NULL)
}

isServerRunning <- function(cluster){
    !all(
        is.null(cluster@cloudRuntime$serverPrivateIp),
        is.null(cluster@cloudRuntime$serverPublicIp)
    )
}

update <- function(cluster){
    removeDiedServer(cluster)
    removeDiedWorkers(cluster)
    cluster
}
