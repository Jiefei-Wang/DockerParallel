resetRuntimeServer <- function(cloudRuntime){
    cloudRuntime$clusterIp <- NULL
    cloudRuntime$serverHandle <- NULL
}
removeRuntimeWorkers <- function(cloudRuntime, index){
    cloudRuntime$workerHandles <- cloudRuntime$workerHandles[-index]
    cloudRuntime$workerPerHandle <- cloudRuntime$workerPerHandle[-index]
}

removeDiedServer <- function(cluster){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudRuntime <- cluster@cloudRuntime
    if(!is.null(cloudRuntime$serverHandle)){
        stoppedServer <-
            IsInstanceStopped(provider, list(cloudRuntime$serverHandle), verbose=verbose)
        if(stoppedServer){
            resetRuntimeServer(cloudRuntime)
        }
    }
}

removeDiedWorkers <- function(cluster){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudRuntime <- cluster@cloudRuntime
    if(length(cloudRuntime$workerHandles)!=0){
        stoppedWorkers <- IsInstanceStopped(provider, cloudRuntime$workerHandles, verbose=verbose)
        if(any(stoppedWorkers)){
            removeRuntimeWorkers(cloudRuntime, which(stoppedWorkers))
        }
    }
}



#
# cluster$startCluster()
# cluster$startServer()
# cluster$startWorkers(workerNum)
# cluster$stopCluster()
# cluster$stopServer()
# cluster$stopWorkers(workerNum)
# cluster$getWorkerNumber()
# cluster$status()
startCluster <- function(cluster){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    startServer(cluster)

    workerNumber <- cluster@cloudConfig$workerNum
    if(workerNumber>0){
        verbosePrint(verbose>0, "Running ",workerNumber," workers")
        setWorkerNumber(cluster, workerNumber)
    }

    registerBackend(cluster)
    invisible(NULL)
}

#' Run the server
#'
#' Run the server and set the cluster IP
startServer <- function(cluster){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    initialProvider(provider = provider, cluster=cluster, verbose = verbose)

    removeDiedServer(cluster)
    if(is.null(cloudRuntime$clusterIp)){
        ## Run the server if it does not exist
        if(is.null(cloudRuntime$serverHandle)){
            if(is.null(cloudConfig$serverContainer)){
                stop("No server container can be found")
            }
            verbosePrint(verbose, "Launching server")
            serverContainer <- configServerContainer(cloudConfig$serverContainer,
                                                     cluster = cluster,
                                                     verbose = verbose)
            instanceHandle <- runServer(provider,
                                        container=serverContainer,
                                        hardware=cloudConfig$serverHardware,
                                        verbose = verbose)
            cloudRuntime$serverHandle <- instanceHandle

            cloudRuntime$clusterIp <- getClusterIp(
                provider,
                serverHandle = cloudRuntime$serverHandle,
                verbose = verbose
            )
        }
    }
    invisible(NULL)
}

setWorkerNumber <- function(cluster, workerNumber){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    currentWorkerNumber <- getWorkerNumber(cluster)
    workerOffset <- workerNumber - currentWorkerNumber
    if(workerOffset!=0){
        if(workerOffset > 0){
            addWorkers(cluster, workerOffset)
        }else{
            removeWorkers(cluster, abs(workerOffset))
        }
    }
}


addWorkers <- function(cluster, workerNumber){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    initialProvider(provider = provider, cluster=cluster, verbose = verbose)

    if(workerNumber<=0){
        return()
    }

    workerContainer <- configWorkerContainer(cloudConfig$workerContainer,
                                             cluster = cluster,
                                             verbose = verbose)
    instanceHandles <- runWorkers(provider,
                                  container = workerContainer,
                                  hardware = cloudConfig$workerHardware,
                                  workerNumber = workerNumber,
                                  verbose = verbose)
    ## Count the number of workers per handle
    uniqueHandles <- unique(instanceHandles)
    workerPerHandle <- rep(0, length(uniqueHandles))
    for(i in seq_along(uniqueHandles)){
        workerPerHandle[i] <- sum(
            vapply(instanceHandles,
                   function(x)identical(x,uniqueHandles[[i]]), logical(1))
        )
    }
    cloudRuntime$workerHandles <- c(cloudRuntime$workerHandles, uniqueHandles)
    cloudRuntime$workerPerHandle <- c(cloudRuntime$workerPerHandle, as.integer(workerPerHandle))

    invisible(NULL)
}


removeWorkers<- function(cluster, workerNum){
    cloudRuntime <- cluster@cloudRuntime
    provider <- cluster@cloudProvider
    verbose <- cluster@verbose
    ## Find which instances will be killed while satisfying
    ## that the killed workers is less than or equal to workerNum
    KnapsackSolution <-
        adagio::knapsack(cloudRuntime$workerPerHandle,
                         cloudRuntime$workerPerHandle,
                         workerNum)
    killedWorkerNumber <- KnapsackSolution$capacity
    killedInstanceIndex <- KnapsackSolution$indices
    if(killedWorkerNumber < workerNum){
        if(killedWorkerNumber==0){
            message("No worker can be killed as all instances have more than ",workerNum," workers")
        }else{
            message(killedWorkerNumber,
                    " workers will be killed as multiple workers share the same instance")
        }
    }
    killInstances(provider,
                  instanceHandles = cloudRuntime$workerHandles[killedInstanceIndex],
                  verbose = verbose)
    removeRuntimeWorkers(cloudRuntime, killedInstanceIndex)
    invisible(NULL)
}



stopCluster <- function(cluster){
    deregisterBackend(cluster)
    setWorkerNumber(cluster, 0)
    stopServer(cluster)
    invisible(NULL)
}

stopServer<- function(cluster){
    verbose <- cluster@verbose
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

registerBackend <- function(cluster){
    registerParallelBackend(container = cluster@cloudConfig$workerContainer,
                            cluster = cluster,
                            verbose = cluster@verbose)
    invisible(NULL)
}

deregisterBackend <- function(cluster){
    deregisterParallelBackend(container = cluster@cloudConfig$workerContainer,
                              cluster = cluster,
                              verbose = cluster@verbose)
    invisible(NULL)
}


update <- function(cluster){
    removeDiedServer(cluster)
    removeDiedWorkers(cluster)
    cluster
}
