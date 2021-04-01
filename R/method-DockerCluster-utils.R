resetRuntimeServer <- function(cloudRuntime){
    cloudRuntime$serverPublicIp <- NULL
    cloudRuntime$serverPrivateIp <- NULL
    cloudRuntime$serverHandle <- NULL
}
removeRuntimeWorkers <- function(cloudRuntime, index){
    cloudRuntime$workerHandles <- cloudRuntime$workerHandles[-index]
    cloudRuntime$workerPerHandle <- cloudRuntime$workerPerHandle[-index]
}

removeDiedServer <- function(cluster){
    verbose <- cluster$verbose
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
    verbose <- cluster$verbose
    provider <- cluster@cloudProvider
    cloudRuntime <- cluster@cloudRuntime
    if(length(cloudRuntime$workerHandles)!=0){
        stoppedWorkers <- IsInstanceStopped(provider, cloudRuntime$workerHandles, verbose=verbose)
        if(any(stoppedWorkers)){
            removeRuntimeWorkers(cloudRuntime, which(stoppedWorkers))
        }
    }
}


addWorkersInternal <- function(cluster, workerNumber){
    verbose <- cluster$verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    if(isServerRunning(cluster)){
        ## By default, we have 1 worker per container
        workerContainer <- configWorkerContainerEnv(
            container = cluster@workerContainer,
            cluster = cluster,
            workerNumber = 1,
            verbose = verbose
        )
        instanceHandles <- runWorkers(provider,
                                      cluster = cluster,
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
        cloudRuntime$workerPerHandle <- c(cloudRuntime$workerPerHandle,
                                          as.integer(workerPerHandle))
    }
    invisible(NULL)
}


removeWorkersInternal <- function(cluster, workerNumber){
    cloudRuntime <- cluster@cloudRuntime
    provider <- cluster@cloudProvider
    verbose <- cluster$verbose

    ## Find which instances will be killed while satisfying
    ## that the killed workers is less than or equal to workerNumber
    KnapsackSolution <-
        myknapsack(cloudRuntime$workerPerHandle,
                         workerNumber)
    killedWorkerNumber <- KnapsackSolution$capacity
    killedInstanceIndex <- KnapsackSolution$indices
    if(killedWorkerNumber < workerNumber){
        if(killedWorkerNumber==0){
            message("No worker can be killed as all instances have more than ",
                    workerNumber,
                    " workers")
        }else{
            message("Only ", killedWorkerNumber,
                    " workers will be killed as multiple workers share the same instance")
        }
    }
    if(killedWorkerNumber!=0){
        killInstances(provider,
                      instanceHandles = cloudRuntime$workerHandles[killedInstanceIndex],
                      verbose = verbose)
        removeRuntimeWorkers(cloudRuntime, killedInstanceIndex)
    }
    invisible(NULL)
}
