resetRuntimeServer <- function(cloudRuntime){
    cloudRuntime$clusterIp <- NULL
    cloudRuntime$serverHandle <- NULL
}
removeRuntimeWorkers <- function(cloudRuntime, index){
    cloudRuntime$workerHandles <- cloudRuntime$workerHandles[-index]
    cloudRuntime$workerPerHandle <- cloudRuntime$workerPerHandle[-index]
}

removeDiedInstance <- function(cluster){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudRuntime <- cluster@cloudRuntime
    if(!is.null(cloudRuntime$serverHandle)){
        serverAlive <-
            instanceAlive(provider, list(cloudRuntime$serverHandle), verbose=verbose)
        if(!serverAlive){
            resetRuntimeServer(cloudRuntime)
        }
    }
    if(length(cloudRuntime$workerHandles)!=0){
        workersAlive <- instanceAlive(provider, cloudRuntime$workerHandles, verbose=verbose)
        if(any(!workersAlive)){
            removeRuntimeWorkers(cloudRuntime, which(!workersAlive))
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
# cluster$status()
# cluster$getWorkerNumber()
startCluster <- function(cluster){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    initialProvider(provider = provider, verbose = verbose)

    ## remove the died instance
    removeDiedInstance(cluster)
    workerNumber <- cluster@cloudConfig$workerNum - length(cluster@cloudRuntime$workerHandles)

    startServer(cluster)
    if(workerNumber>0){
        verbosePrint(verbose>0, "Adding ",workerNumber," workers")
        startWorkers(cluster, workerNumber)
    }
}

#' Run the server
#'
#' Run the server and set the cluster IP
startServer <- function(cluster){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

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
            instanceHandle <- runContainers(provider,
                                            container=serverContainer,
                                            hardware=cloudConfig$serverHardware,
                                            containerNumber = 1L,
                                            verbose = verbose)
            stopifnot(length(instance@instanceHandle)==1)
            cloudRuntime$serverHandle <- instance@instanceHandle[[1]]
        }
        ## Get cluster IP
        cloudRuntime$clusterIp <- getClusterIp(
            provider,
            serverHandle = cloudRuntime$serverHandle,
            verbose = verbose
        )
    }
}

startWorkers <- function(cluster, workerNumber){
    if(workerNumber<=0){
        return()
    }
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime
    workerContainer <- configWorkerContainer(cloudConfig$workerContainer,
                                             cluster = cluster,
                                             verbose = verbose)
    instanceHandles <- runContainers(provider,
                                     container = workerContainer,
                                     hardware = cloudConfig$workerHardware,
                                     containerNumber = workerNumber,
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
    cloudRuntime$workerPerHandle <- c(cloudRuntime$workerHandles, as.integer(workerPerHandle))
}

stopCluster <- function(cluster){
    stopWorkers(cluster, Inf)
    stopServer(cluster)
}

stopServer<- function(cluster){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudRuntime <- cluster@cloudRuntime
    if(!is.null(cloudRuntime$serverHandle)){
        killInstances(provider,
                      instanceHandles = list(cloudRuntime$serverHandle),
                      verbose = verbose)
    }
}

stopWorkers<- function(cluster, workerNum){
    removeDiedInstance(cluster)
    cloudRuntime <- cluster@cloudRuntime
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
}

getWorkerNumber <- function(cluster){
    removeDiedInstance(cluster)
    sum(cluster@cloudRuntime$workerPerHandle)
}
