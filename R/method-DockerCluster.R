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
        serverAlive <-
            instanceAlive(provider, list(cloudRuntime$serverHandle), verbose=verbose)
        if(!serverAlive){
            resetRuntimeServer(cloudRuntime)
        }
    }
}

removeDiedWorkers <- function(cluster){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudRuntime <- cluster@cloudRuntime
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
# cluster$getWorkerNumber()
# cluster$status()
startCluster <- function(cluster){
    verbose <- cluster@verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    initialProvider(provider = provider, cluster=cluster, verbose = verbose)

    workerNumber <- cluster@cloudConfig$workerNum

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

    initialProvider(provider = provider, cluster=cluster, verbose = verbose)

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
}

startWorkers <- function(cluster, workerNumber){
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
        result <- killInstances(provider,
                                instanceHandles = list(cloudRuntime$serverHandle),
                                verbose = verbose)
        if(result){
            resetRuntimeServer(cloudRuntime)
        }
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

registerForeach <- function(cluster){

}


status <- function(cluster){
    cluster
}
