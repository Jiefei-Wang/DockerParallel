removeWorkersHandle <- function(provider, index){
    if(length(index)!=0){
        provider$workerHandles <- provider$workerHandles[-index]
        provider$workerPerHandle <- provider$workerPerHandle[-index]
    }
}

removeDiedWorkers <- function(cluster){
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)
    workerStatus <- NULL
    if(length(provider$workerHandles)!=0){
        workerStatus <- getDockerWorkerStatus(provider, cluster, provider$workerHandles, verbose=verbose)
        if(any(workerStatus=="stopped")){
            removeWorkersHandle(provider, which(workerStatus=="stopped"))
            workerStatus <- workerStatus[workerStatus != "stopped"]
        }
    }
    workerStatus
}

getManagedWorkerNumber <- function(provider){
    sum(provider$workerPerHandle)
}



addManagedWorkersInternal <- function(cluster, container, hardware, workerNumber){
    provider <- .getCloudProvider(cluster)
    workerHandles <- runDockerWorkerContainers(provider = provider,
                                               cluster = cluster,
                                               container = container,
                                               hardware = hardware,
                                               workerNumber = workerNumber,
                                               verbose = cluster$verbose)
    addManagedWorkerHandles(provider, workerHandles)
    invisible(NULL)
}


myknapsack <- function (workerPerHandle, killedWorkerNum)
{
    idx <- which(workerPerHandle<=killedWorkerNum)
    if(length(idx)==0){
        return(list(capacity=0, indices = c()))
    }
    KnapsackSolution <-
        adagio::knapsack(workerPerHandle[idx],
                         workerPerHandle[idx],
                         killedWorkerNum)
    KnapsackSolution$indices <- idx[KnapsackSolution$indices]
    KnapsackSolution
}


removeManagedWorkersInternal <- function(cluster, workerNumber){
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)

    ## Find which workers will be killed while satisfying
    ## that the killed workers is less than or equal to workerNumber
    KnapsackSolution <-
        myknapsack(provider$workerPerHandle,
                   workerNumber)
    killedWorkerNumber <- KnapsackSolution$capacity
    killedInstanceIndex <- KnapsackSolution$indices
    if(killedWorkerNumber < workerNumber){
        if(killedWorkerNumber==0){
            message("No worker can be killed as all containers have more than ",
                    workerNumber,
                    " workers")
        }else{
            message("Only ", killedWorkerNumber,
                    " workers will be killed as multiple workers share the same container")
        }
    }
    if(killedWorkerNumber!=0){
        success <- killDockerWorkerContainers(
            provider,
            cluster,
            workerHandles = provider$workerHandles[killedInstanceIndex],
            verbose = verbose)
        if(any(!success)){
            warning("Fail to kill some worker containers")
        }
        removeWorkersHandle(provider, killedInstanceIndex[success])
    }
    invisible(NULL)
}

