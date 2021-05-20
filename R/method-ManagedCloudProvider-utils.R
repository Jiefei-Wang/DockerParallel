removeWorkersHandle <- function(provider, index){
    if(length(index)!=0){
        cloudRuntime$workerHandles <- provider$workerHandles[-index]
        cloudRuntime$workerPerHandle <- provider$workerPerHandle[-index]
    }
}


removeDiedWorkers <- function(cluster){
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)
    cloudRuntime <- .getCloudRuntime(cluster)
    if(length(cloudRuntime$workerHandles)!=0){
        stoppedWorkers <- IsDockerInstanceStopped(provider, cloudRuntime$workerHandles, verbose=verbose)
        if(any(stoppedWorkers)){
            removeRuntimeWorkers(cloudRuntime, which(stoppedWorkers))
        }
    }
}

getManagedWorkerNumber <- function(provider){
    sum(provider$workerPerHandle)
}

getWorkerHandles <- function(provider){
    workerHandles <- provider$workerHandles
    workerPerHandle <- provider$workerPerHandle
    x <- lapply(seq_along(workerHandles),
                function(i)rep(workerHandles[i],workerPerHandle[i])
    )
    unlist(x)
}

addWorkerHandles <- function(provider, handles){
    allHandles <- c(getWorkerHandles(provider), handles)
    info <- table(allHandles)
    provider$workerHandles <- names(info)
    provider$workerPerHandle <- as.vector(a)
    invisible(NULL)
}

addWorkersInternal <- function(cluster, container, hardware, workerNumber){
    provider <- .getCloudProvider(cluster)
    instanceHandles <- runDockerWorkers(provider = provider,
                                        cluster = cluster,
                                        container = container,
                                        hardware = hardware,
                                        workerNumber = workerNumber,
                                        verbose = cluster$verbose)
    addWorkerHandles(provider, instanceHandles)
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


removeWorkersInternal <- function(cluster, workerNumber){
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)

    ## Find which instances will be killed while satisfying
    ## that the killed workers is less than or equal to workerNumber
    KnapsackSolution <-
        myknapsack(provider$workerPerHandle,
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
        success <- killDockerInstances(provider,
                                       instanceHandles = provider$workerHandles[killedInstanceIndex],
                                       verbose = verbose)
        if(any(!success)){
            warning("Fail to kill some worker instances")
        }
        removeWorkersHandle(provider, killedInstanceIndex[success])
    }
    invisible(NULL)
}
