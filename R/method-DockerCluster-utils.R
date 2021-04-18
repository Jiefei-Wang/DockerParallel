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
            IsDockerInstanceStopped(provider, list(cloudRuntime$serverHandle), verbose=verbose)
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
        stoppedWorkers <- IsDockerInstanceStopped(provider, cloudRuntime$workerHandles, verbose=verbose)
        if(any(stoppedWorkers)){
            removeRuntimeWorkers(cloudRuntime, which(stoppedWorkers))
        }
    }
}


addWorkersInternal <- function(cluster, workerNumber){
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)

    if(isServerRunning(cluster)){
        ## By default, we have 1 worker per container
        workerContainer <- configWorkerContainerEnv(
            container = .getWorkerContainer(cluster),
            cluster = cluster,
            workerNumber = 1,
            verbose = verbose
        )
        instanceHandles <- runDockerWorkers(provider,
                                      cluster = cluster,
                                      container = workerContainer,
                                      hardware = .getWorkerHardware(cluster),
                                      workerNumber = workerNumber,
                                      verbose = verbose)
        .addWorkerHandles(cluster, instanceHandles)
    }
    invisible(NULL)
}


removeWorkersInternal <- function(cluster, workerNumber){
    provider <- .getCloudProvider(cluster)
    cloudRuntime <- .getCloudRuntime(cluster)

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
        killDockerInstances(provider,
                      instanceHandles = cloudRuntime$workerHandles[killedInstanceIndex],
                      verbose = verbose)
        removeRuntimeWorkers(cloudRuntime, killedInstanceIndex)
    }
    invisible(NULL)
}

## Check if the cluster exists on the cloud
## and ask the user if reuse the same cluster
## return `TRUE` to indicate the caller should proceed
## `FALSE` means the caller should directly return
checkIfClusterExistAndAsk <- function(cluster){
    provider <- .getCloudProvider(cluster)
    verbose <- cluster$verbose
    verbosePrint(verbose>0, "Checking if the cluster exist")

    exist <- dockerClusterExists(provider=provider, cluster=cluster, verbose=verbose)
    if(exist){
        jobName <- .getJobQueueName(cluster)
        msg <- paste0(
            "The cluster with the job queue name <",
            jobName,
            "> exists on the cloud, do you want to reuse the same cluster?"
        )
        answer <- menu(c("Yes", "No", "Cancle"), title=msg)
        if(answer == 1){
            if(cluster$stopClusterOnExit){
                verbosePrint(verbose>0, "<stopClusterOnExit> will be set to FALSE")
                cluster$stopClusterOnExit <- FALSE
            }
            reconnectDockerCluster(provider=provider, cluster=cluster, verbose=verbose)
            return(TRUE)
        }
        if(answer == 2){
            return(TRUE)
        }
        if(answer == 0||answer== 3){
            return(FALSE)
        }
    }
    TRUE
}
