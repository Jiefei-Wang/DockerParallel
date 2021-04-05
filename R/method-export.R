#' Wait the instances until they are running
#'
#' Wait the instances until they are running
#'
#' @returns
#' a logical value indicating if the instances are running
#'
#' @export
waitInstanceUntilRunning<-function(provider, instanceHandles, progressBar = FALSE, waitTime = 1){
    on.exit(
        {
            if(progressBar){
                close(pb)
            }
        }
    )
    if(progressBar){
        pb <- txtProgressBar(min=0,max = length(instanceHandles), style=3)
    }
    while(TRUE){
        instanceStatus <- getInstanceStatus(provider=provider,
                                            instanceHandles = instanceHandles,
                                            verbose = FALSE)
        if(all(instanceStatus=="running")){
            return(TRUE)
        }
        if(any(instanceStatus=="stopped")){
            return(FALSE)
        }
        if(progressBar){
            setTxtProgressBar(pb, sum(instanceStatus=="running"))
        }
        Sys.sleep(waitTime)
    }
}


#' @export
getCloudProvider <- function(cluster){
    cluster@cloudProvider
}
#' @export
getServerContainer <- function(cluster){
    cluster@serverContainer
}

#' @export
getWorkerContainer <- function(cluster){
    cluster@workerContainer
}

#' @export
getCloudRuntime <- function(cluster){
    cluster@cloudRuntime
}

#' @export
getCloudConfig <- function(cluster){
    cluster@cloudConfig
}


#' @export
makeDockerCluster <- function(workerNumber = 1,
                              workerCpu = 1024, workerMemory = 2048, workerHardwareId = NULL,
                              serverCpu = 256, serverMemory = 2048, serverHardwareId = NULL,
                              cloudProvider = NULL,
                              cloudConfig = NULL,
                              cloudRuntime = NULL,
                              serverContainer = NULL,
                              workerContainer = NULL,
                              stopClusterOnExit = TRUE,
                              verbose = 1){
    if(is.null(cloudConfig)){
        cloudConfig <- CloudConfig()
        cloudConfig$workerNumber <- as.integer(workerNumber)
        cloudConfig$workerHardware@cpu <- workerCpu
        cloudConfig$workerHardware@memory <- workerMemory
        cloudConfig$workerHardware@id <- workerHardwareId
        cloudConfig$serverHardware@cpu <- serverCpu
        cloudConfig$serverHardware@memory <- serverMemory
        cloudConfig$serverHardware@id <- serverHardwareId
    }
    if(is.null(cloudRuntime)){
        cloudRuntime <- CloudRuntime()
    }
    if(is.null(serverContainer)){
        serverContainer <- getBiocFERServerContainer()
    }
    if(is.null(workerContainer)){
        workerContainer <- getBiocFERWorkerContainer()
    }
    if(is.null(cloudProvider)){
        cloudProvider <- ECSCloudProvider()
    }

    cluster <- dockerCluster(
        cloudProvider = cloudProvider,
        cloudConfig = cloudConfig,
        cloudRuntime = cloudRuntime,
        serverContainer = serverContainer,
        workerContainer = workerContainer
    )
    cluster$verbose <- verbose
    cluster$stopClusterOnExit <- stopClusterOnExit
    cluster
}





