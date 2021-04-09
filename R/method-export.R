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
makeDockerCluster <- function(cloudProvider,
                              workerContainer,
                              workerNumber = 1,
                              workerCpu = 1024, workerMemory = 2048, workerHardwareId = NULL,
                              serverCpu = 256, serverMemory = 2048, serverHardwareId = NULL,
                              cloudConfig = NULL,
                              cloudRuntime = NULL,
                              serverContainer = NULL,
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
        serverContainer <- getServerContainer(workerContainer)
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





