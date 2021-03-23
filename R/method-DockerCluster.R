resetRuntimeServer <- function(cloudRuntime){
    cloudRuntime$serverPublicIp <- NULL
    cloudRuntime$serverPrivateIp <- NULL
    cloudRuntime$serverHandle <- NULL
}
removeRuntimeWorkers <- function(cloudRuntime, index){
    cloudRuntime$workerHandles <- cloudRuntime$workerHandles[-index]
    cloudRuntime$workerPublicIps <- cloudRuntime$workerPublicIps[-index]
    cloudRuntime$workerPrivateIps <- cloudRuntime$workerPrivateIps[-index]
}

removeDiedInstance <- function(cluster){
    provider <- cluster@cloudProvider
    cloudRuntime <- cluster@cloudRuntime
    if(!is.null(cloudRuntime$serverHandle)){
        serverAlive <-
            instanceAlive(provider, list(cloudRuntime$serverHandle), cluster@verbose)
        if(!serverAlive){
            resetRuntimeServer(cloudRuntime)
        }
    }
    if(length(cloudRuntime$workerHandles)!=0){
        workersAlive <- instanceAlive(provider, cloudRuntime$workerHandles, cluster@verbose)
        if(any(!workersAlive)){
            removeRuntimeWorkers(cloudRuntime, which(!workersAlive))
        }
    }
}

#
# cluster$startCluster()
# cluster$restartCluster()
#
# cluster$runServer()
# cluster$runWorkers()
# cluster$addWorkers(workerNum)
# cluster$stopCluster()
# cluster$restartServer()
# cluster$restartWorkers()
# cluster$status()

startCluster <- function(cluster){
    initialProvider(provider = cluster@cloudProvider, verbose = cluster@verbose)
    cluster$runServer()
    # if(){}

    cluster$runWorkers()


}


runCluster <- function(cluster){
    removeDiedInstance(cluster)
    workerNumber <- workerNumber - length(cluster@cloudRuntime$workerHandles)
    if(is.null(cluster@cloudRuntime$serverPrivateIp)){
        serverConfig <- createServerConfig(cluster@cloudConfig$serverContainer, cluster, verbose)
    }
    workerConfig <- createWorkerConfig(container, cluster, workerNumber, verbose = FALSE, ...)
}
