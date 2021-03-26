setMethod("configServerContainer", "Container", function(container, cluster, verbose = FALSE, ...){
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime
    pubKeyValue <- getSSHPubKeyValue()
    stopifnot(!is.empty(cloudConfig$serverPort))
    container@environment <- list(
        serverPort = cloudConfig$serverPort,
        serverPassword = cloudConfig$serverPassword,
        sshPubKey = getSSHPubKeyValue()
    )
    container
})

setMethod("configWorkerContainer", "Container", function(container, cluster, verbose = FALSE, ...){
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime
    if(is.null(cloudRuntime$clusterIp)){
        stop("Fail to find cluster Ip")
    }
    stopifnot(!is.empty(cloudConfig$serverPort))

    container@environment <- list(
        clusterName = cloudConfig$clusterName,
        serverIp = cloudRuntime$clusterIp,
        serverPort = cloudConfig$serverPort,
        serverPassword = cloudConfig$serverPassword,
        sshPubKey = getSSHPubKeyValue()
    )
    container
})

setMethod("configWorkerNumber", "Container", function(container, workerNumber, verbose = FALSE, ...){
    container@environment[["workerNum"]] <- workerNumber
    container
})



setMethod("registerCluster", "Container", function(container, cluster, verbose = FALSE, ...){
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    queue <- cloudConfig$clusterName
    serverClientIP <- cloudRuntime$clusterIp
    password <- cloudConfig$serverPassword

    doRedis::registerDoRedis(queue=queue,
                             host = cloudRuntime$clusterIp,
                             password = cloudConfig$serverPassword,
                             port = cloudConfig$serverPort)
})

