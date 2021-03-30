setMethod("configServerContainerEnv", "Container", function(container, cluster, verbose = FALSE, ...){
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

setMethod("configWorkerContainerEnv", "Container", function(container, cluster, workerNumber, verbose = FALSE, ...){
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    if(cloudConfig$serverWorkerSameNAT){
        serverIp <- cloudRuntime$serverPrivateIp
    }else{
        serverIp <- cloudRuntime$serverPublicIp
    }
    if(is.null(serverIp)){
        stop("Fail to find the server Ip")
    }
    stopifnot(!is.empty(cloudConfig$serverPort))

    container@environment <- list(
        clusterName = cloudConfig$clusterName,
        serverIp = serverIp,
        serverPort = cloudConfig$serverPort,
        serverPassword = cloudConfig$serverPassword,
        sshPubKey = getSSHPubKeyValue(),
        workerNum = workerNumber
    )
    container
})




setMethod("registerParallelBackend", "Container", function(container, cluster, verbose = FALSE, ...){
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    queue <- cloudConfig$clusterName
    if(cloudConfig$serverClientSameNAT){
        serverClientIP <- cloudRuntime$serverPrivateIp
    }else{
        serverClientIP <- cloudRuntime$serverPublicIp
    }
    password <- cloudConfig$serverPassword

    doRedis::registerDoRedis(queue=queue,
                             host = serverClientIP,
                             password = password,
                             port = cloudConfig$serverPort)
})

