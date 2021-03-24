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
