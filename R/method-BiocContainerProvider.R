setMethod("configServerContainerEnv", "BiocContainerProvider",
          function(container, cluster, verbose = FALSE){
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

setMethod("configWorkerContainerEnv", "BiocContainerProvider",
          function(container, cluster, workerNumber, verbose = FALSE){
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
                  queueName = cloudConfig$jobQueueName,
                  serverIp = serverIp,
                  serverPort = cloudConfig$serverPort,
                  serverPassword = cloudConfig$serverPassword,
                  sshPubKey = getSSHPubKeyValue(),
                  workerNum = workerNumber
              )
              container
          })




setMethod("registerParallelBackend", "BiocContainerProvider",
          function(container, cluster, verbose = FALSE, ...){
              cloudConfig <- cluster@cloudConfig
              cloudRuntime <- cluster@cloudRuntime

              queue <- cloudConfig$jobQueueName
              if(cloudConfig$serverClientSameNAT){
                  serverClientIP <- cloudRuntime$serverPrivateIp
              }else{
                  serverClientIP <- cloudRuntime$serverPublicIp
              }
              stopifnot(!is.null(serverClientIP))
              password <- cloudConfig$serverPassword

              doRedis::registerDoRedis(queue=queue,
                                       host = serverClientIP,
                                       password = password,
                                       port = cloudConfig$serverPort, ...)
          })

