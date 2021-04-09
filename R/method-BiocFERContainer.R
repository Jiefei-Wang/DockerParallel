#' @describeIn configServerContainerEnv configure the server container
#' environment, only the environment `serverPort`, `serverPassword` and `sshPubKey`
#' will be set in this method
#' @export
setMethod("configServerContainerEnv", "BiocFERContainer",
          function(container, cluster, verbose = FALSE){
              if(length(container$RPackages)!=0){
                  warning("The server containder does not support installing R packages")
              }
              if(length(container$sysPackages)!=0){
                  warning("The server containder does not support installing system packages")
              }
              container <- container$copy()
              cloudConfig <- cluster@cloudConfig
              cloudRuntime <- cluster@cloudRuntime
              pubKeyValue <- getSSHPubKeyValue()
              stopifnot(!is.empty(cloudConfig$serverPort))
              container$environment <- combineList(
                  container$environment,
                  list(
                      serverPort = cloudConfig$serverPort,
                      serverPassword = cloudConfig$serverPassword,
                      sshPubKey = getSSHPubKeyValue()
                  )
              )
              container
          })

#' @describeIn configWorkerContainerEnv configure the worker container
#' @export
setMethod("configWorkerContainerEnv", "BiocFERContainer",
          function(container, cluster, workerNumber, verbose = FALSE){
              container <- container$copy()
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

              container$environment <- combineList(
                  container$environment,
                  list(
                      queueName = cloudConfig$jobQueueName,
                      serverIp = serverIp,
                      serverPort = cloudConfig$serverPort,
                      serverPassword = cloudConfig$serverPassword,
                      sshPubKey = getSSHPubKeyValue(),
                      workerNum = workerNumber,
                      RPackages = packPackages(container$RPackages),
                      sysPackages = paste0(container$sysPackages,collapse = ",")
                  )
              )
              container
          })



#' @rdname containerParallelBackend
#' @export
setMethod("registerParallelBackend", "BiocFERContainer",
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

#' @describeIn getServerContainer Get the Bioconductor foreach Redis container
#' @export
setMethod("getServerContainer", "BiocFERContainer",function(container, ...){
    BiocFERServerContainer(...)
})


