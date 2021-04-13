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

              serverPort <- .getServerPort(cluster)
              serverPassword <- .getServerPassword(cluster)
              sshPubKey <- getSSHPubKeyValue()

              stopifnot(!is.empty(serverPort))
              if(is.empty(serverPassword)){
                    stop("The server must be password protected!")
              }
              container$environment <- combineList(
                  container$environment,
                  list(
                      serverPort = serverPort,
                      serverPassword = serverPassword,
                      sshPubKey = sshPubKey
                  )
              )
              container
          })

#' @describeIn configWorkerContainerEnv configure the worker container
#' @export
setMethod("configWorkerContainerEnv", "BiocFERContainer",
          function(container, cluster, workerNumber, verbose = FALSE){
              container <- container$copy()

              queueName <- .getJobQueueName(cluster)
              serverPort <- .getServerPort(cluster)
              serverPassword <- .getServerPassword(cluster)
              sshPubKey <- getSSHPubKeyValue()
              RPackages <- packPackages(container$RPackages)
              sysPackages <- paste0(container$sysPackages,collapse = ",")
              if(.getServerWorkerSameLAN(cluster)){
                  serverIp <- .getServerPrivateIp(cluster)
              }else{
                  serverIp <- .getServerPublicIp(cluster)
              }

              if(is.null(serverIp)){
                  stop("Fail to find the server Ip")
              }
              stopifnot(!is.empty(serverPort))

              container$environment <- combineList(
                  container$environment,
                  list(
                      queueName = queueName,
                      serverIp = serverIp,
                      serverPort = serverPort,
                      serverPassword = serverPassword,
                      sshPubKey = sshPubKey,
                      workerNum = workerNumber,
                      RPackages = RPackages,
                      sysPackages = sysPackages
                  )
              )
              container
          })


#' @rdname containerParallelBackend
#' @export
setMethod("registerParallelBackend", "BiocFERContainer",
          function(container, cluster, verbose = FALSE, ...){
              queue <- .getJobQueueName(cluster)
              password <- .getServerPassword(cluster)
              serverPort <- .getServerPort(cluster)
              if(.getServerClientSameLAN(cluster)){
                  serverClientIP <- .getServerPrivateIp(cluster)
              }else{
                  serverClientIP <- .getServerPublicIp(cluster)
              }

              if(is.null(serverClientIP)){
                  stop("Fail to find the server Ip")
              }
              stopifnot(!is.empty(serverPort))

              doRedis::registerDoRedis(queue=queue,
                                       host = serverClientIP,
                                       password = password,
                                       port = serverPort, ...)
          })

#' @describeIn getServerContainer Get the Bioconductor foreach Redis container
#' @export
setMethod("getServerContainer", "BiocFERContainer",function(container, ...){
    BiocFERServerContainer(...)
})


