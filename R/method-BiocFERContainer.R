combineList <- function(x, newX){
    for(i in seq_along(newX)){
        x[[names(newX)[i]]] <- newX[[i]]
    }
    x
}

#' @describeIn configServerContainerEnv configure the server container
#' environment, only the environment `serverPort`, `serverPassword` and `sshPubKey`
#' will be set in this method
#' @export
setMethod("configServerContainerEnv", "BiocFERContainer",
          function(container, cluster, verbose = FALSE){
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
                      RPackages = paste0(container$RPackages,collapse = ","),
                      sysPackages = paste0(container$sysPackages,collapse = ",")
                  )
              )
              container
          })




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


setMethod("getExportedNames", "BiocFERContainer",
          function(x){
              c("getSysPackages", "setSysPackages", "addSysPackages",
                "getRPackages", "setRPackages", "addRPackages")
          }
)


setMethod("getExportedObject", "BiocFERContainer",
          function(x, name){
              get(name)
          }
)

getSysPackages <- function(cluster){
    workerContainer <- getWorkerContainer(cluster)
    workerContainer$sysPackages
}
setSysPackages <- function(cluster, packages){
    workerContainer <- getWorkerContainer(cluster)
    workerContainer$sysPackages <- packages
}
addSysPackages  <- function(cluster, packages){
    workerContainer <- getWorkerContainer(cluster)
    workerContainer$sysPackages <- c(packages,workerContainer$sysPackages)
}

getRPackages <- function(cluster){
    workerContainer <- getWorkerContainer(cluster)
    workerContainer$RPackages
}
setRPackages <- function(cluster, packages){
    workerContainer <- getWorkerContainer(cluster)
    workerContainer$RPackages <- packages
}
addRPackages  <- function(cluster, packages){
    workerContainer <- getWorkerContainer(cluster)
    workerContainer$RPackages <- c(packages,workerContainer$RPackages)
}




