#' Configure the server container environment,
#'
#' Configure the server container environment, only the environment
#' `serverPort`, `serverPassword` and `sshPubKey` will be set in this method
#'
#' @inheritParams configServerContainerEnv
#' @return A `BiocFERContainer` object
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

#' Configure the worker container
#'
#' Configure the worker container
#'
#' @inheritParams configWorkerContainerEnv
#' @return A `BiocFERContainer` object
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


#' Register the foreach parallel backend
#'
#' Register the foreach parallel backend
#'
#' @inheritParams registerParallelBackend
#' @return No return value
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
              invisible(NULL)
          })

#' Get the Bioconductor foreach Redis container
#'
#' Get the Bioconductor foreach Redis container from the worker container
#'
#' @inheritParams getServerContainer
#' @return A `BiocFERContainer` server container
#' @export
setMethod("getServerContainer", "BiocFERContainer",function(workerContainer){
    BiocFERServerContainer()
})




containerExportedMethods <- c("getSysPackages", "setSysPackages", "addSysPackages",
                              "getRPackages", "setRPackages", "addRPackages")


#' Get the exported object
#'
#' Get the exported object. The objects are 'getSysPackages', 'setSysPackages',
#' 'addSysPackages', 'getRPackages', 'setRPackages' and 'addRPackages'. see details
#'
#' @details
#' The function `XSysPackages` can be used to install the system package for the
#' worker container before running the R worker. The package will be installed by
#' `apt-get install`.
#'
#' The function `XRPackages` will install the R packages for the container. The
#' package is installed via `AnVIL::install`. It will first try the fast binary installation,
#' then fallback to `BiocManager::install`. Therefore, you can also provide the GitHub package
#' to this function.
#'
#' Note that these function must be called before deploying the container.
#' Setting the packages will have no effect on the running container.
#'
#'
#' @inheritParams getExportedObject
#' @return For the exported function: The current package vector
#' @rdname BiocFERPackages
#' @export
setMethod("getExportedNames", "BiocFERContainer",
          function(x){
              containerExportedMethods
          }
)

#' @rdname BiocFERPackages
#' @export
setMethod("getExportedObject", "BiocFERContainer",
          function(x, name){
              if(!name%in%containerExportedMethods)
                  stop("Undefined object <",name,">")
              get(name)
          }
)

getSysPackages <- function(cluster){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$sysPackages
}
setSysPackages <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$sysPackages <- unique(packages)
    workerContainer$sysPackages
}
addSysPackages  <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$sysPackages <- unique(c(packages,workerContainer$sysPackages))
    workerContainer$sysPackages
}

getRPackages <- function(cluster){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$RPackages
}
setRPackages <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$RPackages <- unique(packages)
    workerContainer$RPackages
}
addRPackages  <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$RPackages <- unique(c(packages,workerContainer$RPackages))
    workerContainer$RPackages
}




