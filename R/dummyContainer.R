.DummyContainer <- setRefClass(
    "DummyContainer",
    fields = list(),
    contains = "DockerContainer"
)

#' A dummy container
#'
#' A dummy container. It is for purely testing purpose.
#'
#' @param image The image for the container
#' @param backend The parallel backend for the container
#' @param maxWorkerNum The maximum worker number
#' @examples
#' DummyWorkerContainer()
#' @rdname DummyContainer
#' @export
DummyWorkerContainer <- function(image = "workerImage", backend = "testBackend", maxWorkerNum = 123L){
    .DummyContainer(image = image, backend = backend, maxWorkerNum = as.integer(maxWorkerNum))
}

#' @rdname DummyContainer
#' @export
DummyServerContainer <- function(image = "serverImage", backend = "testBackend"){
    .DummyContainer(image = image, backend = backend, maxWorkerNum = 1L)
}

combineList <- function(x, newX){
    for(i in seq_along(newX)){
        x[[names(newX)[i]]] <- newX[[i]]
    }
    x
}

#' @describeIn configServerContainerEnv method for the dummy container
#' @export
setMethod("configServerContainerEnv", "DummyContainer",
          function(container, cluster, verbose = FALSE){
              serverPort <- .getServerPort(cluster)
              serverPassword <- .getServerPassword(cluster)
              sshPubKey <- getSSHPubKeyValue()

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

#' @describeIn configWorkerContainerEnv method for the dummy container
#' @export
setMethod("configWorkerContainerEnv", "DummyContainer",
          function(container, cluster, workerNumber, verbose = FALSE){
              container <- container$copy()
              queueName <- .getJobQueueName(cluster)
              serverPassword <- .getServerPassword(cluster)
              sshPubKey <- getSSHPubKeyValue()
              if(.getServerWorkerSameLAN(cluster)){
                  serverIp <- .getServerPrivateIp(cluster)
                  serverPort <- .getServerPrivatePort(cluster)
              }else{
                  serverIp <- .getServerPublicIp(cluster)
                  serverPort <- .getServerPublicPort(cluster)
              }

              if(length(serverIp)==0 || length(serverPort)==0){
                  stop("Fail to find the server Ip or port")
              }

              container$environment <- combineList(
                  container$environment,
                  list(
                      queueName = queueName,
                      serverIp = serverIp,
                      serverPort = serverPort,
                      serverPassword = serverPassword,
                      sshPubKey = sshPubKey,
                      workerNum = workerNumber
                  )
              )
              container
          })

#' @describeIn getServerContainer method for the dummy container
#' @export
setMethod("getServerContainer", "DummyContainer",function(workerContainer){
    DummyServerContainer()
})

#' @describeIn containerParallelBackend method for the dummy container
#' @export
setMethod("registerParallelBackend", "DummyContainer",
          function(container, cluster, verbose, ...){})

#' @describeIn containerParallelBackend method for the dummy container
#' @export
setMethod("deregisterParallelBackend", "DummyContainer",
          function(container, cluster, verbose, ...){})


