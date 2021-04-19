clusterMethods <- c(
    "startCluster",
    "stopCluster",
    "startServer",
    "stopServer",
    "setWorkerNumber",
    "getWorkerNumber",
    "getExpectedWorkerNumber",
    "addWorkers",
    "removeWorkers",
    "reconnect",
    "update",
    "registerBackend",
    "deregisterBackend",
    "isServerRunning"
)

clusterObjects <- c("cloudProvider", "serverContainer", "workerContainer")

clusterOptions <- c(
    "verbose",
    "stopClusterOnExit"
)

## Constructor
dockerCluster <- function(cloudProvider,
                          serverContainer,
                          workerContainer,
                          cloudConfig = CloudConfig(),
                          cloudRuntime = CloudRuntime(),
                          verbose = 1,
                          stopClusterOnExit = TRUE){
    settings <- new.env(parent = emptyenv())
    settings$verbose <- verbose
    settings$stopClusterOnExit <- stopClusterOnExit
    settings$parallelBackendRegistered <- FALSE

    cluster <- .DockerCluster(cloudProvider = cloudProvider,
                              cloudConfig = cloudConfig,
                              cloudRuntime = cloudRuntime,
                              serverContainer = serverContainer,
                              workerContainer = workerContainer,
                              settings = settings
    )
    cluster <- configNATStatus(cluster)
    settings$cluster <- cluster
    reg.finalizer(settings, DockerCluster.finalizer, onexit = TRUE)
    cluster
}

#' Create a docker cluster
#'
#' Create a docker cluster. The user needs to provide a cloud provider and
#' a worker container to make it work.
#'
#' @param cloudProvider A `CloudProvider` object, the cloud that the container will be
#' deployed
#' @param workerContainer A `DockerContainer` object, the object that defines the worker container
#' @param workerNumber Integer, the number of workers in the cluster
#' @param serverCpu,workerCpu Integer, the CPU unit used by the server or each worker.
#' 1024 CPU unit corresponds to a physical CPU core.
#' @param serverMemory,workerMemory Integer, the memory used by the server or each worker in MB
#' @param cloudConfig A `CloudConfig` object. The object that stores the cloud information such
#' as server password, port, network status. Generally there is no need to provide this object
#' unless you want to customize the cloud setting.
#' @param cloudRuntime A `CloudRuntime` object. The obect that stores the cloud runtime data such
#' as server IP, handle and worker handles. Generally there is no need to provide this object
#' unless you want to customize the cloud setting.
#' @param serverContainer A `DockerContainer` object, the object that defines the server container.
#' If the value is `NULL`, the server container will be obtained from the worker container via
#' `getServerContainer(workerContainer)`
#' @param stopClusterOnExit Logical, whether to stop the cluster when the cluster has been removed
#' from the R session. The default value is `TRUE`.
#' @param verbose Integer, the verbose level
#'
#' @details
#' This is the core function of the `DockerParallel` package which defines the cluster object.
#' To user the function, you need to at least provide the cloud provider and worker container.
#' Currently we have `ECSFargateProvider` and `BiocFERContainer`, see example.
#'
#' @examples
#' \dontrun{
#' ## Load the ECS fargate provider
#' library(ECSFargateProvider)
#' provider <- ECSFargateProvider()
#'
#' ## Load the bioconductor foreach redis container
#' container <- BiocFERWorkerContainer()
#'
#' ## Define a cluster with 2 workers,
#' ## each worker use one fourth CPU core and 512 MB memory
#' cluster <- makeDockerCluster(cloudProvider = provider,
#'                              workerContainer = container,
#'                              workerNumber = 2,
#'                              workerCpu = 256, workerMemory = 512)
#' ## Start the cluster
#' cluster$startCluster()
#'
#' ## rescale the worker number
#' cluster$setWorkerNumber(4)
#'
#' ## Use foreach to do the parallel computing
#' library(foreach)
#' getDoParWorkers()
#' foreach(x= 1:4)%dopar%{
#'     Sys.info()
#' }
#' }
#' @return A `DockerCluster` object
#' @export
makeDockerCluster <- function(cloudProvider,
                              workerContainer,
                              workerNumber = 1,
                              workerCpu = 1024, workerMemory = 2048, workerHardwareId = NULL,
                              serverCpu = 256, serverMemory = 2048, serverHardwareId = NULL,
                              cloudConfig = NULL,
                              cloudRuntime = NULL,
                              serverContainer = NULL,
                              stopClusterOnExit = TRUE,
                              verbose = 1){
    if(is.null(cloudConfig)){
        cloudConfig <- CloudConfig()
        cloudConfig$workerNumber <- as.integer(workerNumber)
        cloudConfig$workerHardware@cpu <- workerCpu
        cloudConfig$workerHardware@memory <- workerMemory
        cloudConfig$workerHardware@id <- workerHardwareId
        cloudConfig$serverHardware@cpu <- serverCpu
        cloudConfig$serverHardware@memory <- serverMemory
        cloudConfig$serverHardware@id <- serverHardwareId
    }
    if(is.null(cloudRuntime)){
        cloudRuntime <- CloudRuntime()
    }
    if(is.null(serverContainer)){
        serverContainer <- getServerContainer(workerContainer)
    }

    cluster <- dockerCluster(
        cloudProvider = cloudProvider,
        cloudConfig = cloudConfig,
        cloudRuntime = cloudRuntime,
        serverContainer = serverContainer,
        workerContainer = workerContainer
    )
    cluster$verbose <- verbose
    cluster$stopClusterOnExit <- stopClusterOnExit
    cluster
}



#' Common `DockerCluster` parameter
#'
#' Common `DockerCluster` parameter
#'
#' @param x The `DockerCluster` object
#' @param name Character, the name of the exported object
#' @param object The `DockerCluster` object
#'
#' @rdname DockerCluster-common-parameters
#' @name DockerCluster-common-parameters
NULL


#' Show the exported object names
#'
#' Show the exported object names
#'
#' @inheritParams DockerCluster-common-parameters
#' @return A character vector
#' @export
setMethod(f = "names",signature = "DockerCluster",
          definition = function(x){
              c(clusterMethods,
                clusterObjects,
                clusterOptions
              )
          })
#' Get the exported object
#'
#' Get the exported object
#'
#' @inheritParams DockerCluster-common-parameters
#' @export
setMethod(f = "$",signature = "DockerCluster",
          definition = function(x, name){
              stopifnot(name %in% names(x))
              if(name%in%clusterOptions){
                  object <- x@settings[[name]]
              }
              if(name%in%clusterObjects){
                  object <- .ClusterMethodGetter(cluster = x,
                                                 object = slot(x, name))
              }
              if(name%in%clusterMethods){
                  object <- get(name)
                  object <- createTempFunction(object, x)
              }
              object
          }
)

#' Set the value of the exported object
#'
#' Set the value of the exported object
#'
#' @inheritParams DockerCluster-common-parameters
#' @param value The value of the exported object
#' @export
setMethod(f = "$<-",signature = "DockerCluster",
          definition = function(x, name, value){
              stopifnot(
                  name%in% clusterObjects||
                      name%in% clusterOptions
              )
              if(name%in%clusterOptions){
                  x@settings[[name]] <- value
              }
              if(name%in%clusterObjects){
                  slot(x, name) <- value
              }
              x
          }
)

#' Print the DockerCluster object
#'
#' Print the DockerCluster object
#'
#' @inheritParams DockerCluster-common-parameters
#'
#' @export
setMethod(f = "show",signature = "DockerCluster",
          definition = function(object){
              cloudRuntime <- object@cloudRuntime
              isServerRunning <- object$isServerRunning()

              publicIp <- ifelse(is.null(cloudRuntime$serverPublicIp) ,
                                 "NULL", cloudRuntime$serverPublicIp)
              privateIp <- ifelse(is.null(cloudRuntime$serverPrivateIp) ,
                                  "NULL", cloudRuntime$serverPrivateIp)

              cat("Server status:     ", ifelse(isServerRunning ,"Running", "Stopped"), "\n")
              if(isServerRunning){
                  cat("Server public IP:  ", publicIp, "\n")
                  cat("Server private IP: ", privateIp, "\n")
              }
              cat("Worker Number:     ", getWorkerNumber(object), "/",
                  getExpectedWorkerNumber(object), " (running/expected)\n")
              invisible(NULL)
          })

####################################################
## Some internal functions
####################################################
# cluster$startCluster()
# cluster$startServer()
# cluster$startWorkers(workerNum)
# cluster$stopCluster()
# cluster$stopServer()
# cluster$stopWorkers(workerNum)
# cluster$getWorkerNumber()
# cluster$status()

startCluster <- function(cluster, ...){
    verbose <- cluster$verbose

    ## Check if the cluster exists on the cloud
    ## and ask user what to do
    answer <- checkIfClusterExistAndAsk(cluster)
    if(!answer)
        return(invisible(NULL))

    ## Start the server
    startServer(cluster)

    ## Start the worker
    workerNumber <- cluster@cloudConfig$workerNumber
    verbosePrint(verbose>0, "The cluster has ",workerNumber," workers")
    setWorkerNumber(cluster, workerNumber)

    ## Register backend
    registerBackend(cluster, ...)
    invisible(NULL)
}

#' Run the server
#'
#' Run the server and set the cluster IP
startServer <- function(cluster){
    verbose <- cluster$verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    initializeProvider(provider = provider, cluster=cluster, verbose = verbose)

    removeDiedServer(cluster)
    if(!isServerRunning(cluster)){
        ## Run the server if it does not exist
        if(!is.null(cloudRuntime$serverHandle)){
            stop("Server handle exists but the cluster IP does not")
        }
        if(is.null(cluster@serverContainer)){
            stop("No server container can be found")
        }
        verbosePrint(verbose, "Launching server")
        serverContainer <-
            configServerContainerEnv(
                .getServerContainer(cluster),
                cluster = cluster,
                verbose = verbose
            )
        instanceHandle <- runDockerServer(provider = provider,
                                    cluster = cluster,
                                    container = serverContainer,
                                    hardware = cloudConfig$serverHardware,
                                    verbose = verbose)
        cloudRuntime$serverHandle <- instanceHandle
        serverIp <- getDockerInstanceIps(
            provider,
            instanceHandles = list(instanceHandle),
            verbose = verbose
        )
        if(serverIp$privateIp==""){
            cloudRuntime$serverPrivateIp <- NULL
        }else{
            cloudRuntime$serverPrivateIp <-serverIp$privateIp
        }
        if(serverIp$publicIp==""){
            cloudRuntime$serverPublicIp <- NULL
        }else{
            cloudRuntime$serverPublicIp <-serverIp$publicIp
        }
    }
    invisible(NULL)
}

setWorkerNumber <- function(cluster, workerNumber){
    stopifnot(workerNumber>=0)
    workerNumber <- as.integer(workerNumber)
    cloudConfig <- cluster@cloudConfig

    cloudConfig$workerNumber <- workerNumber
    workerOffset <- cloudConfig$workerNumber - getWorkerNumber(cluster)
    if(workerOffset > 0){
        addWorkersInternal(cluster, workerOffset)
    }
    if(workerOffset < 0){
        removeWorkersInternal(cluster, abs(workerOffset))
    }
    invisible(NULL)
}


addWorkers <- function(cluster, workerNumber){
    workerNumber <- as.integer(workerNumber)

    verbose <- cluster$verbose
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    cloudRuntime <- cluster@cloudRuntime

    initializeProvider(provider = provider, cluster=cluster, verbose = verbose)
    ## get the expected worker number
    cloudConfig$workerNumber <- cloudConfig$workerNumber + workerNumber

    ## get the number of workers that will be added
    requiredAddedNumber <- cloudConfig$workerNumber - getWorkerNumber(cluster)
    if(requiredAddedNumber<=0){
        return(invisible(NULL))
    }
    addWorkersInternal(cluster, requiredAddedNumber)
    invisible(NULL)
}


removeWorkers<- function(cluster, workerNumber){
    workerNumber <- as.integer(workerNumber)
    cloudRuntime <- cluster@cloudRuntime
    provider <- cluster@cloudProvider
    cloudConfig <- cluster@cloudConfig
    verbose <- cluster$verbose

    stopifnot(cloudConfig$workerNumber >= workerNumber)
    ## set the expected worker number
    cloudConfig$workerNumber <- cloudConfig$workerNumber - workerNumber

    ## get the number of workers that will be killed
    requiredKilledNumber <- getWorkerNumber(cluster) - cloudConfig$workerNumber
    if(requiredKilledNumber <= 0){
        return(invisible(NULL))
    }
    removeWorkersInternal(cluster, requiredKilledNumber)
}



stopCluster <- function(cluster){
    verbosePrint(cluster$verbose, "Stopping cluster")
    deregisterBackend(cluster)
    setWorkerNumber(cluster, 0)
    stopServer(cluster)
    invisible(NULL)
}

stopServer<- function(cluster){
    verbose <- cluster$verbose
    provider <- cluster@cloudProvider
    cloudRuntime <- cluster@cloudRuntime
    if(!is.null(cloudRuntime$serverHandle)){
        success <- killDockerInstances(provider,
                                instanceHandles = list(cloudRuntime$serverHandle),
                                verbose = verbose)
        if(success){
            resetRuntimeServer(cloudRuntime)
        }else{
            warning("Fail to stop the server")
        }
    }
    invisible(NULL)
}

reconnect <- function(cluster, ...){
    verbose <- cluster$verbose
    provider <- cluster@cloudProvider
    exist <- dockerClusterExists(provider=provider, cluster=cluster, verbose=verbose)
    if(!exist)
        stop("The cluster with the job queue name <",
             .getJobQueueName(cluster),
             "> is not running!")

    if(cluster$stopClusterOnExit){
        verbosePrint(verbose>0, "<stopClusterOnExit> will be set to FALSE")
        cluster$stopClusterOnExit <- FALSE
    }
    reconnectDockerCluster(provider = provider,
                           cluster = cluster,
                           verbose = verbose)
    cluster$registerBackend(...)
}


getWorkerNumber <- function(cluster){
    removeDiedWorkers(cluster)
    sum(cluster@cloudRuntime$workerPerHandle)
}

getExpectedWorkerNumber <- function(cluster){
    cluster@cloudConfig$workerNumber
}

registerBackend <- function(cluster, ...){
    verbose <- cluster$verbose
    verbosePrint(verbose, "Registering foreach redis backend, it might take a few minutes")
    if(!is.null(cluster@cloudRuntime$serverHandle)){
        success <- waitInstanceUntilRunning(
            cluster@cloudProvider,
            list(cluster@cloudRuntime$serverHandle)
        )
        if(!success){
            stop("The server is not running!")
        }
    }
    registerParallelBackend(container = cluster@workerContainer,
                            cluster = cluster,
                            verbose = cluster$verbose, ...)
    cluster@settings$parallelBackendRegistered <- TRUE
    invisible(NULL)
}


deregisterBackend <- function(cluster){
    if(cluster@settings$parallelBackendRegistered){
        deregisterParallelBackend(container = cluster@workerContainer,
                                  cluster = cluster,
                                  verbose = cluster$verbose)
        cluster@settings$parallelBackendRegistered <- FALSE
    }
    invisible(NULL)
}

isServerRunning <- function(cluster){
    ipExist <- !all(
        is.null(cluster@cloudRuntime$serverPrivateIp),
        is.null(cluster@cloudRuntime$serverPublicIp)
    )
    if(!ipExist){
        if(is.null(.getServerHandle(cluster))){
            FALSE
        }else{
            TRUE
        }
    }else{
        TRUE
    }
}

update <- function(cluster){
    removeDiedServer(cluster)
    removeDiedWorkers(cluster)
    cluster
}
