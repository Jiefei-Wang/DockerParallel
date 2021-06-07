clusterMethods <- c(
    "startCluster",
    "stopCluster",
    "startServer",
    "stopServer",
    "isServerRunning",
    "setWorkerNumber",
    "getWorkerNumbers",
    "registerBackend",
    "deregisterBackend",
    "update",
    "clusterExists",
    "reconnect",
    "cleanup"
)

clusterObjects <- c("cloudProvider", "serverContainer", "workerContainer")

clusterOptions <- c(
    "verbose",
    "stopClusterOnExit"
)


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
#' @returns
#' No return value
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
#' @return The object in the cluster
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
#' @return The `DockerCluster` object
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
#' @return No return value
#'
#' @export
setMethod(f = "show",signature = "DockerCluster",
          definition = function(object){
              verbose <- object$verbose
              ## Temporary disable the verbose message
              if(verbose < 2){
                  object$verbose <- 0L
                  on.exit(object$verbose <- verbose)
              }
              isServerRunning <- object$isServerRunning()

              cat("Server status:     ", ifelse(isServerRunning ,"Running", "Stopped"), "\n")
              if(isServerRunning){
                  cat("Server public IP:  ", .getServerPublicIp(object), "\n")
                  cat("Server private IP: ", .getServerPrivateIp(object), "\n")
              }
              workerNumbers <- object$getWorkerNumbers()

              cat("Worker Number:     ",
                  workerNumbers$expected, "/",
                  workerNumbers$running,"/",
                  workerNumbers$initializing,
                  " (expected/running/initializing)\n")
              invisible(NULL)
          })


#' @describeIn DockerStaticData The method for DockerCluster
#' @export
setMethod("getDockerStaticData", "DockerCluster", function(x){
    cloudConfig <- .getCloudConfig(x)
    serverContainer <- .getServerContainer(x)
    workerContainer <- .getWorkerContainer(x)
    settings <- .getClusterSettings(x)

    staticData <- list(
        cloudConfigClass = class(cloudConfig),
        serverContainerClass = class(serverContainer),
        workerContainerClass = class(workerContainer),
        cloudConfig = getDockerStaticData(cloudConfig),
        serverContainer = getDockerStaticData(serverContainer),
        workerContainer = getDockerStaticData(workerContainer),
        settings = as.list(settings))
    staticData$settings$parallelBackendRegistered <- NULL
    staticData$settings$cluster <- NULL
    staticData
})

#' @describeIn DockerStaticData The method for DockerCluster
#' @export
setMethod("setDockerStaticData", "DockerCluster", function(x, staticData){
    cloudConfig <- .getCloudConfig(x)
    serverContainer <- .getServerContainer(x)
    workerContainer <- .getWorkerContainer(x)

    if(!identical(staticData$cloudConfigClass, class(cloudConfig))){
        stop("The current <cloudConfig> is different from the <cloudConfig> on the cloud")
    }
    if(!identical(staticData$serverContainerClass, class(serverContainer))){
        stop("The current <serverContainer> is different from the <serverContainer> on the cloud")
    }
    if(!identical(staticData$workerContainerClass, class(workerContainer))){
        stop("The current <workerContainer> is different from the <workerContainer> on the cloud")
    }
    setDockerStaticData(cloudConfig,
                        staticData$cloudConfig)
    setDockerStaticData(serverContainer,
                        staticData$serverContainer)
    setDockerStaticData(workerContainer,
                        staticData$workerContainer)

    staticData$settings$parallelBackendRegistered <- FALSE
    staticData$settings$cluster <- x
    .setClusterSettings(x, as.environment(staticData$settings))
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
# cluster$getWorkerNumbers()
# cluster$status()

startCluster <- function(cluster, ...){
    initializeCloudProviderInternal(cluster)
    verbose <- cluster$verbose

    ## Check if the cluster exists on the cloud
    ## and ask user what to do
    continue <- checkIfClusterExistAndAsk(cluster)
    if(!continue)
        return(invisible(NULL))

    ## Start the server
    startServer(cluster)

    ## Start the worker
    expectedWorkerNumber <- .getExpectedWorkerNumber(cluster)
    verbosePrint(verbose>0, "The cluster has ",expectedWorkerNumber," workers")
    cluster$setWorkerNumber(expectedWorkerNumber)

    ## Register backend
    registerBackend(cluster, ...)
    invisible(NULL)
}

# Run the server
#
# Run the server and set the cluster IP
startServer <- function(cluster){
    initializeCloudProviderInternal(cluster)

    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)
    serverFromOtherSource <- .getServerFromOtherSource(cluster)

    if(!serverFromOtherSource){
        ## update the server status if the server is running
        if(cluster$isServerRunning()){
            updateServerStatus(cluster)
        }
        ## Only run a new server if there is no server running
        if(!cluster$isServerRunning()){
            if(is.null(.getServerContainer(cluster))){
                stop("No server container can be found")
            }
            serverContainer <-
                configServerContainerEnv(
                    container = .getServerContainer(cluster),
                    cluster = cluster,
                    verbose = verbose
                )
            serverHardware <- .getServerHardware(cluster)
            runDockerServer(provider = provider,
                            cluster = cluster,
                            container = serverContainer,
                            hardware = serverHardware,
                            verbose = verbose)
            updateServerIp(cluster)
        }
    }
    invisible(NULL)
}

setWorkerNumber <- function(cluster, workerNumber){
    workerNumber <- as.integer(workerNumber)
    stopifnot(length(workerNumber)==1 && workerNumber>=0)
    .setExpectedWorkerNumber(cluster, workerNumber)

    ## This only can be executed when the server is running
    if(cluster$isServerRunning()){
        initializeCloudProviderInternal(cluster)
        verbose <- cluster$verbose
        provider <- cluster@cloudProvider

        ## We set the worker number to 1 for each container,
        ## but the provider can ignore this setting and call
        ## `configWorkerContainerEnv` again to reconfig the container
        workerContainer <- .getWorkerContainer(cluster)
        workerContainer <- configWorkerContainerEnv(container = workerContainer,
                                                    cluster = cluster,
                                                    workerNumber = 1L,
                                                    verbose = verbose)
        workerHardware <- .getWorkerHardware(cluster)
        setDockerWorkerNumber(provider = provider,
                              cluster = cluster,
                              container = workerContainer,
                              hardware = workerHardware,
                              workerNumber = workerNumber,
                              verbose = verbose)

        updateWorkerNumber(cluster)
    }

    invisible(NULL)
}

getWorkerNumbers <- function(cluster){
    initializeCloudProviderInternal(cluster)

    ## Check the latest status
    updateWorkerNumber(cluster)

    initializing <- .getInitializingWorkerNumber(cluster)
    running <- .getRunningWorkerNumber(cluster)
    expected <- .getExpectedWorkerNumber(cluster)

    list(initializing = initializing,
         running = running,
         expected = expected)
}

stopCluster <- function(cluster, ignoreError = FALSE){
    verbose <- cluster$verbose
    verbosePrint(verbose, "Stopping cluster")
    provider <- .getCloudProvider(cluster)
    settings <- .getClusterSettings(cluster)
    handleError(deregisterBackend(cluster), errorToWarning = ignoreError)
    ## We use this trick to preserve the expected worker number
    expectedWorkerNumber <- .getExpectedWorkerNumber(cluster)
    handleError(setWorkerNumber(cluster, 0), errorToWarning = ignoreError)
    .setExpectedWorkerNumber(cluster, expectedWorkerNumber)

    handleError(stopServer(cluster), errorToWarning = ignoreError)
    if(settings$cloudProviderInitialized){
        handleError(
            cleanupDockerCluster(provider = provider,
                                 cluster = cluster,
                                 verbose = verbose),
            errorToWarning = ignoreError)
    }
    invisible(NULL)
}


stopServer<- function(cluster){
    ## Initialize provider
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)
    cloudRuntime <- .getCloudRuntime(cluster)

    serverFromOtherSource <- .getServerFromOtherSource(cluster)
    if(!serverFromOtherSource){
        if(cluster$isServerRunning()){
            stopDockerServer(provider = provider,
                             cluster = cluster,
                             verbose = verbose)
            resetServerRuntime(cloudRuntime)
        }
    }else{
        verbosePrint(
            verbose>=1,
            "The server is not managed by the cloud provider and will not be stopped")
    }
    invisible(NULL)
}

clusterExists <- function(cluster){
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)
    cluster$isServerRunning()||
        dockerClusterExists(provider, cluster, verbose)
}


reconnect <- function(cluster, ...){
    initializeCloudProviderInternal(cluster)
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)
    exist <- dockerClusterExists(provider=provider, cluster=cluster, verbose=verbose)
    if(!exist)
        stop("The cluster with the job queue name <",
             .getJobQueueName(cluster),
             "> is not running!")

    reconnectClusterInternal(cluster = cluster, ...)
}


registerBackend <- function(cluster, ...){
    if(!cluster$isServerRunning()){
        stop("Cannot register the parallel backend: The server is not running!")
    }

    verbose <- cluster$verbose
    verbosePrint(verbose, "Registering parallel backend, it might take a few minutes")
    settings <- .getClusterSettings(cluster)
    serverFromOtherSource <- .getServerFromOtherSource(cluster)

    if(!serverFromOtherSource){
        status <- waitServerRunning(cluster)
        if(!status){
            resetServerRuntime(.getCloudRuntime(cluster))
            stop("The server has been stopped, something is wrong")
        }
    }

    registerParallelBackend(container = .getWorkerContainer(cluster),
                            cluster = cluster,
                            verbose = cluster$verbose, ...)
    settings$parallelBackendRegistered <- TRUE
    invisible(NULL)
}


deregisterBackend <- function(cluster){
    settings <- .getClusterSettings(cluster)
    if(settings$parallelBackendRegistered){
        deregisterParallelBackend(container = .getWorkerContainer(cluster),
                                  cluster = cluster,
                                  verbose = cluster$verbose)
        settings$parallelBackendRegistered <- FALSE
    }
    invisible(NULL)
}

isServerRunning <- function(cluster){
    privateExists <- length(.getServerPrivateIp(cluster))!=0 &&
        length(.getServerPrivatePort(cluster)) != 0

    publicExists <- length(.getServerPublicIp(cluster))!=0 &&
        length(.getServerPublicPort(cluster)) != 0

    privateExists||publicExists||.getServerFromOtherSource(cluster)
}

update <- function(cluster){
    status <- updateServerStatus(cluster)
    cluster$setWorkerNumber(.getExpectedWorkerNumber(cluster))
    cluster
}

cleanup <- function(cluster, deep = FALSE){
    provider <- .getCloudProvider(cluster)
    verbose <- .getVerbose(cluster)
    if(cluster$isServerRunning()){
        stop("The server is still running")
    }
    workerNumbers <- cluster$getWorkerNumbers()
    if(workerNumbers$initializing + workerNumbers$running != 0){
        stop("The workers are still running")
    }
    cleanupDockerCluster(provider = provider,
                         cluster = cluster,
                         deep = deep,
                         verbose = verbose)
}
