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
    "status",
    "registerBackend",
    "deregisterBackend",
    "isServerRunning"
)

clusterOptions <- c(
    "verbose",
    "stopClusterOnExit"
)

dockerCluster <- function(cloudProvider = ECSProvider(),
                          cloudConfig = CloudConfig(),
                          cloudRuntime = CloudRuntime(),
                          verbose = 1,
                          stopClusterOnExit = TRUE){
    settings <- new.env(parent = emptyenv())
    settings$verbose <- verbose
    settings$stopClusterOnExit <- stopClusterOnExit
    settings$parallelBackendRegistered <- FALSE

    publicIpNULL <- is.null(cloudRuntime$serverPublicIp)
    privateIpNULL <- is.null(cloudRuntime$serverPrivateIp)
    if(!all(publicIpNULL,privateIpNULL)){
        cloudConfig$serverContainer = NULL
    }
    if(publicIpNULL&&!privateIpNULL){
        cloudConfig$serverClientSameNAT <- FALSE
        cloudConfig$serverWorkerSameNAT <- FALSE
    }
    if(!publicIpNULL&&privateIpNULL){
        cloudConfig$serverClientSameNAT <- TRUE
        cloudConfig$serverWorkerSameNAT <- TRUE
    }

    cluster <- .DockerCluster(cloudProvider = cloudProvider,
                              cloudConfig = cloudConfig,
                              cloudRuntime = cloudRuntime,
                              settings = settings
    )
    settings$cluster <- cluster
    reg.finalizer(settings, DockerCluster.finalizer, onexit = TRUE)
    cluster
}

#' @export
setMethod(f = "names",signature = "DockerCluster",
          definition = function(x){
              c(clusterMethods, clusterOptions)
          })
#' @export
setMethod(f = "$",signature = "DockerCluster",
          definition = function(x, name){
              if(name%in%clusterOptions){
                  return(x@settings[[name]])
              }
              if(name%in%clusterMethods){
                  func <- get(name)
                  newFunc <- createTempFunction(name, func)
              }
              environment(newFunc) <- environment()
              newFunc
          })

#' @export
setMethod(f = "$<-",signature = "DockerCluster",
          definition = function(x, name, value){
              if(name%in%clusterOptions){
                  x@settings[[name]] <- value
              }else{
                  stop("Unable to assign the parameter <", name,">")
              }
              x
          }
)



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
          })
