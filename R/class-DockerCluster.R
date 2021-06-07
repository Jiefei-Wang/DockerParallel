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
    settings$cloudProviderInitialized <- FALSE

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
#' @param serverHardwareId,workerHardwareId Character, the ID of the hardware, this argument
#' might be ignored by some cloud providers.
#' @param jobQueueName Character, the job queue name used by the cluster to send the job.
#' @param privateServerData A data object made from `CloudPrivateServer`. If this object
#' is provided, the cluster server should be from another source and the cloud provider
#' will not deploy the server container.
#' @param serverContainer A `DockerContainer` object, the object that defines the server container.
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
makeDockerCluster <- function(cloudProvider = NULL,
                              workerContainer = NULL,
                              workerNumber = 1,
                              workerCpu = 1024, workerMemory = 2048, workerHardwareId = character(0),
                              serverCpu = 256, serverMemory = 2048, serverHardwareId = character(0),
                              jobQueueName = "DockerParallelQueue",
                              privateServerData = NULL,
                              serverContainer = getServerContainer(workerContainer),
                              stopClusterOnExit = TRUE,
                              verbose = 1){
    if(is.null(cloudProvider)){
        if(is.null(packageSetting$cloudProvider)){
            stop("No default cloudProvider can be found")
        }
        cloudProvider <- packageSetting$cloudProvider$copy()
    }else{
        cloudProvider <- cloudProvider$copy()
    }
    if(is.null(workerContainer)){
        if(is.null(packageSetting$workerContainer)){
            stop("No default workerContainer can be found")
        }
        workerContainer <- packageSetting$workerContainer$copy()
    }else{
        workerContainer <- workerContainer$copy()
    }
    serverContainer <- serverContainer$copy()
    serverHardware <- DockerHardware(cpu = serverCpu,
                                     memory = serverMemory,
                                     id = serverHardwareId)
    workerHardware <- DockerHardware(cpu = workerCpu,
                                     memory = workerMemory,
                                     id = workerHardwareId)
    cloudConfig <- CloudConfig(jobQueueName=jobQueueName,
                               expectedWorkerNumber = workerNumber,
                               serverHardware =  serverHardware,
                               workerHardware = workerHardware)
    cloudRuntime <- CloudRuntime(serverFromOtherSource = FALSE)

    if(!is.null(privateServerData)){
        cloudConfig$serverWorkerSameLAN <- privateServerData$serverWorkerSameLAN
        cloudConfig$serverClientSameLAN <- privateServerData$serverClientSameLAN
        cloudRuntime$serverFromOtherSource <- TRUE
        cloudRuntime$serverPublicIp <- privateServerData$publicIp
        cloudRuntime$serverPublicPort <- as.integer(privateServerData$publicPort)
        cloudRuntime$serverPrivateIp <- privateServerData$privateIp
        cloudRuntime$serverPrivatePort <- as.integer(privateServerData$privatePort)
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
