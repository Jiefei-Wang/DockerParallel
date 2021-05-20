#' Accessor functions
#'
#' Accessor functions for the developer.
#'
#' @param cluster A `DockerCluster` object
#' @param value,handles The value you want to set/add/remove
#'
#'
#' @returns
#' No return value for the setter. The getter will get the object from the cluster.
#'
#' @rdname accessors
#' @export
.getCloudProvider <- function(cluster){
    cluster@cloudProvider
}

#' @rdname accessors
#' @export
.getCloudConfig <- function(cluster){
    cluster@cloudConfig
}

#' @rdname accessors
#' @export
.getServerContainer <- function(cluster){
    cluster@serverContainer
}

#' @rdname accessors
#' @export
.getWorkerContainer <- function(cluster){
    cluster@workerContainer
}

#' @rdname accessors
#' @export
.getCloudRuntime <- function(cluster){
    cluster@cloudRuntime
}


## CloudConfig
#' @rdname accessors
#' @export
.getJobQueueName <- function(cluster){
    .getCloudConfig(cluster)$jobQueueName
}
#' @rdname accessors
#' @export
.getExpectedWorkerNumber <- function(cluster){
    .getCloudConfig(cluster)$expectedWorkerNumber
}
#' @rdname accessors
#' @export
.getWorkerHardware <- function(cluster){
    .getCloudConfig(cluster)$workerHardware
}
#' @rdname accessors
#' @export
.getServerHardware <- function(cluster){
    .getCloudConfig(cluster)$serverHardware
}
#' @rdname accessors
#' @export
.getServerWorkerSameLAN <- function(cluster){
    .getCloudConfig(cluster)$serverWorkerSameLAN
}
#' @rdname accessors
#' @export
.getServerClientSameLAN <- function(cluster){
    .getCloudConfig(cluster)$serverClientSameLAN
}
#' @rdname accessors
#' @export
.getServerPassword <- function(cluster){
    .getCloudConfig(cluster)$serverPassword
}
#' @rdname accessors
#' @export
.getServerPort <- function(cluster){
    .getCloudConfig(cluster)$serverPort
}

#' @rdname accessors
#' @export
.setJobQueueName <- function(cluster, value){
    config <- .getCloudConfig(cluster)
    config$jobQueueName <- value
}
#' @rdname accessors
#' @export
.setExpectedWorkerNumber <- function(cluster, value){
    config <- .getCloudConfig(cluster)
    config$expectedWorkerNumber <- as.integer(value)
}
#' @rdname accessors
#' @export
.setWorkerHardware <- function(cluster, value){
    config <- .getCloudConfig(cluster)
    config$workerHardware <- value
}
#' @rdname accessors
#' @export
.setServerHardware <- function(cluster, value){
    config <- .getCloudConfig(cluster)
    config$serverHardware <- value
}
#' @rdname accessors
#' @export
.setServerWorkerSameLAN <- function(cluster, value){
    config <- .getCloudConfig(cluster)
    config$serverWorkerSameLAN <- as.logical(value)
}
#' @rdname accessors
#' @export
.setServerClientSameLAN <- function(cluster, value){
    config <- .getCloudConfig(cluster)
    config$serverClientSameLAN <- as.logical(value)
}
#' @rdname accessors
#' @export
.setServerPassword <- function(cluster, value){
    config <- .getCloudConfig(cluster)
    config$serverPassword <- value
}
#' @rdname accessors
#' @export
.setServerPort <- function(cluster, value){
    config <- .getCloudConfig(cluster)
    config$serverPort <- as.integer(value)
}


## CloudRuntime
#' @rdname accessors
#' @export
.getServerFromOtherSource <- function(cluster){
    .getCloudRuntime(cluster)$serverFromOtherSource
}

#' @rdname accessors
#' @export
.getServerPrivateIp <- function(cluster){
    .getCloudRuntime(cluster)$serverPrivateIp
}

#' @rdname accessors
#' @export
.getServerPrivatePort <- function(cluster){
    .getCloudRuntime(cluster)$serverPrivatePort
}

#' @rdname accessors
#' @export
.getServerPublicIp <- function(cluster){
    .getCloudRuntime(cluster)$serverPublicIp
}

#' @rdname accessors
#' @export
.getServerPublicPort <- function(cluster){
    .getCloudRuntime(cluster)$serverPublicPort
}

#' @rdname accessors
#' @export
.getInitializingWorkerNumber <- function(cluster){
    .getCloudRuntime(cluster)$initializingWorkerNumber
}

#' @rdname accessors
#' @export
.getRunningWorkerNumber <- function(cluster){
    .getCloudRuntime(cluster)$runningWorkerNumber
}

#' @rdname accessors
#' @export
.setServerPrivateIp <- function(cluster, value){
    runtime <- .getCloudRuntime(cluster)
    runtime$serverPrivateIp <- value
}
#' @rdname accessors
#' @export
.setServerPublicIp <- function(cluster, value){
    runtime <- .getCloudRuntime(cluster)
    runtime$serverPublicIp <- value
}
#' @rdname accessors
#' @export
.setServerPrivatePort <- function(cluster, value){
    runtime <- .getCloudRuntime(cluster)
    runtime$serverPrivatePort <- as.integer(value)
}
#' @rdname accessors
#' @export
.setServerPublicPort <- function(cluster, value){
    runtime <- .getCloudRuntime(cluster)
    runtime$serverPublicPort <- as.integer(value)
}
#' @rdname accessors
#' @export
.setInitializingWorkerNumber <- function(cluster, value){
    runtime <- .getCloudRuntime(cluster)
    runtime$InitializingWorkerNumber <- as.integer(value)
}
#' @rdname accessors
#' @export
.setRunningWorkerNumber <- function(cluster, value){
    runtime <- .getCloudRuntime(cluster)
    runtime$runningWorkerNumber <- as.integer(value)
}

#' @rdname accessors
#' @export
.setServerFromOtherSource <- function(cluster, value){
    runtime <- .getCloudRuntime(cluster)
    runtime$serverFromOtherSource <- value
}