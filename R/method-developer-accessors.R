#' Accessor functions
#'
#' Accessor functions for the developer.
#'
#' @param cluster A `DockerCluster` object
#' @param value,handles The value you want to set/add/remove
#'
#' @section worker handles:
#' When multiple workers share the same container, the developer can call
#' `.addWorkerHandles` and pass a list of duplicated handles to inform the cluster the
#' sharing exists. To remove such container handle from the list, you need to call
#' `.removeWorkerHandles` with the duplicated handles.
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
.getWorkerNumber <- function(cluster){
    .getCloudConfig(cluster)$workerNumber
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
.setWorkerNumber <- function(cluster, value){
    config <- .getCloudConfig(cluster)
    config$workerNumber <- as.integer(value)
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
.getWorkerHandles <- function(cluster){
    runtime <- .getCloudRuntime(cluster)
    workerHandles <- runtime$workerHandles
    workerPerHandle <- runtime$workerPerHandle
    x <- lapply(seq_along(workerHandles),
                     function(i)rep(workerHandles[i],workerPerHandle[i])
    )
    result <- list()
    for(i in x){
        result <- c(result, i)
    }
    result
}

#' @rdname accessors
#' @export
.addWorkerHandles <- function(cluster, handles){
    runtime <- .getCloudRuntime(cluster)
    workerHandles <- runtime$workerHandles
    workerPerHandle <- runtime$workerPerHandle
    for(i in handles){
        idx <- which(vapply(workerHandles, function(x) identical(x, i), logical(1)))
        if(length(idx)!=0){
            workerPerHandle[idx] <- workerPerHandle[idx] + 1L
        }else{
            workerHandles <- c(workerHandles, i)
            workerPerHandle <- c(workerPerHandle, 1L)
        }
    }
    runtime$workerHandles <- workerHandles
    runtime$workerPerHandle <- workerPerHandle
    invisible(NULL)
}

#' @rdname accessors
#' @export
.removeWorkerHandles <- function(cluster, handles){
    runtime <- .getCloudRuntime(cluster)
    workerHandles <- runtime$workerHandles
    workerPerHandle <- runtime$workerPerHandle
    for(i in handles){
        idx <- which(vapply(workerHandles, function(x) identical(x, i), logical(1)))
        if(length(idx)!=0){
            workerPerHandle[idx] <- workerPerHandle[idx] - 1L
            if(workerPerHandle[idx]==0){
                workerHandles <- workerHandles[-idx]
                workerPerHandle <- workerPerHandle[-idx]
            }
        }
    }
    runtime$workerHandles <- workerHandles
    runtime$workerPerHandle <- workerPerHandle
    invisible(NULL)
}


#' @rdname accessors
#' @export
.getServerHandle <- function(cluster){
    .getCloudRuntime(cluster)$serverHandle
}
#' @rdname accessors
#' @export
.getServerPrivateIp <- function(cluster){
    .getCloudRuntime(cluster)$serverPrivateIp
}
#' @rdname accessors
#' @export
.getServerPublicIp <- function(cluster){
    .getCloudRuntime(cluster)$serverPublicIp
}

#' @rdname accessors
#' @export
.setServerHandle <- function(cluster, value){
    runtime <- .getCloudRuntime(cluster)
    runtime$serverHandle <- value
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

