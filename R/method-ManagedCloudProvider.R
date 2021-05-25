#' @describeIn setDockerWorkerNumber The method for the `ManagedCloudProvider`
#' @export
setMethod("setDockerWorkerNumber", "ManagedCloudProvider",
          function(provider, cluster, container, hardware, workerNumber, verbose)
          {
              verbosePrint(verbose, "Setting worker number to ", workerNumber)
              if(cluster$isServerRunning()){
                  ## set the expected worker number
                  removeDiedWorkers(cluster)

                  workerOffset <- workerNumber - getManagedWorkerNumber(provider)
                  if(workerOffset > 0){
                      addManagedWorkersInternal(cluster = cluster,
                                                container = container,
                                                hardware = hardware,
                                                workerNumber = workerOffset)
                  }
                  if(workerOffset < 0){
                      removeManagedWorkersInternal(cluster = cluster,
                                                   workerNumber = abs(workerOffset))
                  }
              }
          })

#' @describeIn getDockerWorkerNumbers The method for the `ManagedCloudProvider`
#' @export
setMethod("getDockerWorkerNumbers", "ManagedCloudProvider", function(provider, cluster, verbose){
    if(cluster$isServerRunning()){
        workerStatus <- removeDiedWorkers(cluster)
        list(initializing = sum(workerStatus=="initializing"), running = sum(workerStatus=="running"))
    }else{
        list(initializing = 0L, running = 0L)
    }
})


#' Add or get the worker container handles to the managed cloud provider
#'
#' Add or get the worker container handles to the managed cloud provider. The
#' handles can be duplicated if multiple workers share the same container
#'
#' @param provider A `ManagedCloudProvider` object
#' @param handles the worker container handles
#' @rdname managedHandles
#' @returns
#' addManagedWorkerHandles: No return value
#' getManagedWorkerHandles: A character vector of the worker handles
#' @export
addManagedWorkerHandles <- function(provider, handles){
    allHandles <- c(getManagedWorkerHandles(provider), handles)
    info <- table(allHandles)
    provider$workerHandles <- names(info)
    provider$workerPerHandle <- as.vector(info)
    invisible(NULL)
}


#' @rdname managedHandles
#' @export
getManagedWorkerHandles <- function(provider){
    workerHandles <- provider$workerHandles
    workerPerHandle <- provider$workerPerHandle
    x <- lapply(seq_along(workerHandles),
                function(i)rep(workerHandles[i],workerPerHandle[i])
    )
    unlist(x)
}

