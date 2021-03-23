###############cloud provider###############
setGeneric("initialProvider", function(provider, verbose = FALSE, ...){
    standardGeneric("initialProvider")
})
setGeneric("initialCluster", function(provider, cluster, verbose = FALSE, ...){
    standardGeneric("initialCluster")
})

#'
setGeneric("runContainers", function(provider, container, hardware, containerNumber, verbose = FALSE, ...){
    standardGeneric("runContainers")
})


#' @returns
#' A data.frame with the columns `publicIp` and `privateIp`
setGeneric("getInstanceIps", function(provider, instanceHandles, publicIp = TRUE,
                                      privateIp = TRUE, verbose = FALSE, ...){
    standardGeneric("getInstanceIps")
})
#' @returns
#' A logical vector
setGeneric("instanceAlive", function(provider, instanceHandles, verbose = FALSE, ...){
    standardGeneric("instanceAlive")
})
setGeneric("killInstances", function(provider, instanceHandles, verbose = FALSE, ...){
    standardGeneric("killInstances")
})

###############container###############
#' @returns
#' A list object with element `container` and `hardware`
setGeneric("createServerConfig", function(container, cluster, verbose = FALSE, ...){
    standardGeneric("createServerConfig")
})
#' @returns
#' A list object with element `container` and `hardware`
setGeneric("createWorkerConfig", function(container, cluster, workerNumber, verbose = FALSE, ...){
    standardGeneric("createWorkerConfig")
})
