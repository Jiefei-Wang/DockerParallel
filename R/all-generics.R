###############cloud provider###############
setGeneric("initialProvider", function(provider, cluster, verbose = FALSE, ...){
    standardGeneric("initialProvider")
})

#' Run the containers
#'
#' Run the containers and return a list of instance handles
#'
#' @return InstanceInfo object
setGeneric("runServerContainer", function(provider, container, hardware, verbose = FALSE, ...){
    standardGeneric("runContainers")
})


setGeneric("runWorkerContainers", function(provider, container, hardware, containerNumber, verbose = FALSE, ...){
    standardGeneric("runContainers")
})


#' Get the public Ips for the instances
#'
#' Get the public Ips for the instances
#'
#' @param instanceHandles A list of instance handles
#'
#' @return InstanceInfo object
# setGeneric("getInstanceIps", function(provider, instanceHandles, verbose = FALSE, ...){
#     standardGeneric("getInstanceIps")
# })

setGeneric("getClusterIp", function(provider, serverHandle, verbose = FALSE, ...){
    standardGeneric("getClusterIp")
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
#' `container` object
setGeneric("configServerContainer", function(container, cluster, verbose = FALSE, ...){
    standardGeneric("configServerContainer")
})

#' @returns
#' `container` object
setGeneric("configWorkerContainer", function(container, cluster, verbose = FALSE, ...){
    standardGeneric("configWorkerContainer")
})
