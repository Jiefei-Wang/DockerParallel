setGeneric("runContainers", function(platform, container, containerNumber){
    standardGeneric("runContainers")
})

setGeneric("getInstanceIps", function(platform, instanceHandles, publicIp = TRUE, privateIp = TRUE){
    standardGeneric("getInstanceIps")
})
