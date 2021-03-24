CloudRuntime <- function(
    serverHandle = NULL,
    workerHandles = list(),
    workerPerHandle = c(),
    clusterIp = NULL
    ){
    .CloudRuntime$new(serverHandle = serverHandle,
                      workerHandles = workerHandles,
                      workerPerHandle = as.integer(workerPerHandle),
                      clusterIp=clusterIp)
}


.CloudRuntime$methods(
    show = function(){
        clusterIpTmp <- ifelse(is.null(.self$clusterIp), "NULL", .self$clusterIp)
        workerNumber <- sum(.self$workerPerHandle)

        cat("Cluster Ip:    ", clusterIpTmp, "\n")
        cat("Worker number: ", workerNumber, "\n")
        invisible(NULL)
    }
)

