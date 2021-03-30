CloudRuntime <- function(
    serverHandle = NULL,
    workerHandles = list(),
    workerPerHandle = c(),
    serverPublicIp = NULL,
    serverPrivateIp = NULL
    ){
    .CloudRuntime$new(serverHandle = serverHandle,
                      workerHandles = workerHandles,
                      workerPerHandle = as.integer(workerPerHandle),
                      serverPublicIp=serverPublicIp,
                      serverPrivateIp=serverPrivateIp
                      )
}


.CloudRuntime$methods(
    show = function(){
        publicIp <- ifelse(is.null(.self$serverPublicIp), "NULL", .self$serverPublicIp)
        workerNumber <- sum(.self$workerPerHandle)

        cat("Server public Ip: ", publicIp, "\n")
        cat("Worker number:    ", workerNumber, "\n")
        invisible(NULL)
    }
)

