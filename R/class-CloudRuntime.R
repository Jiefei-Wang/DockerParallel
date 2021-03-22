CloudRuntime <- function(serverPublicIp = NULL,
                         serverPrivateIp = NULL,
                         serverPort = 6379L,
                         serverPassword = NULL,
                         workerPublicIps = NULL,
                         workerPrivateIps = NULL){
    .CloudRuntime$new(serverPublicIp=serverPublicIp,
                      serverPrivateIp=serverPrivateIp,
                      serverPort=as.integer(serverPort),
                      serverPassword=serverPassword,
                      workerPublicIps=workerPublicIps,
                      workerPrivateIps=workerPrivateIps)
}


.CloudRuntime$methods(
    show = function(){
        serverPublicIpTmp <- ifelse(is.null(.self$serverPublicIp), "NULL", .self$serverPublicIp)
        serverPrivateIpTmp <- ifelse(is.null(.self$serverPrivateIp), "NULL", .self$serverPrivateIp)
        serverPasswordTmp <- ifelse(is.null(.self$serverPassword), "TRUE", "FALSE")
        workerNumber <- length(.self$workerHandles)

        cat("Server public IP:  ", serverPublicIpTmp, "\n")
        cat("Server private Ip: ", serverPrivateIpTmp, "\n")
        cat("Server port:       ", .self$serverPort, "\n")
        cat("Server password:   ", serverPasswordTmp, "\n")
        cat("Worker number:     ", workerNumber, "\n")
        invisible(NULL)
    }
)

