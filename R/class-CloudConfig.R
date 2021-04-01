CloudConfig <- function(jobQueueName = "DockerParallelQueue",
                        workerNumber = 1L,
                        serverHardware = CloudHardware(),
                        workerHardware = CloudHardware(),
                        serverPort = 6379L,
                        serverPassword = generateRandomPassword(),
                        serverWorkerSameNAT = TRUE,
                        serverClientSameNAT = FALSE){
    .CloudConfig$new(jobQueueName = jobQueueName,
                     workerNumber = as.integer(workerNumber),
                     serverHardware = serverHardware,
                     workerHardware = workerHardware,
                     serverPort = as.integer(serverPort),
                     serverPassword=serverPassword,
                     serverWorkerSameNAT=serverWorkerSameNAT,
                     serverClientSameNAT=serverClientSameNAT)
}



.CloudConfig$methods(
    show = function(){
        serverPasswordTmp <- ifelse(is.null(.self$serverPassword), "FALSE", "TRUE")

        cat("Job queue name:  ", .self$jobQueueName, "\n")
        cat("Worker number:   ", .self$workerNumber, "\n")
        if(!is.null(.self$serverContainer)){
            cat("server CPU:      ", .self$serverHardware@cpu, " unit\n")
            cat("server memory:   ", .self$serverHardware@memory, " MB\n")
            cat("Server port:     ", .self$serverPort, "\n")
            cat("Server password: ", serverPasswordTmp, "\n")
        }
        cat("Worker CPU:      ", .self$serverHardware@cpu, " unit\n")
        cat("Worker memory:   ", .self$serverHardware@memory, " MB\n")
        invisible(NULL)
    }
)
