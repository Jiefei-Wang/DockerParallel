CloudConfig <- function(jobQueueName = "DockerParallelQueue",
                        workerNumber = 1L,
                        serverHardware = DockerHardware(),
                        workerHardware = DockerHardware(),
                        serverPort = 6379L,
                        serverPassword = generateRandomPassword(),
                        serverWorkerSameLAN = TRUE,
                        serverClientSameLAN = FALSE){
    .CloudConfig$new(jobQueueName = jobQueueName,
                     workerNumber = as.integer(workerNumber),
                     serverHardware = serverHardware,
                     workerHardware = workerHardware,
                     serverPort = as.integer(serverPort),
                     serverPassword=serverPassword,
                     serverWorkerSameLAN=serverWorkerSameLAN,
                     serverClientSameLAN=serverClientSameLAN)
}


.CloudConfig$methods(
    show = function(){
        serverPasswordTmp <- ifelse(is.null(.self$serverPassword), "FALSE", "TRUE")

        cat("Job queue name:  ", .self$jobQueueName, "\n")
        cat("Worker number:   ", .self$workerNumber, "\n")
        cat("Worker CPU:      ", .self$workerHardware@cpu, " unit\n")
        cat("Worker memory:   ", .self$workerHardware@memory, " MB\n")
        cat("server CPU:      ", .self$serverHardware@cpu, " unit\n")
        cat("server memory:   ", .self$serverHardware@memory, " MB\n")
        cat("Server port:     ", .self$serverPort, "\n")
        cat("Server password: ", serverPasswordTmp, "\n")
        invisible(NULL)
    }
)
