CloudConfig <- function(clusterName = "dockerCluster",
                        workerNumber = 1L,
                        serverContainer = getServerContainer(),
                        workerContainer = getWorkerContainer(),
                        serverHardware = CloudHardware(),
                        workerHardware = CloudHardware(),
                        serverPort = 6379L,
                        serverPassword = generateRandomPassword(),
                        serverWorkerSameNAT = TRUE,
                        serverClientSameNAT = FALSE){
    .CloudConfig$new(clusterName = clusterName,
                     workerNumber = as.integer(workerNumber),
                     serverContainer = serverContainer,
                     workerContainer = workerContainer,
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

        cat("Cluster name:    ", .self$clusterName, "\n")
        cat("Worker number:   ", .self$workerNumber, "\n")
        if(!is.null(.self$serverContainer)){
            cat("Server container:", .self$serverContainer@image, "\n")
            cat("server CPU:      ", .self$serverHardware@cpu, " unit\n")
            cat("server memory:   ", .self$serverHardware@memory, " MB\n")
            cat("Server port:     ", .self$serverPort, "\n")
            cat("Server password: ", serverPasswordTmp, "\n")
        }
        cat("Worker container:", .self$workerContainer@image, "\n")
        cat("Worker CPU:      ", .self$serverHardware@cpu, " unit\n")
        cat("Worker memory:   ", .self$serverHardware@memory, " MB\n")
        invisible(NULL)
    }
)
