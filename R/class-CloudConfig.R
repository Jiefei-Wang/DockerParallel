CloudConfig <- function(clusterName = "dockerCluster",
                        workerNum = 1L,
                        serverContainer = Container("dockerparallel/parallel-redis-server"),
                        workerContainer = Container("dockerparallel/parallel-redis-worker"),
                        serverHardware = CloudHardware(),
                        workerHardware = CloudHardware()){
    .CloudConfig$new(clusterName = clusterName,
                     workerNum = as.integer(workerNum),
                     serverContainer = serverContainer,
                     workerContainer = workerContainer,
                     serverHardware = serverHardware,
                     workerHardware = workerHardware)
}



.CloudConfig$methods(
    show = function(){
        cat("Cluster name:    ", .self$clusterName, "\n")
        cat("Worker number:   ", .self$workerNum, "\n")
        if(!is.null(.self$serverContainer)){
            cat("Server container:", .self$serverContainer@image, "\n")
            cat("server CPU:      ", .self$serverHardware@cpu, " unit\n")
            cat("server memory:   ", .self$serverHardware@memory, " MB\n")
        }
        cat("Worker container:", .self$workerContainer@image, "\n")
        cat("Worker CPU:      ", .self$serverHardware@cpu, " unit\n")
        cat("Worker memory:   ", .self$serverHardware@memory, " MB\n")
        invisible(NULL)
    }
)