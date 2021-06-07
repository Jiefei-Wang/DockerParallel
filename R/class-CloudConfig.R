CloudConfig <- function(jobQueueName = "DockerParallelQueue",
                        expectedWorkerNumber = 1L,
                        serverHardware = DockerHardware(),
                        workerHardware = DockerHardware(),
                        serverPort = 8888L,
                        serverPassword = generateRandomPassword(),
                        serverWorkerSameLAN = TRUE,
                        serverClientSameLAN = FALSE){
    .CloudConfig$new(jobQueueName = jobQueueName,
                     expectedWorkerNumber = as.integer(expectedWorkerNumber),
                     serverHardware = serverHardware,
                     workerHardware = workerHardware,
                     serverPort = as.integer(serverPort),
                     serverPassword=serverPassword,
                     serverWorkerSameLAN=serverWorkerSameLAN,
                     serverClientSameLAN=serverClientSameLAN)
}

#' Print the CloudConfig
#'
#' Print the CloudConfig
#'
#' @param object The CloudConfig object
#' @return No return value
#' @export
setMethod(f = "show",signature = "CloudConfig",
          definition = function(object){
              serverPasswordTmp <- ifelse(is.null(object$serverPassword), "FALSE", "TRUE")
              cat("A reference CloudConfig object\n")
              cat("Job queue name:  ", object$jobQueueName, "\n")
              cat("Worker number:   ", object$expectedWorkerNumber, "\n")
              cat("Worker CPU:      ", object$workerHardware@cpu, " unit\n")
              cat("Worker memory:   ", object$workerHardware@memory, " MB\n")
              cat("server CPU:      ", object$serverHardware@cpu, " unit\n")
              cat("server memory:   ", object$serverHardware@memory, " MB\n")
              cat("Server port:     ", object$serverPort, "\n")
              cat("Server password: ", serverPasswordTmp, "\n")
              invisible(NULL)
          }
)

#' @describeIn DockerStaticData The method for CloudConfig
#' @export
setMethod("getDockerStaticData", "CloudConfig", getObjectData)

#' @describeIn DockerStaticData The method for CloudConfig
#' @export
setMethod("setDockerStaticData", "CloudConfig", setObjectData)
