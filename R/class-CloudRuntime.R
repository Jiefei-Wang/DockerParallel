CloudRuntime <- function(
    serverFromOtherSource = TRUE,
    serverPublicIp = character(0),
    serverPublicPort = character(0),
    serverPrivateIp = character(0),
    serverPrivatePort = character(0),
    initializingWorkerNumber = 0L,
    runningWorkerNumber = 0L
){
    .CloudRuntime$new(
        serverFromOtherSource = serverFromOtherSource,
        serverPublicIp = serverPublicIp,
        serverPublicPort = as.integer(serverPublicPort),
        serverPrivateIp = serverPrivateIp,
        serverPrivatePort = as.integer(serverPrivatePort),
        initializingWorkerNumber = as.integer(initializingWorkerNumber),
        runningWorkerNumber = as.integer(runningWorkerNumber)
    )
}


#' Print the cloudRuntime
#'
#' Print the cloudRuntime
#'
#' @param object The cloudRuntime object
#' @return No return value
#' @export
setMethod(f = "show",signature = "CloudRuntime",
          definition = function(object){
              cat("A reference CloudRuntime object\n")
              if(object$serverFromOtherSource){
                  cat("The server is from other source\n")
              }else{
                  cat("The server is provided by the cloud provider\n")
              }
              cat("Server public Ip:    ", object$serverPublicIp, "\n")
              cat("Server public port:  ", object$serverPublicPort, "\n")
              cat("Server private Ip:   ", object$serverPrivateIp, "\n")
              cat("Server private port: ", object$serverPrivatePort, "\n")
              cat("Worker number:       ", object$runningWorkerNumber, "/",
                  object$initializingWorkerNumber, " (running/initializing)\n")
              invisible(NULL)
          })



