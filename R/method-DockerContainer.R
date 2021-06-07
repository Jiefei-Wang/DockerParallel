#' Show the docker container
#'
#' Show the docker container
#'
#' @param object The `DockerContainer` object
#' @return No return value
#' @export
setMethod("show", "DockerContainer", function(object){
    cat("Docker container reference object\n")
    cat("  Image:     ", object$image, "\n")
    cat("  Backend:   ", object$backend, "\n")
    cat("  MaxWorkers:", object$maxWorkerNum, "\n")
    cat("  Environment variables:\n")
    for(i in names(object$environment)){
        cat("    ",i,": ", object$environment[[i]], "\n",sep="")
    }
    invisible(NULL)
})

#' @describeIn DockerStaticData The method for DockerContainer
#' @export
setMethod("getDockerStaticData", "DockerContainer", getObjectData)

#' @describeIn DockerStaticData The method for DockerContainer
#' @export
setMethod("setDockerStaticData", "DockerContainer", setObjectData)
