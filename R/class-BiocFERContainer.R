BiocFERContainer <- function(image = "", name = NULL,  environment = list(),
                             maxWorkerNum = 4L,
                             RPackages = NULL,
                             sysPackages = NULL){
  .BiocFERContainer$new(
    name=name, image = image,
    environment = environment,
    maxWorkerNum = as.integer(maxWorkerNum),
    RPackages=RPackages,
    sysPackages=sysPackages)
}

#' Common BiocFERContainer parameter
#'
#' Common BiocFERContainer parameter
#'
#' @param image Character, the container image
#' @param name Character, the optional name of the container
#' @param environment List, the environment variables in the container
#' @rdname BiocFERContainer-commom-parameters
#' @name BiocFERContainer-commom-parameters
NULL



#' Get the Bioconductor Foreach Redis server container
#'
#' Get the Bioconductor Foreach Redis server container.
#'
#' @inheritParams BiocFERContainer-commom-parameters
#' @examples BiocFERServerContainer()
#' @return a `BiocFERContainer` object
#' @export
BiocFERServerContainer <- function(image = "dockerparallel/parallel-redis-server",
                                   name = "redisRServerContainer",
                                   environment = list()){
  BiocFERContainer(image = image, name=name,
                   environment=environment,
                   maxWorkerNum=1L)
}
#' Get the Bioconductor Foreach Redis worker container
#'
#' Get the Bioconductor Foreach Redis worker container.
#'
#' @inheritParams BiocFERContainer-commom-parameters
#' @param RPackages Character, a vector of R packages that will be installed
#' by `AnVIL::install` before connecting with the server
#' @param sysPackages Character, a vector of system packages that will be installed
#' by `apt-get install` before running the R worker
#' @param maxWorkerNum Integer, the maximum worker number in a container
#'
#' @examples BiocFERWorkerContainer()
#' @return a `BiocFERContainer` object
#' @export
BiocFERWorkerContainer <- function(image = "dockerparallel/parallel-redis-worker",
                                   name = "redisRWorkerContainer",
                                   RPackages = NULL,
                                   sysPackages = NULL,
                                   environment = list(),
                                   maxWorkerNum = 4L){
  BiocFERContainer(image = image, name=name, RPackages=RPackages, sysPackages=sysPackages,
                   environment=environment,
                   maxWorkerNum=maxWorkerNum)
}

.BiocFERContainer$methods(
  show = function(){
    cat("Bioconductor foreach redis container reference object\n")
    cat("  Image:     ", .self$image, "\n")
    cat("  maxWorkers:", .self$maxWorkerNum, "\n")
    if(!is.null(.self$RPackages)){
      cat("  R packages:", paste0(.self$RPackages, collapse = ", "), "\n")
    }
    if(!is.null(.self$sysPackages)){
      cat("  system packages:", paste0(.self$sysPackages, collapse = ", "), "\n")
    }
    cat("  Environment variables:\n")
    for(i in names(.self$environment)){
      cat("    ",i,": ", .self$environment[[i]], "\n",sep="")
    }
    invisible(NULL)
  }
)


