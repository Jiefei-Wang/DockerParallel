#' @export
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

#' @export
BiocFERServerContainer <- function(image = "dockerparallel/parallel-redis-server",
                                   name = "redisRServerContainer",
                                   environment = list()){
  BiocFERContainer(image = image, name=name,
                   environment=environment,
                   maxWorkerNum=1L)
}
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


