#' @export
BiocFERContainer <- function(image, name = NULL,  environment = list(), command = NULL, maxWorkerNum = 4L){
  .BiocFERContainer$new(
    name=name, image = image,
    environment = environment,
    command = command,
    maxWorkerNum = as.integer(maxWorkerNum))
}

#' @export
getBiocFERServerContainer <- function(image = "dockerparallel/parallel-redis-server",
                                   name = "redisRServerContainer", ...){
  BiocFERContainer(image = image, name=name, ...)
}
#' @export
getBiocFERWorkerContainer <- function(image = "dockerparallel/parallel-redis-worker",
                                   name = "redisRWorkerContainer", ...){
  BiocFERContainer(image = image, name=name, ...)
}

.BiocFERContainer$methods(
  show = function(){
    cat("Bioconductor foreach redis container reference object\n")
    cat("  Image:     ", .self$image, "\n")
    if(!is.null(.self$command)){
      cat("  Command:   ", .self$command, "\n")
    }
    cat("  maxWorkers:", .self$maxWorkerNum, "\n")
    cat("  Environment variables:\n")
    for(i in names(.self$environment)){
      cat("    ",i,": ", .self$environment[[i]], "\n",sep="")
    }
    invisible(NULL)
  }
)


