BiocContainerProvider <- function(image, name = NULL,  environment = list(), command = NULL, maxWorkerNum = 4L){
  .BiocContainerProvider(name=name, image = image,
             environment = environment,
             command = command,
             maxWorkerNum = as.integer(maxWorkerNum))
}


getBiocServerContainer <- function(image = "dockerparallel/parallel-redis-server",
                               name = "redisRServerContainer", ...){
  BiocContainerProvider(image = image, name=name, ...)
}
getBiocWorkerContainer <- function(image = "dockerparallel/parallel-redis-worker",
                               name = "redisRWorkerContainer", ...){
  BiocContainerProvider(image = image, name=name, ...)
}


setMethod("show", "BiocContainerProvider", function(object){
cat("S4 Bioc container object\n")
  cat("  Image:     ", object@image, "\n")
  cat("  Command:   ", object@command, "\n")
  cat("  maxWorkers:", object@maxWorkerNum, "\n")
  cat("  Environment variables:\n")
  for(i in names(object@environment)){
      cat("    ",i,": ", object@environment[[i]], "\n",sep="")
  }
  invisible(NULL)
})



