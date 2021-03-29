Container <- function(image, name = NULL,  environment = list(), command = NULL, maxWorkers = Inf){
  .Container(name=name, image = image, environment = environment, command = command, maxWorkers = maxWorkers)
}


getServerContainer <- function(image = "dockerparallel/parallel-redis-server", name = "redisRServerContainer", ...){
  Container(image = image, name=name, ...)
}
getWorkerContainer <- function(image = "dockerparallel/parallel-redis-worker", name = "redisRWorkerContainer", ...){
  Container(image = image, name=name, ...)
}


setMethod("show", "Container", function(object){
cat("S4 Container object\n")
  cat("  Image:  ", object@image, "\n")
  cat("  Command:", object@command, "\n")
  cat("  maxWorkers:", object@maxWorkers, "\n")
  cat("  Environment variables:\n")
  for(i in names(object@environment)){
      cat("    ",i,": ", object@environment[[i]], "\n",sep="")
  }
  invisible(NULL)
})



