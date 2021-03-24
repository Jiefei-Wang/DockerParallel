Container <- function(image, environment = list(), command = NULL, maxWorkers = Inf){
  .Container(image = image, environment = environment, command = command, maxWorkers = maxWorkers)
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



