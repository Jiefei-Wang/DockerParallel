Container <- function(image, environment = list(), command = NULL){
  .Container(image = image, environment = environment, command = command)
}


setMethod("show", "Container", function(object){
cat("S4 Container object\n")
  cat("  Image:  ", object@image, "\n")
  cat("  Command:", object@command, "\n")
  cat("  Environment variables:")
  for(i in names(object@environment)){
      cat("    ",i,": ", object@environment[[i]], "\n",sep="")
  }
  invisible(NULL)
})



