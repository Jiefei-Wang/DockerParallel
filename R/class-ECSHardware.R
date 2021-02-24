ECSHardware <- function(type = "fargate", CPU = NULL, memory = NULL, id = NULL){
    .ECSHardware(type=type,CPU=CPU,memory=memory, id= id)
}


#' @export
setMethod(f = "$",signature = "ECSHardware",
          definition = function(x, name){
              if(name=="CPU")
                  return(x@CPU*1024)
              else
                  return(do.call("@", list(x,name)))
          })



#' @export
setMethod(f = "show",signature = "ECSHardware",
          definition = function(object){
              cat("ECSHardware S4 object\n")
              cat("  Type:", object@type, "\n")
              cat("  CPU:", object@CPU, "cores\n")
              cat("  Memory:", object@memory, "MB\n")
              invisible(NULL)
          })
