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
