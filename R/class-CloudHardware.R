CloudHardware <- function(cpu = 256, memory = 512, id = NULL){
    .CloudHardware(cpu=cpu,memory=memory, id = id)
}


#' @export
setMethod(f = "show",signature = "CloudHardware",
          definition = function(object){
              cat("CloudHardware S4 object\n")
              cat("  CPU:", object@cpu, " unit\n")
              cat("  Memory:", object@memory, "MB\n")
              invisible(NULL)
          })
