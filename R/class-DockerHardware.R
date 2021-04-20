DockerHardware <- function(cpu = 256, memory = 512, id = NULL){
    .DockerHardware(cpu=cpu,memory=memory, id = id)
}


#' Print the docker hardware
#'
#' Print the docker hardware
#'
#' @param object The DockerHardware object
#'
#' @return NULL
#' @export
setMethod(f = "show",signature = "DockerHardware",
          definition = function(object){
              cat("DockerHardware S4 object\n")
              cat("  CPU:", object@cpu, " unit\n")
              cat("  Memory:", object@memory, "MB\n")
              invisible(NULL)
          })
