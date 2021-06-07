#' Make a DockerHardware object
#'
#' Make a DockerHardware object
#' @param cpu Numeric(1), the CPU limitation for the docker. 1024 CPU unit
#' corresponds to 1 core.
#' @param memory Numeric(1), the memory limitation for the docker, the unit
#' is MB.
#' @param id character(1) or character(0), the id of the hardware, the meaning of
#' `id` depends on the cloud provider.
#' @examples
#' DockerHardware()
#' @return A DockerHardware object
#' @export
DockerHardware <- function(cpu = 256, memory = 512, id = character(0)){
    .DockerHardware(cpu=cpu,memory=memory, id = id)
}


#' Print the docker hardware
#'
#' Print the docker hardware
#'
#' @param object The DockerHardware object
#'
#' @examples
#' hardware <- DockerHardware()
#' show(hardware)
#' @return No return value
#' @export
setMethod(f = "show",signature = "DockerHardware",
          definition = function(object){
              cat("DockerHardware S4 object\n")
              cat("  CPU:", object@cpu, " unit\n")
              cat("  Memory:", object@memory, "MB\n")
              invisible(NULL)
          })
