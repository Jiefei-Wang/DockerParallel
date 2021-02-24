DedicatedServer <- function(IP, port = 6379, password = NULL){
    .DedicatedServer(IP=IP,
                     port=as.integer(port),
                     password=password
    )
}


#' @export
setMethod(f = "show",signature = "DedicatedServer",
          definition = function(object){
              cat("IP:      ", object@IP, "\n")
              cat("Port:    ", object@port, "\n")
              cat("Password:", ifelse(is.null(object@IP), "TRUE", "FALSE"), "\n")
              invisible(NULL)
          })
