#' @export
setMethod(f = "names",signature = "ClusterMethodGetter",
          definition = function(x){
              getExportedNames(x@object)
          })
#' @export
setMethod(f = "$",signature = "ClusterMethodGetter",
          definition = function(x, name){
              stopifnot(name %in% names(x))
              object <- getExportedObject(x@object, name)
              ## Change the formals of the function so
              ## it can implicitly use the variable `cluster`
              if(is.function(object)){
                  object <- createTempFunction(object, x@cluster)
              }
              object
          }
)
#' @export
setMethod(f = "show",signature = "ClusterMethodGetter",
          definition = function(object){
              show(object@object)
              cat("The available methods/variables:\n",
                  paste0(getExportedNames(object@object), sep = ",")
                  )
              invisible(NULL)
          }
)
