## Change the formals of the function so
## it can implicitly use the variable `cluster`
createTempFunction <- function(func, cluster){
    funcEnv <- new.env(parent = environment(func))
    funcEnv$cluster <- cluster
    funcFormals <- formals(func)
    funcFormals[["cluster"]]<-NULL
    formals(func) <- funcFormals
    environment(func) <- funcEnv
    func
}


#' Get the exported object names
#'
#' Get the exported object names
#'
#' @param x `ClusterMethodGetter` object
#'
#' @return A vector of object names
#' @export
setMethod(f = "names",signature = "ClusterMethodGetter",
          definition = function(x){
              getExportedNames(x@object)
          })
#' Get the exported object by the name
#'
#' Get the exported object by the name
#'
#' @param x `ClusterMethodGetter` object
#' @param name Character name
#' @return the exported object
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
#' print method
#'
#' print method
#'
#' @param object `ClusterMethodGetter` object
#' @return No return value
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
